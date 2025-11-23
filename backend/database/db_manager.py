"""
SQLite Database Manager for BRE System
Handles all database operations including CRUD, queries, and transactions
"""
import sqlite3
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime
import hashlib


class DatabaseManager:
    """Manages SQLite database operations for the BRE system"""

    def __init__(self, db_path: str = "backend/data/bre_analysis.db"):
        """Initialize database connection"""
        self.db_path = Path(db_path)
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self.connection = None
        self._initialize_database()

    def _initialize_database(self):
        """Initialize database with schema if it doesn't exist"""
        is_new_db = not self.db_path.exists()

        self.connection = sqlite3.connect(str(self.db_path), check_same_thread=False)
        self.connection.row_factory = sqlite3.Row  # Return rows as dictionaries

        if is_new_db:
            print(f"[INFO] Creating new database at {self.db_path}")
            self._execute_schema()
        # Removed noisy "Connected to existing database" logging for performance

    def _execute_schema(self):
        """Execute schema SQL file to create tables"""
        schema_file = Path(__file__).parent / "schema.sql"

        if not schema_file.exists():
            raise FileNotFoundError(f"Schema file not found: {schema_file}")

        with open(schema_file, 'r') as f:
            schema_sql = f.read()

        cursor = self.connection.cursor()
        cursor.executescript(schema_sql)
        self.connection.commit()
        print("[INFO] Database schema created successfully")

    def close(self):
        """Close database connection"""
        if self.connection:
            self.connection.close()

    def __enter__(self):
        """Context manager entry"""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.close()

    # ============================================
    # USER MANAGEMENT
    # ============================================

    def create_user(self, username: str, email: str, password_hash: str,
                   role_id: int, full_name: str = None) -> int:
        """Create a new user"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO users (username, email, password_hash, role_id, full_name)
            VALUES (?, ?, ?, ?, ?)
        """, (username, email, password_hash, role_id, full_name))
        self.connection.commit()
        return cursor.lastrowid

    def get_user_by_username(self, username: str) -> Optional[Dict]:
        """Get user by username"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT u.*, r.role_name, r.can_upload, r.can_analyze,
                   r.can_delete, r.can_export, r.can_manage_users
            FROM users u
            JOIN roles r ON u.role_id = r.id
            WHERE u.username = ?
        """, (username,))
        row = cursor.fetchone()
        return dict(row) if row else None

    def get_user_by_id(self, user_id: int) -> Optional[Dict]:
        """Get user by ID"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT u.*, r.role_name
            FROM users u
            JOIN roles r ON u.role_id = r.id
            WHERE u.id = ?
        """, (user_id,))
        row = cursor.fetchone()
        return dict(row) if row else None

    def update_last_login(self, user_id: int):
        """Update user's last login timestamp"""
        cursor = self.connection.cursor()
        cursor.execute("""
            UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE id = ?
        """, (user_id,))
        self.connection.commit()

    def log_user_activity(self, user_id: int, activity_type: str,
                         description: str = None, ip_address: str = None):
        """Log user activity"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO user_activity_log (user_id, activity_type, activity_description, ip_address)
            VALUES (?, ?, ?, ?)
        """, (user_id, activity_type, description, ip_address))
        self.connection.commit()

    # ============================================
    # PROJECT MANAGEMENT
    # ============================================

    def create_project(self, name: str, description: str = None,
                      created_by: int = None) -> int:
        """Create a new project"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO projects (name, description, created_by)
            VALUES (?, ?, ?)
        """, (name, description, created_by))
        self.connection.commit()
        return cursor.lastrowid

    def get_project_by_name(self, name: str) -> Optional[Dict]:
        """Get project by name"""
        cursor = self.connection.cursor()
        cursor.execute("SELECT * FROM projects WHERE name = ?", (name,))
        row = cursor.fetchone()
        return dict(row) if row else None

    def get_or_create_project(self, name: str, description: str = None,
                             created_by: int = None) -> int:
        """Get existing project or create new one"""
        project = self.get_project_by_name(name)
        if project:
            return project['id']
        return self.create_project(name, description, created_by)

    def get_all_projects(self) -> List[Dict]:
        """Get all projects"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT p.*, u.username as created_by_username,
                   COUNT(DISTINCT ar.id) as analysis_count
            FROM projects p
            LEFT JOIN users u ON p.created_by = u.id
            LEFT JOIN analysis_runs ar ON p.id = ar.project_id
            GROUP BY p.id
            ORDER BY p.updated_at DESC
        """)
        return [dict(row) for row in cursor.fetchall()]

    # ============================================
    # ANALYSIS RUN MANAGEMENT
    # ============================================

    def create_analysis_run(self, project_id: int, upload_id: str,
                          upload_filename: str = None, uploaded_by: int = None,
                          selected_extensions: List[str] = None) -> int:
        """Create a new analysis run with optional file extension filtering"""
        cursor = self.connection.cursor()

        # Convert selected_extensions list to JSON string
        extensions_json = None
        if selected_extensions:
            import json
            extensions_json = json.dumps(selected_extensions)

        cursor.execute("""
            INSERT INTO analysis_runs (project_id, upload_id, upload_filename, uploaded_by, selected_extensions)
            VALUES (?, ?, ?, ?, ?)
        """, (project_id, upload_id, upload_filename, uploaded_by, extensions_json))
        self.connection.commit()
        return cursor.lastrowid

    def update_analysis_run_status(self, analysis_run_id: int, status: str,
                                   error_message: str = None):
        """Update analysis run status"""
        cursor = self.connection.cursor()
        if status == 'completed':
            cursor.execute("""
                UPDATE analysis_runs
                SET status = ?, completed_at = CURRENT_TIMESTAMP, error_message = ?
                WHERE id = ?
            """, (status, error_message, analysis_run_id))
        else:
            cursor.execute("""
                UPDATE analysis_runs
                SET status = ?, error_message = ?
                WHERE id = ?
            """, (status, error_message, analysis_run_id))
        self.connection.commit()

    def update_analysis_run_metrics(self, analysis_run_id: int,
                                   total_files: int, total_loc: int,
                                   total_sloc: int, total_comments: int,
                                   total_blank: int, avg_complexity: float):
        """Update analysis run summary metrics"""
        cursor = self.connection.cursor()
        cursor.execute("""
            UPDATE analysis_runs
            SET total_files = ?, total_loc = ?, total_sloc = ?,
                total_comments = ?, total_blank = ?, avg_complexity = ?
            WHERE id = ?
        """, (total_files, total_loc, total_sloc, total_comments,
              total_blank, avg_complexity, analysis_run_id))
        self.connection.commit()

    def get_analysis_run_by_upload_id(self, upload_id: str) -> Optional[Dict]:
        """Get analysis run by upload ID"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT ar.*, p.name as project_name, u.username as uploaded_by_username
            FROM analysis_runs ar
            JOIN projects p ON ar.project_id = p.id
            LEFT JOIN users u ON ar.uploaded_by = u.id
            WHERE ar.upload_id = ?
        """, (upload_id,))
        row = cursor.fetchone()
        return dict(row) if row else None

    def get_project_analysis_history(self, project_id: int, limit: int = 50) -> List[Dict]:
        """Get analysis history for a project"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT ar.*, u.username as uploaded_by_username
            FROM analysis_runs ar
            LEFT JOIN users u ON ar.uploaded_by = u.id
            WHERE ar.project_id = ?
            ORDER BY ar.analysis_date DESC
            LIMIT ?
        """, (project_id, limit))
        return [dict(row) for row in cursor.fetchall()]

    # ============================================
    # FILE MANAGEMENT
    # ============================================

    def create_file(self, analysis_run_id: int, file_path: str, file_name: str,
                   file_type: str = None, file_size: int = None, loc: int = 0,
                   sloc: int = 0, comments: int = 0, blank: int = 0,
                   complexity: int = 0, functions: int = 0) -> int:
        """Create a file record"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO files (analysis_run_id, file_path, file_name, file_type, file_size,
                             loc, sloc, comments, blank, complexity, functions)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (analysis_run_id, file_path, file_name, file_type, file_size,
              loc, sloc, comments, blank, complexity, functions))
        self.connection.commit()
        return cursor.lastrowid

    def get_files_by_analysis_run(self, analysis_run_id: int) -> List[Dict]:
        """Get all files for an analysis run"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT * FROM files WHERE analysis_run_id = ? ORDER BY file_path
        """, (analysis_run_id,))
        return [dict(row) for row in cursor.fetchall()]

    # ============================================
    # DEPENDENCY MANAGEMENT
    # ============================================

    def create_dependency(self, file_id: int, analysis_run_id: int,
                         dependency_type: str, target: str, line_number: int = None,
                         signature: str = None, description: str = None,
                         parameters: List[str] = None) -> int:
        """Create a dependency record with parameters"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO dependencies (file_id, analysis_run_id, dependency_type,
                                    target, line_number, signature, description)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (file_id, analysis_run_id, dependency_type, target,
              line_number, signature, description))
        dep_id = cursor.lastrowid

        # Insert parameters if provided
        if parameters:
            for idx, param in enumerate(parameters):
                cursor.execute("""
                    INSERT INTO dependency_parameters (dependency_id, parameter_value, parameter_order)
                    VALUES (?, ?, ?)
                """, (dep_id, param, idx))

        self.connection.commit()
        return dep_id

    def get_dependencies_by_file(self, file_id: int) -> List[Dict]:
        """Get all dependencies for a file with parameters"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT d.*,
                   GROUP_CONCAT(dp.parameter_value, '|||') as parameters
            FROM dependencies d
            LEFT JOIN dependency_parameters dp ON d.id = dp.dependency_id
            WHERE d.file_id = ?
            GROUP BY d.id
            ORDER BY d.line_number
        """, (file_id,))

        results = []
        for row in cursor.fetchall():
            dep = dict(row)
            # Convert parameters back to list
            if dep['parameters']:
                dep['parameters'] = dep['parameters'].split('|||')
            else:
                dep['parameters'] = []
            results.append(dep)
        return results

    def get_dependencies_by_analysis_run(self, analysis_run_id: int) -> List[Dict]:
        """Get all dependencies for an analysis run"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT d.*, f.file_path,
                   GROUP_CONCAT(dp.parameter_value, '|||') as parameters
            FROM dependencies d
            JOIN files f ON d.file_id = f.id
            LEFT JOIN dependency_parameters dp ON d.id = dp.dependency_id
            WHERE d.analysis_run_id = ?
            GROUP BY d.id
            ORDER BY f.file_path, d.line_number
        """, (analysis_run_id,))

        results = []
        for row in cursor.fetchall():
            dep = dict(row)
            if dep['parameters']:
                dep['parameters'] = dep['parameters'].split('|||')
            else:
                dep['parameters'] = []
            results.append(dep)
        return results

    # ============================================
    # DATABASE OPERATIONS MANAGEMENT
    # ============================================

    def create_db_operation(self, file_id: int, analysis_run_id: int,
                           operation_type: str, category: str = None,
                           line_number: int = None, query_text: str = None,
                           target_table: str = None, target_segment: str = None,
                           parameters: List[Tuple[str, str]] = None) -> int:
        """Create a database operation record with parameters"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO database_operations (file_id, analysis_run_id, operation_type,
                                           category, line_number, query_text,
                                           target_table, target_segment)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """, (file_id, analysis_run_id, operation_type, category, line_number,
              query_text, target_table, target_segment))
        db_op_id = cursor.lastrowid

        # Insert parameters if provided (key-value pairs)
        if parameters:
            for idx, (key, value) in enumerate(parameters):
                cursor.execute("""
                    INSERT INTO db_operation_parameters (db_operation_id, parameter_key,
                                                        parameter_value, parameter_order)
                    VALUES (?, ?, ?, ?)
                """, (db_op_id, key, value, idx))

        self.connection.commit()
        return db_op_id

    def get_db_operations_by_analysis_run(self, analysis_run_id: int) -> List[Dict]:
        """Get all database operations for an analysis run"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT dbo.*, f.file_path,
                   GROUP_CONCAT(dbop.parameter_key || ':' || dbop.parameter_value, '|||') as parameters
            FROM database_operations dbo
            JOIN files f ON dbo.file_id = f.id
            LEFT JOIN db_operation_parameters dbop ON dbo.id = dbop.db_operation_id
            WHERE dbo.analysis_run_id = ?
            GROUP BY dbo.id
            ORDER BY f.file_path, dbo.line_number
        """, (analysis_run_id,))

        results = []
        for row in cursor.fetchall():
            op = dict(row)
            if op['parameters']:
                # Convert 'key:value|||key:value' to list of tuples
                param_list = []
                for param_str in op['parameters'].split('|||'):
                    if ':' in param_str:
                        key, value = param_str.split(':', 1)
                        param_list.append((key, value))
                op['parameters'] = param_list
            else:
                op['parameters'] = []
            results.append(op)
        return results

    # ============================================
    # BUSINESS RULES MANAGEMENT
    # ============================================

    def create_business_rule(self, file_id: int, analysis_run_id: int,
                            rule_type: str, line_number: int = None,
                            condition_name: str = None, condition_value: str = None,
                            description: str = None, code_snippet: str = None,
                            confidence_score: float = 1.0) -> int:
        """Create a business rule record"""
        cursor = self.connection.cursor()
        cursor.execute("""
            INSERT INTO business_rules (file_id, analysis_run_id, rule_type,
                                      line_number, condition_name, condition_value,
                                      description, code_snippet, confidence_score)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (file_id, analysis_run_id, rule_type, line_number, condition_name,
              condition_value, description, code_snippet, confidence_score))
        self.connection.commit()
        return cursor.lastrowid

    def get_business_rules_by_analysis_run(self, analysis_run_id: int) -> List[Dict]:
        """Get all business rules for an analysis run"""
        cursor = self.connection.cursor()
        cursor.execute("""
            SELECT br.*, f.file_path
            FROM business_rules br
            JOIN files f ON br.file_id = f.id
            WHERE br.analysis_run_id = ?
            ORDER BY f.file_path, br.line_number
        """, (analysis_run_id,))
        return [dict(row) for row in cursor.fetchall()]

    # ============================================
    # QUERY HELPERS
    # ============================================

    def get_full_analysis_results(self, upload_id: str) -> Optional[Dict]:
        """Get complete analysis results for an upload_id"""
        analysis_run = self.get_analysis_run_by_upload_id(upload_id)
        if not analysis_run:
            return None

        analysis_run_id = analysis_run['id']

        # Get all related data
        files = self.get_files_by_analysis_run(analysis_run_id)
        dependencies = self.get_dependencies_by_analysis_run(analysis_run_id)
        db_operations = self.get_db_operations_by_analysis_run(analysis_run_id)
        business_rules = self.get_business_rules_by_analysis_run(analysis_run_id)

        return {
            'analysis_run': analysis_run,
            'files': files,
            'dependencies': dependencies,
            'database_operations': db_operations,
            'business_rules': business_rules
        }

    # ============================================
    # DELETE OPERATIONS
    # ============================================

    def delete_analysis_run(self, analysis_run_id: int) -> bool:
        """
        Delete an analysis run and all related data (cascading delete)
        Returns True if successful, False if not found
        """
        try:
            cursor = self.connection.cursor()
            # Check if exists
            cursor.execute("SELECT id FROM analysis_runs WHERE id = ?", (analysis_run_id,))
            if not cursor.fetchone():
                return False

            # Delete (cascade will handle related tables)
            cursor.execute("DELETE FROM analysis_runs WHERE id = ?", (analysis_run_id,))
            self.connection.commit()
            print(f"[INFO] Deleted analysis run {analysis_run_id} and all related data")
            return True
        except Exception as e:
            print(f"[ERROR] Failed to delete analysis run {analysis_run_id}: {e}")
            self.connection.rollback()
            raise

    def delete_analysis_run_by_upload_id(self, upload_id: str) -> bool:
        """
        Delete an analysis run by upload_id and all related data
        Returns True if successful, False if not found
        """
        try:
            cursor = self.connection.cursor()
            # Get analysis run id
            cursor.execute("SELECT id FROM analysis_runs WHERE upload_id = ?", (upload_id,))
            row = cursor.fetchone()
            if not row:
                return False

            analysis_run_id = row['id']
            return self.delete_analysis_run(analysis_run_id)
        except Exception as e:
            print(f"[ERROR] Failed to delete analysis run with upload_id {upload_id}: {e}")
            raise

    def delete_all_analysis_runs(self) -> int:
        """
        Delete all analysis runs and related data
        Returns the number of analysis runs deleted
        """
        try:
            cursor = self.connection.cursor()
            # Count before deleting
            cursor.execute("SELECT COUNT(*) as count FROM analysis_runs")
            count = cursor.fetchone()['count']

            # Delete all
            cursor.execute("DELETE FROM analysis_runs")
            self.connection.commit()
            print(f"[INFO] Deleted all {count} analysis runs and related data")
            return count
        except Exception as e:
            print(f"[ERROR] Failed to delete all analysis runs: {e}")
            self.connection.rollback()
            raise

    def execute_query(self, query: str, params: tuple = ()) -> List[Dict]:
        """Execute a custom query and return results"""
        cursor = self.connection.cursor()
        cursor.execute(query, params)
        return [dict(row) for row in cursor.fetchall()]

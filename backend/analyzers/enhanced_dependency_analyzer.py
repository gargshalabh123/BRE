"""
Enhanced Dependency Analyzer
Creates first-class entities for programs, DB objects, copybooks,
and tracks comprehensive relationships between them
"""
import re
import json
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
from collections import defaultdict, deque


class EnhancedDependencyAnalyzer:
    """
    Enhanced dependency analyzer that creates registries for:
    - Programs (COBOL, Python, JS, etc.)
    - Database objects (tables, procedures, views)
    - Copybooks (COBOL includes)
    - BMS Maps (CICS screens)

    And tracks relationships:
    - Program -> Program calls
    - Program -> Database access
    - Program -> Copybook usage
    - Program -> BMS map usage
    """

    def __init__(self, analysis_run_id: int, db_manager):
        self.analysis_run_id = analysis_run_id
        self.db = db_manager

        # Registries built during analysis (in-memory cache)
        self.programs = {}  # program_name -> program_id
        self.db_objects = {}  # object_name -> db_object_id
        self.copybooks = {}  # copybook_name -> copybook_id
        self.bms_maps = {}  # (mapset, map) -> bms_map_id

        # File path to file_id mapping
        self.file_ids = {}

        print(f"[Enhanced Dependency] Initialized for analysis_run_id={analysis_run_id}")

    def analyze_codebase(self, files: List[Path], upload_dir: Path) -> Dict[str, Any]:
        """
        Two-pass analysis:
        1. Register all entities (programs, DB objects, copybooks)
        2. Extract relationships between entities
        3. Build call graph
        """
        self.upload_dir = upload_dir

        print("[Enhanced Dependency] Phase 1: Registering entities...")
        self._register_entities(files)

        print("[Enhanced Dependency] Phase 2: Extracting relationships...")
        self._extract_relationships(files)

        print("[Enhanced Dependency] Phase 3: Building call graph...")
        self._build_call_graph()

        return {
            'programs_found': len(self.programs),
            'db_objects_found': len(self.db_objects),
            'copybooks_found': len(self.copybooks),
            'bms_maps_found': len(self.bms_maps)
        }

    # ========================================
    # PHASE 1: ENTITY REGISTRATION
    # ========================================

    def _register_entities(self, files: List[Path]):
        """Register all programs, DB objects, and copybooks"""
        for file_path in files:
            ext = file_path.suffix.lower()

            # Register programs
            if ext in ['.cbl', '.cob', '.cobol']:
                self._register_cobol_program(file_path)
            elif ext == '.py':
                self._register_python_module(file_path)
            elif ext in ['.js', '.ts', '.jsx', '.tsx']:
                self._register_js_module(file_path)

            # Register copybooks
            if ext in ['.cpy', '.copy']:
                self._register_copybook(file_path)

            # Register DB objects
            if ext in ['.sql', '.ddl']:
                self._register_db_objects_from_sql(file_path)

    def _register_cobol_program(self, file_path: Path):
        """Register a COBOL program"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception as e:
            print(f"  Warning: Could not read {file_path}: {e}")
            return

        # Extract program name from PROGRAM-ID
        program_name = None
        cics_trans_id = None
        is_entry_point = False

        for line in lines[:200]:  # Check first 200 lines
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # PROGRAM-ID
            if 'PROGRAM-ID' in clean_line.upper():
                match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
                if match:
                    program_name = match.group(1)

            # Check if CICS program (has EXEC CICS)
            if 'EXEC CICS' in clean_line.upper():
                # Try to find transaction ID
                trans_match = re.search(r'TRANSID\([\'"]?([A-Z0-9]+)[\'"]?\)', clean_line, re.IGNORECASE)
                if trans_match:
                    cics_trans_id = trans_match.group(1)
                    is_entry_point = True  # CICS transaction = entry point

        if not program_name:
            # Fallback to filename
            program_name = file_path.stem.upper()

        # Get file_id
        file_id = self._get_file_id(file_path)

        # Insert into programs table
        cursor = self.db.connection.cursor()
        cursor.execute("""
            INSERT INTO programs (analysis_run_id, program_name, program_type,
                                 file_id, file_path, language, cics_transaction_id, entry_point)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """, (self.analysis_run_id, program_name, 'COBOL_PROGRAM',
              file_id, str(file_path), 'COBOL', cics_trans_id, is_entry_point))
        program_id = cursor.lastrowid
        self.db.connection.commit()

        self.programs[program_name] = program_id
        print(f"  Registered COBOL program: {program_name} (id={program_id})")

    def _register_python_module(self, file_path: Path):
        """Register a Python module as a program"""
        module_name = file_path.stem

        # Determine if entry point (has if __name__ == '__main__')
        is_entry_point = False
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                if '__name__' in content and '__main__' in content:
                    is_entry_point = True
        except Exception:
            pass

        file_id = self._get_file_id(file_path)

        cursor = self.db.connection.cursor()
        cursor.execute("""
            INSERT INTO programs (analysis_run_id, program_name, program_type,
                                 file_id, file_path, language, entry_point)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (self.analysis_run_id, module_name, 'PYTHON_MODULE',
              file_id, str(file_path), 'Python', is_entry_point))
        program_id = cursor.lastrowid
        self.db.connection.commit()

        self.programs[module_name] = program_id
        print(f"  Registered Python module: {module_name} (id={program_id})")

    def _register_js_module(self, file_path: Path):
        """Register a JavaScript/TypeScript module"""
        module_name = file_path.stem

        # Check if entry point (common patterns)
        is_entry_point = False
        if module_name.lower() in ['index', 'main', 'app', 'server']:
            is_entry_point = True

        file_id = self._get_file_id(file_path)

        cursor = self.db.connection.cursor()
        cursor.execute("""
            INSERT INTO programs (analysis_run_id, program_name, program_type,
                                 file_id, file_path, language, entry_point)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (self.analysis_run_id, module_name, 'JS_MODULE',
              file_id, str(file_path), 'JavaScript', is_entry_point))
        program_id = cursor.lastrowid
        self.db.connection.commit()

        self.programs[module_name] = program_id
        print(f"  Registered JS module: {module_name} (id={program_id})")

    def _register_db_objects_from_sql(self, file_path: Path):
        """Extract and register database objects from SQL DDL"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception as e:
            print(f"  Warning: Could not read {file_path}: {e}")
            return

        file_id = self._get_file_id(file_path)

        # Extract tables
        for i, line in enumerate(lines):
            # CREATE TABLE
            table_match = re.search(
                r'CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?(?:([`\[\]\'"\w]+)\.)?([`\[\]\'"\w]+)',
                line, re.IGNORECASE
            )
            if table_match:
                schema = table_match.group(1).strip('`[]\'"') if table_match.group(1) else None
                table_name = table_match.group(2).strip('`[]\'"')

                # Extract columns
                columns = []
                j = i
                while j < len(lines) and j < i + 100:
                    col_match = re.search(
                        r'^\s*([`\[\]\'"\w]+)\s+(VARCHAR|INT|INTEGER|DATE|DECIMAL|TEXT|BIGINT|CHAR|TIMESTAMP|BOOLEAN|NUMERIC|REAL|FLOAT|DOUBLE)',
                        lines[j], re.IGNORECASE
                    )
                    if col_match:
                        col_name = col_match.group(1).strip('`[]\'"')
                        col_type = col_match.group(2).upper()
                        columns.append({'name': col_name, 'type': col_type})
                    if ';' in lines[j]:
                        break
                    j += 1

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO database_objects
                    (analysis_run_id, object_name, object_type, schema_name,
                     database_type, definition_file_id, columns_json)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, table_name, 'TABLE', schema,
                      'SQL', file_id, json.dumps(columns)))
                db_object_id = cursor.lastrowid
                self.db.connection.commit()

                self.db_objects[table_name.upper()] = db_object_id
                print(f"  Registered table: {table_name} ({len(columns)} columns)")

            # CREATE PROCEDURE/FUNCTION
            proc_match = re.search(
                r'CREATE\s+(?:OR\s+REPLACE\s+)?(PROCEDURE|FUNCTION)\s+([`\[\]\'"\w]+)',
                line, re.IGNORECASE
            )
            if proc_match:
                obj_type = proc_match.group(1).upper()
                obj_name = proc_match.group(2).strip('`[]\'"')

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO database_objects
                    (analysis_run_id, object_name, object_type, database_type, definition_file_id)
                    VALUES (?, ?, ?, ?, ?)
                """, (self.analysis_run_id, obj_name, obj_type, 'SQL', file_id))
                db_object_id = cursor.lastrowid
                self.db.connection.commit()

                self.db_objects[obj_name.upper()] = db_object_id
                print(f"  Registered {obj_type.lower()}: {obj_name}")

    def _register_copybook(self, file_path: Path):
        """Register a copybook and extract its data structures"""
        copybook_name = file_path.stem.upper()

        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception as e:
            print(f"  Warning: Could not read {file_path}: {e}")
            return

        # Extract data structures (01 level items)
        data_structures = []
        for line in lines:
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # 01 level data items
            match = re.match(r'^\s*01\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if match:
                struct_name = match.group(1)
                data_structures.append(struct_name)

        file_id = self._get_file_id(file_path)

        cursor = self.db.connection.cursor()
        cursor.execute("""
            INSERT INTO copybooks
            (analysis_run_id, copybook_name, file_id, file_path, data_structures_json)
            VALUES (?, ?, ?, ?, ?)
        """, (self.analysis_run_id, copybook_name, file_id,
              str(file_path), json.dumps(data_structures)))
        copybook_id = cursor.lastrowid
        self.db.connection.commit()

        self.copybooks[copybook_name] = copybook_id
        print(f"  Registered copybook: {copybook_name} ({len(data_structures)} structures)")

    # ========================================
    # PHASE 2: RELATIONSHIP EXTRACTION
    # ========================================

    def _extract_relationships(self, files: List[Path]):
        """Extract relationships between entities"""
        for file_path in files:
            ext = file_path.suffix.lower()

            if ext in ['.cbl', '.cob', '.cobol']:
                self._extract_cobol_relationships(file_path)
            elif ext == '.py':
                self._extract_python_relationships(file_path)
            elif ext in ['.js', '.ts', '.jsx', '.tsx']:
                self._extract_js_relationships(file_path)

    def _extract_cobol_relationships(self, file_path: Path):
        """Extract COBOL program relationships"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception:
            return

        # Get program_id for this file
        program_name = self._get_program_name_from_file(file_path)
        if not program_name or program_name not in self.programs:
            return

        program_id = self.programs[program_name]
        file_id = self._get_file_id(file_path)

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # Skip comments
            if len(line) >= 7 and line[6] in ['*', '/', '$']:
                continue

            # 1. PROGRAM CALLS
            call_match = re.search(
                r'\bCALL\s+[\'"]?([A-Z0-9\-]+)[\'"]?\s*(USING\s+(.+?))?(?:END-CALL|\.|$)',
                clean_line, re.IGNORECASE
            )
            if call_match:
                callee_name = call_match.group(1)
                using_clause = call_match.group(3)

                # Extract parameters
                parameters = []
                if using_clause:
                    params = re.findall(r'([A-Z0-9\-]+)', using_clause, re.IGNORECASE)
                    parameters = [p for p in params if p.upper() not in ['BY', 'REFERENCE', 'CONTENT', 'VALUE']]

                callee_id = self.programs.get(callee_name)

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_calls
                    (analysis_run_id, caller_program_id, caller_file_id, caller_line_number,
                     callee_program_id, callee_program_name, call_type, call_mechanism,
                     parameters_json, call_signature)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      callee_id, callee_name, 'STATIC_CALL', 'CALL',
                      json.dumps(parameters), clean_line[:200]))
                self.db.connection.commit()

            # CICS XCTL
            xctl_match = re.search(
                r'EXEC\s+CICS\s+XCTL\s+PROGRAM\([\'"]?([A-Z0-9\-]+)[\'"]?\)',
                clean_line, re.IGNORECASE
            )
            if xctl_match:
                callee_name = xctl_match.group(1)
                callee_id = self.programs.get(callee_name)

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_calls
                    (analysis_run_id, caller_program_id, caller_file_id, caller_line_number,
                     callee_program_id, callee_program_name, call_type, call_mechanism,
                     call_signature)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      callee_id, callee_name, 'CICS_XCTL', 'EXEC CICS XCTL',
                      clean_line[:200]))
                self.db.connection.commit()

            # CICS LINK
            link_match = re.search(
                r'EXEC\s+CICS\s+LINK\s+PROGRAM\([\'"]?([A-Z0-9\-]+)[\'"]?\)',
                clean_line, re.IGNORECASE
            )
            if link_match:
                callee_name = link_match.group(1)
                callee_id = self.programs.get(callee_name)

                # Extract COMMAREA
                commarea_match = re.search(r'COMMAREA\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                commarea = commarea_match.group(1) if commarea_match else None

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_calls
                    (analysis_run_id, caller_program_id, caller_file_id, caller_line_number,
                     callee_program_id, callee_program_name, call_type, call_mechanism,
                     commarea_structure, call_signature)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      callee_id, callee_name, 'CICS_LINK', 'EXEC CICS LINK',
                      commarea, clean_line[:200]))
                self.db.connection.commit()

            # 2. COPYBOOK USAGE
            copy_match = re.search(r'\bCOPY\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if copy_match:
                copybook_name = copy_match.group(1)
                copybook_id = self.copybooks.get(copybook_name)

                # Determine context
                context = 'UNKNOWN'
                context_lines = '\n'.join(lines[max(0, i-30):i]).upper()
                if 'WORKING-STORAGE' in context_lines:
                    context = 'WORKING_STORAGE'
                elif 'LINKAGE' in context_lines:
                    context = 'LINKAGE_SECTION'
                elif 'FILE' in context_lines and 'SECTION' in context_lines:
                    context = 'FILE_SECTION'

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_copybook_usage
                    (analysis_run_id, program_id, file_id, line_number,
                     copybook_id, copybook_name, usage_context)
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      copybook_id, copybook_name, context))
                self.db.connection.commit()

            # 3. DATABASE ACCESS (Embedded SQL)
            if 'EXEC SQL' in clean_line.upper():
                self._extract_sql_from_exec(program_id, file_id, i, lines[i-1:min(len(lines), i+10)])

            # 4. BMS MAP USAGE
            send_map_match = re.search(
                r'EXEC\s+CICS\s+SEND\s+MAP\([\'"]?([A-Z0-9]+)[\'"]?\)\s+MAPSET\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line, re.IGNORECASE
            )
            if send_map_match:
                map_name = send_map_match.group(1)
                mapset_name = send_map_match.group(2)

                bms_map_id = self._get_or_create_bms_map(map_name, mapset_name)

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_bms_usage
                    (analysis_run_id, program_id, file_id, line_number,
                     bms_map_id, map_name, mapset_name, operation)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      bms_map_id, map_name, mapset_name, 'SEND'))
                self.db.connection.commit()

            receive_map_match = re.search(
                r'EXEC\s+CICS\s+RECEIVE\s+MAP\([\'"]?([A-Z0-9]+)[\'"]?\)\s+MAPSET\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line, re.IGNORECASE
            )
            if receive_map_match:
                map_name = receive_map_match.group(1)
                mapset_name = receive_map_match.group(2)

                bms_map_id = self._get_or_create_bms_map(map_name, mapset_name)

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_bms_usage
                    (analysis_run_id, program_id, file_id, line_number,
                     bms_map_id, map_name, mapset_name, operation)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      bms_map_id, map_name, mapset_name, 'RECEIVE'))
                self.db.connection.commit()

    def _extract_python_relationships(self, file_path: Path):
        """Extract Python module relationships"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')
        except Exception:
            return

        module_name = file_path.stem
        if module_name not in self.programs:
            return

        program_id = self.programs[module_name]
        file_id = self._get_file_id(file_path)

        for i, line in enumerate(lines, 1):
            # Function calls (simplified - captures function_name())
            func_call_match = re.search(r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\(', line)
            if func_call_match:
                func_name = func_call_match.group(1)

                # Skip common built-ins
                if func_name in ['if', 'for', 'while', 'def', 'class', 'print', 'len', 'range', 'str', 'int', 'return']:
                    continue

                # Check if it's a known module function
                callee_id = None
                for prog_name, prog_id in self.programs.items():
                    if func_name in prog_name or prog_name in func_name:
                        callee_id = prog_id
                        break

                cursor = self.db.connection.cursor()
                cursor.execute("""
                    INSERT INTO program_calls
                    (analysis_run_id, caller_program_id, caller_file_id, caller_line_number,
                     callee_program_id, callee_program_name, call_type, call_mechanism,
                     call_signature)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """, (self.analysis_run_id, program_id, file_id, i,
                      callee_id, func_name, 'FUNCTION_CALL', 'function_call',
                      line.strip()[:200]))
                self.db.connection.commit()

    def _extract_js_relationships(self, file_path: Path):
        """Extract JavaScript/TypeScript relationships"""
        # Similar to Python but with JS patterns
        pass

    def _extract_sql_from_exec(self, program_id: int, file_id: int, start_line: int, lines: List[str]):
        """Extract SQL statement from EXEC SQL block"""
        sql_buffer = []

        for line in lines:
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()
            sql_buffer.append(clean_line)

            if 'END-EXEC' in clean_line.upper():
                break

        sql_statement = ' '.join(sql_buffer)

        # Determine operation type
        access_type = 'UNKNOWN'
        access_mode = 'READ'
        for cmd in ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'CALL']:
            if cmd in sql_statement.upper():
                access_type = cmd
                if cmd in ['INSERT', 'UPDATE', 'DELETE']:
                    access_mode = 'WRITE'
                elif cmd == 'SELECT':
                    access_mode = 'READ'
                break

        # Extract table name (simplified)
        table_name = None
        if access_type == 'SELECT':
            table_match = re.search(r'FROM\s+([A-Z0-9_]+)', sql_statement, re.IGNORECASE)
            if table_match:
                table_name = table_match.group(1)
        elif access_type in ['INSERT', 'UPDATE', 'DELETE']:
            table_match = re.search(r'(?:INTO|UPDATE|FROM)\s+([A-Z0-9_]+)', sql_statement, re.IGNORECASE)
            if table_match:
                table_name = table_match.group(1)

        if table_name:
            db_object_id = self.db_objects.get(table_name.upper())

            cursor = self.db.connection.cursor()
            cursor.execute("""
                INSERT INTO program_db_access
                (analysis_run_id, program_id, file_id, line_number,
                 db_object_id, db_object_name, db_object_type, access_type,
                 access_mode, sql_statement)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """, (self.analysis_run_id, program_id, file_id, start_line,
                  db_object_id, table_name, 'TABLE', access_type,
                  access_mode, sql_statement[:500]))
            self.db.connection.commit()

    # ========================================
    # PHASE 3: CALL GRAPH BUILDING
    # ========================================

    def _build_call_graph(self):
        """Build complete call graph with transitive paths"""
        print("  Building call graph paths...")

        # Get all direct calls
        cursor = self.db.connection.cursor()
        cursor.execute("""
            SELECT caller_program_id, callee_program_id
            FROM program_calls
            WHERE analysis_run_id = ? AND callee_program_id IS NOT NULL
        """, (self.analysis_run_id,))
        calls = cursor.fetchall()

        # Build adjacency list
        graph = defaultdict(list)
        for call in calls:
            caller_id = call[0]
            callee_id = call[1]
            if caller_id and callee_id:
                graph[caller_id].append(callee_id)

        # For each program, find all reachable programs (BFS)
        paths_inserted = 0
        for source_id in self.programs.values():
            paths = self._find_all_paths_bfs(source_id, graph, max_depth=10)

            # Save paths
            for target_id, path in paths.items():
                if source_id != target_id:
                    path_names = [self._get_program_name_by_id(pid) for pid in path]

                    cursor.execute("""
                        INSERT INTO call_graph_paths
                        (analysis_run_id, source_program_id, target_program_id,
                         path_length, path_json, path_description)
                        VALUES (?, ?, ?, ?, ?, ?)
                    """, (self.analysis_run_id, source_id, target_id,
                          len(path) - 1,
                          json.dumps(path),
                          ' -> '.join(path_names)))
                    paths_inserted += 1

        self.db.connection.commit()
        print(f"  Call graph built with {paths_inserted} paths")

    def _find_all_paths_bfs(self, start: int, graph: Dict, max_depth: int = 10) -> Dict[int, List[int]]:
        """Find shortest paths from start to all reachable nodes using BFS"""
        visited = {start: [start]}
        queue = deque([(start, [start])])

        while queue:
            node, path = queue.popleft()

            if len(path) > max_depth:
                continue

            for neighbor in graph.get(node, []):
                if neighbor not in visited:
                    new_path = path + [neighbor]
                    visited[neighbor] = new_path
                    queue.append((neighbor, new_path))

        return visited

    # ========================================
    # HELPER METHODS
    # ========================================

    def _get_file_id(self, file_path: Path) -> Optional[int]:
        """Get file_id from files table"""
        # Convert to relative path from upload dir
        try:
            rel_path = str(file_path.relative_to(self.upload_dir))
        except ValueError:
            rel_path = str(file_path)

        if rel_path in self.file_ids:
            return self.file_ids[rel_path]

        cursor = self.db.connection.cursor()
        cursor.execute("""
            SELECT id FROM files
            WHERE analysis_run_id = ? AND file_path = ?
        """, (self.analysis_run_id, rel_path))
        result = cursor.fetchone()

        if result:
            file_id = result[0]
            self.file_ids[rel_path] = file_id
            return file_id

        return None

    def _get_program_name_from_file(self, file_path: Path) -> Optional[str]:
        """Extract program name from file"""
        # For COBOL, look for PROGRAM-ID
        if file_path.suffix.lower() in ['.cbl', '.cob', '.cobol']:
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    for line in f:
                        clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()
                        if 'PROGRAM-ID' in clean_line.upper():
                            match = re.search(r'PROGRAM-ID\.\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
                            if match:
                                return match.group(1)
            except Exception:
                pass

        # Fallback to filename
        return file_path.stem.upper()

    def _get_program_name_by_id(self, program_id: int) -> str:
        """Get program name by ID"""
        for name, pid in self.programs.items():
            if pid == program_id:
                return name
        return f"UNKNOWN_{program_id}"

    def _get_or_create_bms_map(self, map_name: str, mapset_name: str) -> int:
        """Get or create BMS map entry"""
        key = (mapset_name, map_name)

        if key in self.bms_maps:
            return self.bms_maps[key]

        cursor = self.db.connection.cursor()
        cursor.execute("""
            INSERT INTO bms_maps
            (analysis_run_id, map_name, mapset_name)
            VALUES (?, ?, ?)
        """, (self.analysis_run_id, map_name, mapset_name))
        bms_map_id = cursor.lastrowid
        self.db.connection.commit()

        self.bms_maps[key] = bms_map_id
        return bms_map_id

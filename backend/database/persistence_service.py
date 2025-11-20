"""
Persistence Service - Saves analysis results to SQLite database
"""
from typing import Dict, Any, List, Optional
from pathlib import Path
from .db_manager import DatabaseManager


class PersistenceService:
    """Service to persist analysis results to database"""

    def __init__(self, db_manager: DatabaseManager):
        self.db = db_manager

    def save_analysis_results(self, upload_id: str, analysis_results: Dict[str, Any],
                              project_name: str = "Default Project",
                              upload_filename: str = None,
                              user_id: int = None) -> int:
        """
        Save complete analysis results to database

        Args:
            upload_id: Unique upload identifier
            analysis_results: Analysis results from CodeAnalyzer
            project_name: Project name (default: "Default Project")
            upload_filename: Original uploaded filename
            user_id: User who uploaded (optional)

        Returns:
            analysis_run_id: ID of created analysis run
        """
        try:
            # Get or create project
            project_id = self.db.get_or_create_project(project_name, created_by=user_id)

            # Create analysis run
            analysis_run_id = self.db.create_analysis_run(
                project_id=project_id,
                upload_id=upload_id,
                upload_filename=upload_filename,
                uploaded_by=user_id
            )

            # Update status to in_progress
            self.db.update_analysis_run_status(analysis_run_id, 'in_progress')

            # Save metrics
            metrics = analysis_results.get('metrics', {})
            if metrics:
                self._save_metrics(analysis_run_id, metrics)

            # Save dependencies
            dependencies = analysis_results.get('detailed_dependencies', {})
            if dependencies:
                self._save_dependencies(analysis_run_id, dependencies)

            # Save database operations
            db_ops = analysis_results.get('database_operations', {})
            if db_ops:
                self._save_database_operations(analysis_run_id, db_ops)

            # Save business rules
            business_rules = analysis_results.get('business_rules', [])
            if business_rules:
                self._save_business_rules(analysis_run_id, business_rules)

            # Update summary metrics
            if metrics:
                total_files = len(metrics.get('by_file', []))
                total_loc = metrics.get('total_loc', 0)
                total_sloc = metrics.get('total_sloc', 0)
                total_comments = metrics.get('total_comments', 0)
                total_blank = metrics.get('total_blank', 0)
                avg_complexity = metrics.get('average_complexity', 0.0)

                self.db.update_analysis_run_metrics(
                    analysis_run_id, total_files, total_loc, total_sloc,
                    total_comments, total_blank, avg_complexity
                )

            # Mark as completed
            self.db.update_analysis_run_status(analysis_run_id, 'completed')

            # Log activity
            if user_id:
                self.db.log_user_activity(
                    user_id, 'analyze',
                    f'Completed analysis for {upload_filename or upload_id}'
                )

            print(f"[INFO] Successfully saved analysis results to database (run_id: {analysis_run_id})")
            return analysis_run_id

        except Exception as e:
            print(f"[ERROR] Failed to save analysis results: {e}")
            if 'analysis_run_id' in locals():
                self.db.update_analysis_run_status(
                    analysis_run_id, 'failed', error_message=str(e)
                )
            raise

    def _save_metrics(self, analysis_run_id: int, metrics: Dict[str, Any]):
        """Save file metrics to database"""
        by_file = metrics.get('by_file', [])

        for file_metrics in by_file:
            file_path = file_metrics.get('file', '')
            file_name = Path(file_path).name
            file_type = Path(file_path).suffix

            file_id = self.db.create_file(
                analysis_run_id=analysis_run_id,
                file_path=file_path,
                file_name=file_name,
                file_type=file_type,
                loc=file_metrics.get('loc', 0),
                sloc=file_metrics.get('sloc', 0),
                comments=file_metrics.get('comments', 0),
                blank=file_metrics.get('blank', 0),
                complexity=file_metrics.get('complexity', 0),
                functions=file_metrics.get('functions', 0)
            )

        print(f"[INFO] Saved {len(by_file)} file metrics")

    def _save_dependencies(self, analysis_run_id: int, dependencies: Dict[str, List]):
        """Save dependencies to database"""
        total_deps = 0

        for file_path, deps in dependencies.items():
            # Find file_id
            files = self.db.execute_query(
                "SELECT id FROM files WHERE analysis_run_id = ? AND file_path = ?",
                (analysis_run_id, file_path)
            )

            if not files:
                print(f"[WARN] File not found for dependencies: {file_path}")
                continue

            file_id = files[0]['id']

            for dep in deps:
                self.db.create_dependency(
                    file_id=file_id,
                    analysis_run_id=analysis_run_id,
                    dependency_type=dep.get('type', 'UNKNOWN'),
                    target=dep.get('target', ''),
                    line_number=dep.get('line'),
                    signature=dep.get('signature'),
                    description=dep.get('description'),
                    parameters=dep.get('parameters', [])
                )
                total_deps += 1

        print(f"[INFO] Saved {total_deps} dependencies")

    def _save_database_operations(self, analysis_run_id: int, db_ops: Dict[str, Any]):
        """Save database operations to database"""
        queries = db_ops.get('queries', [])
        total_ops = 0

        for query in queries:
            file_path = query.get('file', '')

            # Find file_id
            files = self.db.execute_query(
                "SELECT id FROM files WHERE analysis_run_id = ? AND file_path = ?",
                (analysis_run_id, file_path)
            )

            if not files:
                print(f"[WARN] File not found for DB operation: {file_path}")
                continue

            file_id = files[0]['id']

            # Parse parameters if they exist
            parameters = []
            if 'parameters' in query and query['parameters']:
                for param in query['parameters']:
                    if ':' in param:
                        key, value = param.split(':', 1)
                        parameters.append((key, value))
                    else:
                        parameters.append(('VALUE', param))

            self.db.create_db_operation(
                file_id=file_id,
                analysis_run_id=analysis_run_id,
                operation_type=query.get('type', 'UNKNOWN'),
                category=query.get('category'),
                line_number=query.get('line'),
                query_text=query.get('query'),
                target_table=query.get('target'),
                parameters=parameters
            )
            total_ops += 1

        print(f"[INFO] Saved {total_ops} database operations")

    def _save_business_rules(self, analysis_run_id: int, business_rules: List[Dict]):
        """Save business rules to database"""
        total_rules = 0

        for rule in business_rules:
            file_path = rule.get('file', '')

            # Find file_id
            files = self.db.execute_query(
                "SELECT id FROM files WHERE analysis_run_id = ? AND file_path = ?",
                (analysis_run_id, file_path)
            )

            if not files:
                print(f"[WARN] File not found for business rule: {file_path}")
                continue

            file_id = files[0]['id']

            self.db.create_business_rule(
                file_id=file_id,
                analysis_run_id=analysis_run_id,
                rule_type=rule.get('type', 'UNKNOWN'),
                line_number=rule.get('line'),
                condition_name=rule.get('condition_name'),
                condition_value=rule.get('value'),
                description=rule.get('description'),
                code_snippet=rule.get('code_snippet'),
                confidence_score=rule.get('confidence', 1.0)
            )
            total_rules += 1

        print(f"[INFO] Saved {total_rules} business rules")

    def load_analysis_results(self, upload_id: str) -> Optional[Dict[str, Any]]:
        """
        Load analysis results from database by upload_id

        Returns analysis results in the same format as CodeAnalyzer.analyze_all()
        """
        db_results = self.db.get_full_analysis_results(upload_id)

        if not db_results:
            return None

        # Transform database format to API format
        return self._transform_to_api_format(db_results)

    def _transform_to_api_format(self, db_results: Dict[str, Any]) -> Dict[str, Any]:
        """Transform database results to API format"""
        analysis_run = db_results['analysis_run']
        files = db_results['files']
        dependencies = db_results['dependencies']
        db_operations = db_results['database_operations']
        business_rules = db_results['business_rules']

        # Build metrics
        by_file = []
        for file in files:
            by_file.append({
                'file': file['file_path'],
                'loc': file['loc'],
                'sloc': file['sloc'],
                'comments': file['comments'],
                'blank': file['blank'],
                'complexity': file['complexity'],
                'functions': file['functions']
            })

        metrics = {
            'total_loc': analysis_run['total_loc'],
            'total_sloc': analysis_run['total_sloc'],
            'total_comments': analysis_run['total_comments'],
            'total_blank': analysis_run['total_blank'],
            'average_complexity': analysis_run['avg_complexity'],
            'by_file': by_file,
            'by_language': {}  # TODO: Calculate from files
        }

        # Build dependencies dict
        deps_by_file = {}
        for dep in dependencies:
            file_path = dep['file_path']
            if file_path not in deps_by_file:
                deps_by_file[file_path] = []

            deps_by_file[file_path].append({
                'type': dep['dependency_type'],
                'target': dep['target'],
                'line': dep['line_number'],
                'signature': dep['signature'],
                'description': dep['description'],
                'parameters': dep['parameters']
            })

        # Build database operations
        queries = []
        for op in db_operations:
            query_dict = {
                'file': op['file_path'],
                'line': op['line_number'],
                'type': op['operation_type'],
                'category': op['category'],
                'query': op['query_text'],
                'target': op['target_table'] or op['target_segment']
            }

            # Add parameters if they exist
            if op.get('parameters'):
                params = []
                for key, value in op['parameters']:
                    params.append(f"{key}:{value}")
                query_dict['parameters'] = params

            queries.append(query_dict)

        # Count by type
        by_type = {}
        for query in queries:
            qtype = query['type']
            by_type[qtype] = by_type.get(qtype, 0) + 1

        db_ops_result = {
            'queries': queries,
            'total_count': len(queries),
            'by_type': by_type
        }

        # Build business rules list
        rules_list = []
        for rule in business_rules:
            rules_list.append({
                'file': rule['file_path'],
                'line': rule['line_number'],
                'type': rule['rule_type'],
                'condition_name': rule['condition_name'],
                'value': rule['condition_value'],
                'description': rule['description'],
                'code_snippet': rule['code_snippet']
            })

        return {
            'metrics': metrics,
            'dependencies': {},  # Keep for backward compatibility
            'detailed_dependencies': deps_by_file,
            'database_operations': db_ops_result,
            'business_rules': rules_list,
            'complexity': {},  # TODO: Add complexity details
            'TEST_SERVER_RELOADED': True,
            'TEST_MESSAGE': 'LOADED FROM DATABASE',
            'CACHED_FROM_DB': True,
            'analysis_run_id': analysis_run['id']
        }

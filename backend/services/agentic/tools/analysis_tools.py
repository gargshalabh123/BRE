"""
Analysis tools for querying code metrics, database operations, and business rules

These tools query pre-computed analysis results (no file access needed)
"""
from typing import Dict, List, Optional, Any
from collections import defaultdict

from .base_tool import ToolBase


class AnalysisTool(ToolBase):
    """Tools for querying analysis results"""

    async def get_file_metrics(self, file_path: str) -> Dict[str, Any]:
        """
        Get code metrics for a specific file

        Args:
            file_path: Path to file

        Returns:
            Metrics including LOC, complexity, etc.
        """
        # Validate file was analyzed
        try:
            self._validate_file_access(file_path)
        except Exception as e:
            return {"error": str(e)}

        # Get cached metrics from analysis results
        metrics = self.analysis_results.get('metrics', {})

        for file_metric in metrics.get('by_file', []):
            if self._normalize_path(file_metric['file']) == self._normalize_path(file_path):
                return {
                    "file": file_path,
                    "loc": file_metric.get('loc', 0),
                    "sloc": file_metric.get('sloc', 0),
                    "comments": file_metric.get('comments', 0),
                    "blank": file_metric.get('blank', 0),
                    "complexity": file_metric.get('complexity', 0),
                    "complexity_rating": file_metric.get('complexity_rating', 'Unknown'),
                    "functions": file_metric.get('functions', 0)
                }

        return {"error": f"Metrics not found for {file_path}"}

    async def get_codebase_summary(self) -> Dict[str, Any]:
        """
        Get overall codebase statistics

        Returns:
            Summary with file counts, LOC totals, etc.
        """
        summary = self.analysis_results.get('summary', {})
        metrics = self.analysis_results.get('metrics', {})

        return {
            "total_files": summary.get('total_files', 0),
            "total_size_mb": summary.get('total_size_mb', 0),
            "file_types": summary.get('file_types', {}),
            "total_loc": metrics.get('total_loc', 0),
            "total_sloc": metrics.get('total_sloc', 0),
            "total_comments": metrics.get('total_comments', 0),
            "total_blank": metrics.get('total_blank', 0),
            "business_rules_count": len(self.analysis_results.get('business_rules', [])),
            "database_operations_count": self.analysis_results.get('database_operations', {}).get('total_count', 0),
            "dependencies_count": len(self.analysis_results.get('dependencies', {}))
        }

    async def query_database_operations(
        self,
        operation_type: Optional[str] = None,
        file_path: Optional[str] = None,
        category: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Query database operations (SQL queries, DB calls)

        Args:
            operation_type: Filter by type (SELECT, INSERT, UPDATE, DELETE, etc.)
            file_path: Filter by specific file
            category: Filter by category (SQL, NoSQL, etc.)

        Returns:
            List of database operations matching criteria
        """
        db_ops = self.analysis_results.get('database_operations', {})
        queries = db_ops.get('queries', [])

        # Apply filters
        filtered = queries

        if operation_type:
            filtered = [q for q in filtered if q.get('type', '').upper() == operation_type.upper()]

        if file_path:
            normalized_path = self._normalize_path(file_path)
            filtered = [q for q in filtered
                       if self._normalize_path(q.get('file', '')) == normalized_path]

        if category:
            filtered = [q for q in filtered if q.get('category', '').upper() == category.upper()]

        return filtered

    async def get_business_rules(
        self,
        rule_type: Optional[str] = None,
        file_path: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Query business rules detected in codebase

        Args:
            rule_type: Filter by type (Financial Rule, Validation Rule, etc.)
            file_path: Filter by file

        Returns:
            List of business rules matching criteria
        """
        rules = self.analysis_results.get('business_rules', [])

        # Apply filters
        filtered = rules

        if rule_type:
            filtered = [r for r in filtered if r.get('type', '') == rule_type]

        if file_path:
            normalized_path = self._normalize_path(file_path)
            filtered = [r for r in filtered
                       if self._normalize_path(r.get('file', '')) == normalized_path]

        return filtered

    async def get_complexity_hotspots(
        self,
        min_complexity: int = 10,
        limit: int = 10
    ) -> List[Dict[str, Any]]:
        """
        Find files with highest complexity

        Args:
            min_complexity: Minimum complexity threshold
            limit: Maximum number of results

        Returns:
            List of files sorted by complexity (highest first)
        """
        metrics = self.analysis_results.get('metrics', {})
        files = metrics.get('by_file', [])

        # Filter by minimum complexity
        hotspots = [
            {
                'file': f['file'],
                'complexity': f.get('complexity', 0),
                'loc': f.get('loc', 0),
                'sloc': f.get('sloc', 0),
                'complexity_rating': f.get('complexity_rating', 'Unknown')
            }
            for f in files
            if f.get('complexity', 0) >= min_complexity
        ]

        # Sort by complexity (descending)
        hotspots.sort(key=lambda x: x['complexity'], reverse=True)

        return hotspots[:limit]

    async def get_files_by_type(self, file_type: str) -> List[Dict[str, Any]]:
        """
        Get all files of a specific type

        Args:
            file_type: File type (e.g., "Python", "Java", "SQL")

        Returns:
            List of files matching the type
        """
        files_info = self.analysis_results.get('files', [])

        matching = [
            {
                'path': f['path'],
                'type': f.get('type', 'Unknown'),
                'extension': f.get('extension', ''),
                'size': f.get('size', 0)
            }
            for f in files_info
            if f.get('type', '').lower() == file_type.lower()
        ]

        return matching

    async def get_database_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about database operations

        Returns:
            Statistics by operation type, file, etc.
        """
        db_ops = self.analysis_results.get('database_operations', {})

        queries = db_ops.get('queries', [])
        by_type = db_ops.get('by_type', {})

        # Calculate additional stats
        files_with_queries = set(q['file'] for q in queries)

        by_file = defaultdict(int)
        for q in queries:
            by_file[q['file']] += 1

        # Find files with most queries
        top_files = sorted(by_file.items(), key=lambda x: x[1], reverse=True)[:10]

        return {
            "total_operations": db_ops.get('total_count', 0),
            "by_type": by_type,
            "files_with_operations": len(files_with_queries),
            "top_files": [
                {"file": file, "count": count}
                for file, count in top_files
            ]
        }

    async def get_business_rule_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about business rules

        Returns:
            Statistics by rule type, file, etc.
        """
        rules = self.analysis_results.get('business_rules', [])

        # Count by type
        by_type = defaultdict(int)
        for rule in rules:
            by_type[rule.get('type', 'Unknown')] += 1

        # Count by file
        by_file = defaultdict(int)
        for rule in rules:
            by_file[rule['file']] += 1

        # Find files with most rules
        top_files = sorted(by_file.items(), key=lambda x: x[1], reverse=True)[:10]

        return {
            "total_rules": len(rules),
            "by_type": dict(by_type),
            "files_with_rules": len(by_file),
            "top_files": [
                {"file": file, "count": count}
                for file, count in top_files
            ]
        }

    async def find_duplicate_code_patterns(
        self,
        min_occurrences: int = 2
    ) -> List[Dict[str, Any]]:
        """
        Find potentially duplicate code patterns

        Args:
            min_occurrences: Minimum number of occurrences to report

        Returns:
            List of potential duplicates
        """
        # Look for similar business rules as a proxy for duplicate logic
        rules = self.analysis_results.get('business_rules', [])

        # Group by rule type
        by_type = defaultdict(list)
        for rule in rules:
            by_type[rule.get('type', 'Unknown')].append(rule)

        duplicates = []

        for rule_type, rule_list in by_type.items():
            if len(rule_list) >= min_occurrences:
                duplicates.append({
                    'pattern': rule_type,
                    'occurrences': len(rule_list),
                    'files': list(set(r['file'] for r in rule_list)),
                    'examples': [
                        {'file': r['file'], 'line': r['line'], 'code': r.get('code', '')[:100]}
                        for r in rule_list[:3]
                    ]
                })

        # Sort by occurrences
        duplicates.sort(key=lambda x: x['occurrences'], reverse=True)

        return duplicates

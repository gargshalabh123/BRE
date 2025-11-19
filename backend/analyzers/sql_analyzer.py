"""
SQL-specific code analyzer
Handles SQL scripts, stored procedures, triggers, and functions
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict


class SQLAnalyzer:
    """Specialized analyzer for SQL code"""

    def __init__(self):
        self.tables = []
        self.procedures = []
        self.functions = []
        self.triggers = []

    def analyze_file(self, file_path: Path) -> Dict[str, Any]:
        """Analyze a SQL script file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            return {
                'file': str(file_path),
                'language': 'SQL',
                'dialect': self._detect_dialect(content),
                'tables': self._extract_tables(content, lines),
                'procedures': self._extract_procedures(content, lines),
                'functions': self._extract_functions(content, lines),
                'triggers': self._extract_triggers(content, lines),
                'queries': self._extract_queries(content, lines),
                'business_rules': self._extract_business_rules(content, lines),
                'indexes': self._extract_indexes(content, lines),
                'constraints': self._extract_constraints(content, lines),
                'metrics': self._calculate_metrics(content, lines),
                'optimization_hints': self._get_optimization_hints(content)
            }
        except Exception as e:
            return {'error': str(e), 'file': str(file_path)}

    def _detect_dialect(self, content: str) -> str:
        """Detect SQL dialect"""
        dialects = {
            'Oracle': ['EXEC', 'EXECUTE IMMEDIATE', 'DBMS_OUTPUT', 'VARCHAR2', 'NUMBER(', 'ROWNUM'],
            'SQL Server': ['GO', 'EXEC sp_', 'IDENTITY(', 'GETDATE()', 'ISNULL('],
            'MySQL': ['AUTO_INCREMENT', 'ENGINE=InnoDB', 'LIMIT', 'CONCAT('],
            'PostgreSQL': ['SERIAL', 'RETURNING', 'PERFORM', '::'],
            'DB2': ['SYSIBM', 'FETCH FIRST', 'CURRENT TIMESTAMP'],
        }

        detected = []
        for dialect, indicators in dialects.items():
            if any(ind in content.upper() for ind in indicators):
                detected.append(dialect)

        return detected[0] if detected else 'Generic SQL'

    def _extract_tables(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract table definitions"""
        tables = []

        # CREATE TABLE pattern
        table_pattern = r'CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?([`\[]?\w+[`\]]?\.)?([`\[]?\w+[`\]]?)'

        for i, line in enumerate(lines, 1):
            if match := re.search(table_pattern, line, re.IGNORECASE):
                table_name = match.group(2).strip('`[]')

                # Try to extract columns (simplified)
                columns = []
                j = i
                while j < len(lines) and j < i + 50:  # Look ahead max 50 lines
                    col_match = re.search(r'^\s*([`\[]?\w+[`\]]?)\s+(VARCHAR|INT|DATE|DECIMAL|TEXT|BIGINT|CHAR|TIMESTAMP|BOOLEAN|NUMERIC)',
                                        lines[j], re.IGNORECASE)
                    if col_match:
                        columns.append(col_match.group(1).strip('`[]'))
                    if ';' in lines[j]:
                        break
                    j += 1

                tables.append({
                    'name': table_name,
                    'line': i,
                    'columns': columns
                })

        return tables

    def _extract_procedures(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract stored procedures"""
        procedures = []

        # CREATE PROCEDURE pattern
        proc_pattern = r'CREATE\s+(?:OR\s+REPLACE\s+)?PROCEDURE\s+([`\[]?\w+[`\]]?)'

        for i, line in enumerate(lines, 1):
            if match := re.search(proc_pattern, line, re.IGNORECASE):
                proc_name = match.group(1).strip('`[]')

                # Extract parameters
                params = []
                if '(' in line:
                    param_text = re.search(r'\((.*?)\)', line)
                    if param_text:
                        params = [p.strip() for p in param_text.group(1).split(',')]

                procedures.append({
                    'name': proc_name,
                    'line': i,
                    'parameters': params
                })

        return procedures

    def _extract_functions(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract user-defined functions"""
        functions = []

        # CREATE FUNCTION pattern
        func_pattern = r'CREATE\s+(?:OR\s+REPLACE\s+)?FUNCTION\s+([`\[]?\w+[`\]]?)'

        for i, line in enumerate(lines, 1):
            if match := re.search(func_pattern, line, re.IGNORECASE):
                func_name = match.group(1).strip('`[]')

                # Extract return type
                return_type = None
                return_match = re.search(r'RETURNS?\s+(\w+)', line, re.IGNORECASE)
                if return_match:
                    return_type = return_match.group(1)

                functions.append({
                    'name': func_name,
                    'line': i,
                    'return_type': return_type
                })

        return functions

    def _extract_triggers(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract database triggers"""
        triggers = []

        # CREATE TRIGGER pattern
        trigger_pattern = r'CREATE\s+(?:OR\s+REPLACE\s+)?TRIGGER\s+([`\[]?\w+[`\]]?)'

        for i, line in enumerate(lines, 1):
            if match := re.search(trigger_pattern, line, re.IGNORECASE):
                trigger_name = match.group(1).strip('`[]')

                # Determine trigger type
                trigger_type = 'UNKNOWN'
                if re.search(r'\bBEFORE\s+INSERT\b', line, re.IGNORECASE):
                    trigger_type = 'BEFORE INSERT'
                elif re.search(r'\bAFTER\s+INSERT\b', line, re.IGNORECASE):
                    trigger_type = 'AFTER INSERT'
                elif re.search(r'\bBEFORE\s+UPDATE\b', line, re.IGNORECASE):
                    trigger_type = 'BEFORE UPDATE'
                elif re.search(r'\bAFTER\s+UPDATE\b', line, re.IGNORECASE):
                    trigger_type = 'AFTER UPDATE'

                triggers.append({
                    'name': trigger_name,
                    'line': i,
                    'type': trigger_type
                })

        return triggers

    def _extract_queries(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract SQL queries"""
        queries = []

        query_types = {
            'SELECT': r'\bSELECT\b',
            'INSERT': r'\bINSERT\s+INTO\b',
            'UPDATE': r'\bUPDATE\b',
            'DELETE': r'\bDELETE\s+FROM\b',
            'MERGE': r'\bMERGE\s+INTO\b',
        }

        for i, line in enumerate(lines, 1):
            for qtype, pattern in query_types.items():
                if re.search(pattern, line, re.IGNORECASE):
                    queries.append({
                        'type': qtype,
                        'line': i,
                        'query': line.strip()[:200]
                    })
                    break  # Only count once per line

        return queries

    def _extract_business_rules(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract business rules from SQL"""
        rules = []

        patterns = [
            (r'\bCHECK\s*\(.*(?:amount|price|balance|total|rate)', 'Financial Constraint'),
            (r'\bCHECK\s*\(.*(?:date|timestamp|year)', 'Date Constraint'),
            (r'\bCASE\s+WHEN.*(?:status|state|type)', 'Status Logic'),
            (r'\bDEFAULT\s+', 'Default Value Rule'),
            (r'\bFOREIGN\s+KEY\b', 'Referential Integrity'),
            (r'\bUNIQUE\b', 'Uniqueness Constraint'),
            (r'\bNOT\s+NULL\b', 'Required Field'),
            (r'\b(?:MIN|MAX)\s*\(.*\)', 'Aggregation Rule'),
        ]

        for i, line in enumerate(lines, 1):
            for pattern, rule_type in patterns:
                if re.search(pattern, line, re.IGNORECASE):
                    rules.append({
                        'type': rule_type,
                        'line': i,
                        'code': line.strip()[:150]
                    })

        return rules

    def _extract_indexes(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract index definitions"""
        indexes = []

        index_pattern = r'CREATE\s+(?:UNIQUE\s+)?INDEX\s+([`\[]?\w+[`\]]?)\s+ON\s+([`\[]?\w+[`\]]?)'

        for i, line in enumerate(lines, 1):
            if match := re.search(index_pattern, line, re.IGNORECASE):
                indexes.append({
                    'name': match.group(1).strip('`[]'),
                    'table': match.group(2).strip('`[]'),
                    'unique': 'UNIQUE' in line.upper(),
                    'line': i
                })

        return indexes

    def _extract_constraints(self, content: str, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract constraints"""
        constraints = []

        constraint_patterns = {
            'PRIMARY KEY': r'\bPRIMARY\s+KEY\b',
            'FOREIGN KEY': r'\bFOREIGN\s+KEY\b',
            'UNIQUE': r'\bUNIQUE\b',
            'CHECK': r'\bCHECK\s*\(',
            'NOT NULL': r'\bNOT\s+NULL\b',
        }

        for i, line in enumerate(lines, 1):
            for ctype, pattern in constraint_patterns.items():
                if re.search(pattern, line, re.IGNORECASE):
                    constraints.append({
                        'type': ctype,
                        'line': i,
                        'definition': line.strip()[:200]
                    })

        return constraints

    def _calculate_metrics(self, content: str, lines: List[str]) -> Dict[str, Any]:
        """Calculate SQL-specific metrics"""
        metrics = {
            'total_lines': len(lines),
            'code_lines': 0,
            'comment_lines': 0,
            'blank_lines': 0,
            'select_count': len(re.findall(r'\bSELECT\b', content, re.IGNORECASE)),
            'insert_count': len(re.findall(r'\bINSERT\b', content, re.IGNORECASE)),
            'update_count': len(re.findall(r'\bUPDATE\b', content, re.IGNORECASE)),
            'delete_count': len(re.findall(r'\bDELETE\b', content, re.IGNORECASE)),
            'join_count': len(re.findall(r'\bJOIN\b', content, re.IGNORECASE)),
            'subquery_count': content.count('(SELECT'),
        }

        in_multiline_comment = False

        for line in lines:
            stripped = line.strip()

            if not stripped:
                metrics['blank_lines'] += 1
            elif stripped.startswith('--'):
                metrics['comment_lines'] += 1
            elif '/*' in stripped:
                metrics['comment_lines'] += 1
                in_multiline_comment = True
            elif in_multiline_comment:
                metrics['comment_lines'] += 1
                if '*/' in stripped:
                    in_multiline_comment = False
            else:
                metrics['code_lines'] += 1

        return metrics

    def _get_optimization_hints(self, content: str) -> List[Dict[str, str]]:
        """Provide SQL optimization hints"""
        hints = []

        # SELECT *
        if re.search(r'SELECT\s+\*\s+FROM', content, re.IGNORECASE):
            hints.append({
                'type': 'Performance',
                'issue': 'SELECT * found',
                'recommendation': 'Specify explicit column names instead of SELECT *'
            })

        # Missing WHERE clause
        update_without_where = re.findall(r'UPDATE\s+\w+\s+SET.*?(?:;|$)(?!.*WHERE)', content, re.IGNORECASE | re.DOTALL)
        if update_without_where:
            hints.append({
                'type': 'Safety',
                'issue': 'UPDATE without WHERE clause',
                'recommendation': 'Always use WHERE clause with UPDATE to avoid updating all rows'
            })

        # Implicit JOIN
        if re.search(r'FROM\s+\w+\s*,\s*\w+', content, re.IGNORECASE):
            hints.append({
                'type': 'Readability',
                'issue': 'Implicit JOIN syntax (comma-separated tables)',
                'recommendation': 'Use explicit JOIN syntax for better readability'
            })

        # OR in WHERE clause
        if len(re.findall(r'WHERE.*?\bOR\b', content, re.IGNORECASE)) > 3:
            hints.append({
                'type': 'Performance',
                'issue': 'Multiple OR conditions in WHERE clause',
                'recommendation': 'Consider using IN clause or UNION for better performance'
            })

        # Subqueries in SELECT
        if content.count('(SELECT') > 5:
            hints.append({
                'type': 'Performance',
                'issue': 'Many subqueries detected',
                'recommendation': 'Consider using JOINs or CTEs (WITH clause) instead'
            })

        return hints

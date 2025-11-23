"""
COBOL-specific code analyzer
Handles COBOL/COBOL II/COBOL 85 legacy code analysis
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict
from .base_analyzer import BaseFileAnalyzer


class COBOLAnalyzer(BaseFileAnalyzer):
    """Specialized analyzer for COBOL code"""

    def __init__(self):
        self.divisions = {
            'IDENTIFICATION': [],
            'ENVIRONMENT': [],
            'DATA': [],
            'PROCEDURE': []
        }
        self.copybooks = []
        self.paragraphs = []
        self.sections = []
        self.file_definitions = []
        self.database_calls = []
        self.business_rules = []

    def can_analyze(self, file_path: Path) -> bool:
        """Check if this analyzer can handle the file"""
        return file_path.suffix.lower() in ['.cbl', '.cob', '.cobol']

    def get_file_types(self) -> List[str]:
        """Get supported file types"""
        return ['.cbl', '.cob', '.cobol']

    def get_analyzer_name(self) -> str:
        """Get analyzer name"""
        return 'COBOL'

    def get_priority(self) -> int:
        """Get analyzer priority"""
        return 1

    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Analyze a COBOL source file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # Extract COBOL-specific data
            data = {
                'divisions': self._extract_divisions(lines),
                'copybooks': self._extract_copybooks(lines),
                'paragraphs': self._extract_paragraphs(lines),
                'sections': self._extract_sections(lines),
                'file_io': self._extract_file_operations(lines),
                'database': self._extract_database_operations(lines),
                'business_rules': self._extract_business_rules(lines),
                'data_items': self._extract_data_items(lines),
                'metrics': self._calculate_metrics(lines),
                'modernization_hints': self._get_modernization_hints(lines)
            }

            # Return standardized result
            return self.create_result(
                file_path=file_path,
                file_type='COBOL Program',
                language='COBOL',
                data=data
            )
        except Exception as e:
            return self.create_result(
                file_path=file_path,
                file_type='COBOL Program',
                language='COBOL',
                data={},
                error=str(e)
            )

    def _extract_divisions(self, lines: List[str]) -> Dict[str, int]:
        """Extract COBOL division information"""
        divisions = {
            'IDENTIFICATION DIVISION': 0,
            'ENVIRONMENT DIVISION': 0,
            'DATA DIVISION': 0,
            'PROCEDURE DIVISION': 0
        }

        for line in lines:
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()
            for div in divisions.keys():
                if div in clean_line.upper():
                    divisions[div] += 1

        return divisions

    def _extract_copybooks(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract COPY statements (similar to includes/imports)"""
        copybooks = []

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # COPY bookname
            match = re.search(r'\bCOPY\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if match:
                copybooks.append({
                    'name': match.group(1),
                    'line': i,
                    'type': 'COPYBOOK'
                })

        return copybooks

    def _extract_paragraphs(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract paragraph names (COBOL subroutines)"""
        paragraphs = []
        in_procedure_division = False

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            if 'PROCEDURE DIVISION' in clean_line.upper():
                in_procedure_division = True
                continue

            if in_procedure_division and clean_line:
                # Paragraph name ends with a period and is not a statement
                if (clean_line.endswith('.') and
                    not any(keyword in clean_line.upper() for keyword in
                           ['MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
                            'IF', 'PERFORM', 'DISPLAY', 'ACCEPT', 'COMPUTE'])):
                    para_name = clean_line.rstrip('.')
                    if para_name and not para_name.startswith('*'):
                        paragraphs.append({
                            'name': para_name,
                            'line': i
                        })

        return paragraphs

    def _extract_sections(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract COBOL sections"""
        sections = []

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            if ' SECTION' in clean_line.upper():
                section_name = clean_line.replace('SECTION', '').strip().rstrip('.')
                sections.append({
                    'name': section_name,
                    'line': i
                })

        return sections

    def _extract_file_operations(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract file I/O operations"""
        operations = []
        file_operations = ['OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 'DELETE']

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            for op in file_operations:
                if re.search(rf'\b{op}\b', clean_line, re.IGNORECASE):
                    operations.append({
                        'operation': op,
                        'line': i,
                        'code': clean_line[:100]
                    })

        return operations

    def _extract_database_operations(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract database operations (EXEC SQL, EXEC CICS, etc.)"""
        db_operations = []
        in_sql_block = False
        sql_buffer = []
        sql_start_line = 0

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # EXEC SQL
            if 'EXEC SQL' in clean_line.upper():
                in_sql_block = True
                sql_start_line = i
                sql_buffer = [clean_line]
                continue

            if in_sql_block:
                sql_buffer.append(clean_line)
                if 'END-EXEC' in clean_line.upper():
                    sql_statement = ' '.join(sql_buffer)

                    # Determine SQL type
                    sql_type = 'UNKNOWN'
                    for cmd in ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'DECLARE', 'OPEN', 'FETCH', 'CLOSE']:
                        if cmd in sql_statement.upper():
                            sql_type = cmd
                            break

                    db_operations.append({
                        'type': sql_type,
                        'statement': sql_statement[:200],
                        'line': sql_start_line,
                        'category': 'EMBEDDED SQL'
                    })
                    in_sql_block = False
                    sql_buffer = []

            # EXEC CICS
            if 'EXEC CICS' in clean_line.upper():
                db_operations.append({
                    'type': 'CICS',
                    'statement': clean_line[:200],
                    'line': i,
                    'category': 'CICS COMMAND'
                })

        return db_operations

    def _extract_business_rules(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract business rules specific to COBOL"""
        rules = []

        # COBOL-specific business rule patterns
        patterns = [
            (r'\bIF\s+.*(?:AMOUNT|BALANCE|RATE|PRICE|TOTAL)', 'Financial Calculation'),
            (r'\bIF\s+.*(?:DATE|YEAR|MONTH|DAY|AGE)', 'Date/Time Rule'),
            (r'\bIF\s+.*(?:STATUS|CODE|FLAG|SWITCH)', 'Status Check'),
            (r'\bCOMPUTE\s+.*(?:INTEREST|TAX|DISCOUNT|FEE)', 'Business Calculation'),
            (r'\bEVALUATE\s+(?:TRUE|.*WHEN)', 'Decision Logic'),
            (r'88\s+[A-Z0-9\-]+\s+VALUE', 'Condition Name (88-level)'),
            (r'\bPERFORM\s+.*(?:VALIDATE|CHECK|VERIFY)', 'Validation Rule'),
            (r'\b(?:ADD|SUBTRACT|MULTIPLY|DIVIDE).*(?:GIVING|TO)', 'Arithmetic Operation'),
        ]

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            for pattern, rule_type in patterns:
                if re.search(pattern, clean_line, re.IGNORECASE):
                    rules.append({
                        'type': rule_type,
                        'line': i,
                        'code': clean_line[:150]
                    })

        return rules

    def _extract_data_items(self, lines: List[str]) -> Dict[str, Any]:
        """Extract data item definitions from DATA DIVISION"""
        data_items = {
            'total_items': 0,
            'level_01': 0,
            'level_77': 0,
            'level_88': 0,
            'pic_clauses': [],
            'value_clauses': 0
        }

        in_data_division = False

        for line in lines:
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            if 'DATA DIVISION' in clean_line.upper():
                in_data_division = True
                continue

            if 'PROCEDURE DIVISION' in clean_line.upper():
                in_data_division = False

            if in_data_division:
                # Count level numbers
                if re.match(r'^\s*01\s+', clean_line):
                    data_items['level_01'] += 1
                    data_items['total_items'] += 1
                elif re.match(r'^\s*77\s+', clean_line):
                    data_items['level_77'] += 1
                    data_items['total_items'] += 1
                elif re.match(r'^\s*88\s+', clean_line):
                    data_items['level_88'] += 1

                # Count PIC clauses
                if 'PIC ' in clean_line.upper() or 'PICTURE ' in clean_line.upper():
                    pic_match = re.search(r'PIC(?:TURE)?\s+([A-Z0-9\(\)]+)', clean_line, re.IGNORECASE)
                    if pic_match:
                        data_items['pic_clauses'].append(pic_match.group(1))

                # Count VALUE clauses
                if 'VALUE ' in clean_line.upper():
                    data_items['value_clauses'] += 1

        return data_items

    def _calculate_metrics(self, lines: List[str]) -> Dict[str, Any]:
        """Calculate COBOL-specific metrics"""
        total_lines = len(lines)
        comment_lines = 0
        blank_lines = 0
        code_lines = 0

        for line in lines:
            if not line.strip():
                blank_lines += 1
            elif len(line) >= 7 and line[6] == '*':
                comment_lines += 1
            elif line.strip().startswith('*'):
                comment_lines += 1
            else:
                code_lines += 1

        return {
            'total_lines': total_lines,
            'code_lines': code_lines,
            'comment_lines': comment_lines,
            'blank_lines': blank_lines,
            'code_percentage': round((code_lines / total_lines * 100) if total_lines > 0 else 0, 2)
        }

    def _get_modernization_hints(self, lines: List[str]) -> List[Dict[str, str]]:
        """Identify modernization opportunities"""
        hints = []
        content = '\n'.join(lines).upper()

        # Check for outdated features
        if 'GOTO' in content:
            hints.append({
                'type': 'Code Smell',
                'issue': 'GOTO statements found',
                'recommendation': 'Replace with structured programming (PERFORM, IF-THEN-ELSE)'
            })

        if 'ALTER' in content:
            hints.append({
                'type': 'Deprecated Feature',
                'issue': 'ALTER statement found',
                'recommendation': 'Remove ALTER statements, use structured logic'
            })

        if 'EXEC CICS' in content:
            hints.append({
                'type': 'Modernization',
                'issue': 'CICS commands detected',
                'recommendation': 'Consider migrating to modern middleware (REST APIs, microservices)'
            })

        if 'EXEC SQL' in content:
            hints.append({
                'type': 'Database',
                'issue': 'Embedded SQL found',
                'recommendation': 'Consider extracting SQL to stored procedures or ORM layer'
            })

        return hints

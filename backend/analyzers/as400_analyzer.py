"""
AS/400 RPG/RPGLE/RPGIV code analyzer
Handles RPG III, RPG IV (ILE RPG), and RPGLE legacy code analysis
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict


class AS400Analyzer:
    """Specialized analyzer for AS/400 RPG code"""

    def __init__(self):
        self.procedures = []
        self.subroutines = []
        self.file_definitions = []
        self.database_operations = []
        self.business_rules = []

    def analyze_file(self, file_path: Path) -> Dict[str, Any]:
        """Analyze an RPG/RPGLE source file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # Determine RPG format
            rpg_format = self._determine_rpg_format(lines)

            return {
                'file': str(file_path),
                'language': 'AS400/RPG',
                'format': rpg_format,
                'procedures': self._extract_procedures(lines, rpg_format),
                'subroutines': self._extract_subroutines(lines, rpg_format),
                'file_definitions': self._extract_file_definitions(lines, rpg_format),
                'database_operations': self._extract_database_operations(lines, rpg_format),
                'business_rules': self._extract_business_rules(lines, rpg_format),
                'data_structures': self._extract_data_structures(lines, rpg_format),
                'indicators': self._extract_indicators(lines, rpg_format),
                'metrics': self._calculate_metrics(lines, rpg_format),
                'modernization_hints': self._get_modernization_hints(lines, rpg_format)
            }
        except Exception as e:
            return {'error': str(e), 'file': str(file_path)}

    def _determine_rpg_format(self, lines: List[str]) -> str:
        """Determine if this is RPG III, RPG IV, or free-format RPGLE"""
        for line in lines[:50]:  # Check first 50 lines
            # Free-format RPGLE indicators
            if line.strip().upper().startswith(('DCL-', 'BEGSR', 'ENDSR', 'DCL-PROC', 'END-PROC')):
                return 'RPGLE Free-Format'

            # Check for control specification with NOMAIN or other RPG IV features
            if len(line) >= 6 and line[5] == 'H' and 'NOMAIN' in line.upper():
                return 'RPG IV (ILE RPG)'

            # Fixed-format with /FREE directive
            if '/FREE' in line.upper():
                return 'RPG IV Mixed-Format'

        # Default to RPG III fixed format
        return 'RPG III Fixed-Format'

    def _extract_procedures(self, lines: List[str], rpg_format: str) -> List[Dict[str, Any]]:
        """Extract procedure definitions"""
        procedures = []

        for i, line in enumerate(lines, 1):
            # Free-format: DCL-PROC
            if 'DCL-PROC' in line.upper():
                match = re.search(r'DCL-PROC\s+([A-Z0-9_]+)', line, re.IGNORECASE)
                if match:
                    procedures.append({
                        'name': match.group(1),
                        'line': i,
                        'type': 'PROCEDURE',
                        'format': 'free'
                    })

            # Fixed-format: P spec
            elif len(line) >= 6 and line[5].upper() == 'P':
                # Extract procedure name from columns 7-21
                proc_name = line[6:21].strip()
                if proc_name and line[24].upper() == 'B':  # Beginning of procedure
                    procedures.append({
                        'name': proc_name,
                        'line': i,
                        'type': 'PROCEDURE',
                        'format': 'fixed'
                    })

        return procedures

    def _extract_subroutines(self, lines: List[str], rpg_format: str) -> List[Dict[str, Any]]:
        """Extract subroutine definitions"""
        subroutines = []

        for i, line in enumerate(lines, 1):
            # Free-format: BEGSR
            if line.strip().upper().startswith('BEGSR'):
                match = re.search(r'BEGSR\s+([A-Z0-9_]+)', line, re.IGNORECASE)
                if match:
                    subroutines.append({
                        'name': match.group(1),
                        'line': i,
                        'format': 'free'
                    })

            # Fixed-format: C spec with BEGSR
            elif len(line) >= 6 and line[5].upper() == 'C':
                if 'BEGSR' in line[6:].upper():
                    # Extract subroutine name
                    match = re.search(r'BEGSR\s+([A-Z0-9_]+)', line[6:], re.IGNORECASE)
                    if match:
                        subroutines.append({
                            'name': match.group(1),
                            'line': i,
                            'format': 'fixed'
                        })

        return subroutines

    def _extract_file_definitions(self, lines: List[str], rpg_format: str) -> List[Dict[str, Any]]:
        """Extract file definitions (database files, display files, etc.)"""
        files = []

        for i, line in enumerate(lines, 1):
            # Free-format: DCL-F
            if 'DCL-F' in line.upper():
                match = re.search(r'DCL-F\s+([A-Z0-9_]+)', line, re.IGNORECASE)
                if match:
                    file_type = 'DATABASE' if 'DISK' in line.upper() else 'DISPLAY' if 'WORKSTN' in line.upper() else 'PRINTER' if 'PRINTER' in line.upper() else 'UNKNOWN'
                    files.append({
                        'name': match.group(1),
                        'line': i,
                        'type': file_type,
                        'format': 'free'
                    })

            # Fixed-format: F spec
            elif len(line) >= 6 and line[5].upper() == 'F':
                file_name = line[6:21].strip()
                if file_name:
                    # Determine file type from position 17 (device type)
                    device = line[35:42].strip().upper() if len(line) > 42 else ''
                    file_type = 'DATABASE' if 'DISK' in device else 'DISPLAY' if 'WORKSTN' in device else 'PRINTER' if 'PRINTER' in device else 'UNKNOWN'

                    files.append({
                        'name': file_name,
                        'line': i,
                        'type': file_type,
                        'format': 'fixed'
                    })

        return files

    def _extract_database_operations(self, lines: List[str], rpg_format: str) -> List[Dict[str, Any]]:
        """Extract database I/O operations"""
        operations = []
        db_opcodes = ['READ', 'READE', 'READP', 'READPE', 'CHAIN', 'SETLL', 'SETGT', 'WRITE', 'UPDATE', 'DELETE', 'EXFMT']

        for i, line in enumerate(lines, 1):
            # Free-format
            for opcode in db_opcodes:
                if re.search(rf'\b{opcode}\b', line, re.IGNORECASE):
                    operations.append({
                        'operation': opcode,
                        'line': i,
                        'code': line.strip()[:100]
                    })
                    break

            # Fixed-format: C spec operations
            if len(line) >= 6 and line[5].upper() == 'C':
                opcode_field = line[26:36].strip().upper()
                if opcode_field in db_opcodes:
                    operations.append({
                        'operation': opcode_field,
                        'line': i,
                        'code': line[6:].strip()[:100]
                    })

        return operations

    def _extract_business_rules(self, lines: List[str], rpg_format: str) -> List[Dict[str, Any]]:
        """Extract business rules specific to RPG"""
        rules = []

        # RPG-specific business rule patterns
        patterns = [
            (r'\b(?:IF|WHEN)\s+.*(?:AMOUNT|BALANCE|RATE|PRICE|TOTAL|AMT)', 'Financial Calculation'),
            (r'\b(?:IF|WHEN)\s+.*(?:DATE|YEAR|MONTH|DAY|AGE)', 'Date/Time Rule'),
            (r'\b(?:IF|WHEN)\s+.*(?:STATUS|CODE|FLAG|IND)', 'Status Check'),
            (r'\bEVAL\s+.*(?:INTEREST|TAX|DISCOUNT|FEE)', 'Business Calculation'),
            (r'\bSELECT\s*;', 'Decision Logic'),
            (r'\b(?:ADD|SUB|MULT|DIV|Z-ADD|Z-SUB)', 'Arithmetic Operation'),
            (r'\bDO[UW]?\s+', 'Loop Control'),
        ]

        for i, line in enumerate(lines, 1):
            clean_line = line.strip()

            # Skip comment lines
            if clean_line.startswith('*') or (len(line) >= 7 and line[6] == '*'):
                continue

            for pattern, rule_type in patterns:
                if re.search(pattern, clean_line, re.IGNORECASE):
                    rules.append({
                        'type': rule_type,
                        'line': i,
                        'code': clean_line[:150]
                    })
                    break

        return rules

    def _extract_data_structures(self, lines: List[str], rpg_format: str) -> Dict[str, Any]:
        """Extract data structure definitions"""
        data_structures = {
            'total_ds': 0,
            'standalone_fields': 0,
            'data_structures': []
        }

        for i, line in enumerate(lines, 1):
            # Free-format: DCL-DS
            if 'DCL-DS' in line.upper():
                match = re.search(r'DCL-DS\s+([A-Z0-9_]+)', line, re.IGNORECASE)
                if match:
                    data_structures['total_ds'] += 1
                    data_structures['data_structures'].append({
                        'name': match.group(1),
                        'line': i,
                        'format': 'free'
                    })

            # Fixed-format: D spec
            elif len(line) >= 6 and line[5].upper() == 'D':
                ds_name = line[6:21].strip()
                if ds_name:
                    # Check if it's a data structure (DS keyword)
                    if 'DS' in line[24:44].upper():
                        data_structures['total_ds'] += 1
                        data_structures['data_structures'].append({
                            'name': ds_name,
                            'line': i,
                            'format': 'fixed'
                        })
                    else:
                        data_structures['standalone_fields'] += 1

        return data_structures

    def _extract_indicators(self, lines: List[str], rpg_format: str) -> Dict[str, Any]:
        """Extract indicator usage (RPG III/IV legacy feature)"""
        indicators = {
            'total_indicators_used': 0,
            'indicator_list': set()
        }

        for line in lines:
            # Look for *IN or *INxx patterns
            matches = re.findall(r'\*IN\d{2}', line, re.IGNORECASE)
            for match in matches:
                indicators['indicator_list'].add(match.upper())

            # Fixed-format indicator columns (positions 9-17 for conditioning, 54-59 for resulting)
            if len(line) >= 6 and line[5].upper() == 'C':
                # Check conditioning indicators (positions 9-17)
                if len(line) >= 17:
                    for pos in [8, 10, 12]:  # Positions for indicators
                        if len(line) > pos and line[pos].isdigit():
                            indicators['indicator_list'].add(f"*IN{line[pos:pos+2]}")

        indicators['total_indicators_used'] = len(indicators['indicator_list'])
        indicators['indicator_list'] = sorted(list(indicators['indicator_list']))

        return indicators

    def _calculate_metrics(self, lines: List[str], rpg_format: str) -> Dict[str, Any]:
        """Calculate RPG-specific metrics"""
        total_lines = len(lines)
        comment_lines = 0
        blank_lines = 0
        code_lines = 0

        for line in lines:
            if not line.strip():
                blank_lines += 1
            elif line.strip().startswith('*') or line.strip().startswith('//'):
                comment_lines += 1
            elif len(line) >= 7 and line[6] == '*':
                comment_lines += 1
            else:
                code_lines += 1

        return {
            'total_lines': total_lines,
            'code_lines': code_lines,
            'comment_lines': comment_lines,
            'blank_lines': blank_lines,
            'code_percentage': round((code_lines / total_lines * 100) if total_lines > 0 else 0, 2),
            'format': rpg_format
        }

    def _get_modernization_hints(self, lines: List[str], rpg_format: str) -> List[Dict[str, str]]:
        """Identify modernization opportunities"""
        hints = []
        content = '\n'.join(lines).upper()

        # Check for outdated features
        if rpg_format == 'RPG III Fixed-Format':
            hints.append({
                'type': 'Modernization',
                'issue': 'RPG III fixed-format code',
                'recommendation': 'Consider migrating to RPG IV or free-format RPGLE'
            })

        if '*IN' in content and rpg_format != 'RPGLE Free-Format':
            hints.append({
                'type': 'Code Smell',
                'issue': 'Indicator usage found',
                'recommendation': 'Replace indicators with boolean variables for better readability'
            })

        if re.search(r'\b(?:GOTO|TAG)\b', content):
            hints.append({
                'type': 'Code Smell',
                'issue': 'GOTO statements found',
                'recommendation': 'Replace with structured programming (DOW, DOU, IF-THEN-ELSE)'
            })

        if re.search(r'\b(?:Z-ADD|Z-SUB)\b', content):
            hints.append({
                'type': 'Deprecated Feature',
                'issue': 'Z-ADD/Z-SUB operations found',
                'recommendation': 'Replace with EVAL or modern assignment syntax'
            })

        if 'CHAIN' in content or 'SETLL' in content:
            hints.append({
                'type': 'Database',
                'issue': 'Native file I/O detected',
                'recommendation': 'Consider using SQL for better portability and performance'
            })

        if rpg_format == 'RPG IV Mixed-Format':
            hints.append({
                'type': 'Modernization',
                'issue': 'Mixed fixed/free format code',
                'recommendation': 'Consider converting entirely to free-format for consistency'
            })

        return hints

"""
Copybook Analyzer
Handles detailed analysis of COBOL copybook structures
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from .base_analyzer import BaseFileAnalyzer


class CopybookAnalyzer(BaseFileAnalyzer):
    """Specialized analyzer for COBOL copybooks"""

    def can_analyze(self, file_path: Path) -> bool:
        """Check if this analyzer can handle the file"""
        return file_path.suffix.lower() == '.cpy'

    def get_file_types(self) -> List[str]:
        """Get supported file types"""
        return ['.cpy']

    def get_analyzer_name(self) -> str:
        """Get analyzer name"""
        return 'Copybook'

    def get_priority(self) -> int:
        """Get analyzer priority"""
        return 4

    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Analyze a copybook file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # Extract copybook-specific data
            fields = self._extract_field_hierarchy(lines)
            redefines = self._extract_redefines(fields)
            constants = self._extract_constants(fields)
            arrays = self._extract_arrays(fields)
            includes = self._extract_includes(lines)
            comp_fields = self._extract_comp_fields(fields)

            # Calculate statistics
            max_level = max([f['level_number'] for f in fields]) if fields else 0
            total_redefines = len(redefines)
            total_occurs = len(arrays)
            total_comp = len(comp_fields)
            total_88_levels = len([f for f in fields if f['level_number'] == 88])
            estimated_length = self._calculate_record_length(fields)

            data = {
                'fields': fields,
                'redefines': redefines,
                'constants': constants,
                'arrays': arrays,
                'includes': includes,
                'comp_fields': comp_fields,
                'statistics': {
                    'total_fields': len(fields),
                    'max_level': max_level,
                    'total_redefines': total_redefines,
                    'total_occurs': total_occurs,
                    'total_comp_fields': total_comp,
                    'total_88_levels': total_88_levels,
                    'estimated_record_length': estimated_length
                }
            }

            # Return standardized result
            return self.create_result(
                file_path=file_path,
                file_type='Copybook',
                language='COBOL',
                data=data
            )
        except Exception as e:
            return self.create_result(
                file_path=file_path,
                file_type='Copybook',
                language='COBOL',
                data={},
                error=str(e)
            )

    def _extract_field_hierarchy(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract field hierarchy with level numbers"""
        fields = []
        field_order = 0

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # Skip comments and blank lines
            if not clean_line or clean_line.startswith('*'):
                continue

            # Look for level numbers (01-49, 66, 77, 88)
            level_match = re.match(r'^(\d{1,2})\s+(.+)$', clean_line)
            if level_match:
                level_num = int(level_match.group(1))
                rest = level_match.group(2).strip()

                # Skip invalid level numbers
                if level_num > 49 and level_num not in [66, 77, 88]:
                    continue

                field_order += 1
                field = {
                    'line': i,
                    'level_number': level_num,
                    'field_name': None,
                    'pic_clause': None,
                    'usage_clause': None,
                    'value_clause': None,
                    'occurs_count': None,
                    'occurs_depending': None,
                    'redefines_field': None,
                    'justified': None,
                    'blank_when_zero': False,
                    'sign_clause': None,
                    'synchronized': False,
                    'indexed_by': None,
                    'field_order': field_order,
                    'computed_length': 0,
                    'raw': clean_line
                }

                # Extract field name (first word after level)
                parts = rest.split()
                if parts:
                    field['field_name'] = parts[0]

                # Extract PIC clause
                pic_match = re.search(r'PIC(?:TURE)?\s+IS\s+([A-Z0-9\(\)V\+\-\$,\.]+)', rest, re.IGNORECASE)
                if not pic_match:
                    pic_match = re.search(r'PIC(?:TURE)?\s+([A-Z0-9\(\)V\+\-\$,\.]+)', rest, re.IGNORECASE)
                if pic_match:
                    field['pic_clause'] = pic_match.group(1).upper()
                    field['computed_length'] = self._calculate_pic_length(field['pic_clause'])

                # Extract USAGE clause
                usage_match = re.search(r'USAGE\s+IS\s+(\S+)', rest, re.IGNORECASE)
                if not usage_match:
                    usage_match = re.search(r'USAGE\s+(\S+)', rest, re.IGNORECASE)
                if usage_match:
                    field['usage_clause'] = usage_match.group(1).upper()
                else:
                    # Check for shorthand (COMP, COMP-1, etc.)
                    for comp_type in ['COMP-5', 'COMP-4', 'COMP-3', 'COMP-2', 'COMP-1', 'COMP', 'BINARY', 'PACKED-DECIMAL']:
                        if comp_type in rest.upper():
                            field['usage_clause'] = comp_type
                            break

                # Extract VALUE clause
                value_match = re.search(r"VALUE\s+IS\s+'([^']*)'", rest, re.IGNORECASE)
                if not value_match:
                    value_match = re.search(r"VALUE\s+'([^']*)'", rest, re.IGNORECASE)
                if not value_match:
                    value_match = re.search(r'VALUE\s+IS\s+(\S+)', rest, re.IGNORECASE)
                if not value_match:
                    value_match = re.search(r'VALUE\s+(\S+)', rest, re.IGNORECASE)
                if value_match:
                    field['value_clause'] = value_match.group(1)

                # Extract OCCURS clause
                occurs_match = re.search(r'OCCURS\s+(\d+)', rest, re.IGNORECASE)
                if occurs_match:
                    field['occurs_count'] = int(occurs_match.group(1))

                # Extract DEPENDING ON
                depending_match = re.search(r'DEPENDING\s+ON\s+([A-Z0-9\-]+)', rest, re.IGNORECASE)
                if depending_match:
                    field['occurs_depending'] = depending_match.group(1)

                # Extract REDEFINES
                redefines_match = re.search(r'REDEFINES\s+([A-Z0-9\-]+)', rest, re.IGNORECASE)
                if redefines_match:
                    field['redefines_field'] = redefines_match.group(1)

                # Extract JUSTIFIED
                if 'JUSTIFIED' in rest.upper() or 'JUST' in rest.upper():
                    if 'RIGHT' in rest.upper():
                        field['justified'] = 'RIGHT'
                    else:
                        field['justified'] = 'LEFT'

                # Extract BLANK WHEN ZERO
                if 'BLANK WHEN ZERO' in rest.upper():
                    field['blank_when_zero'] = True

                # Extract SIGN clause
                sign_match = re.search(r'SIGN\s+IS\s+(LEADING|TRAILING)(?:\s+SEPARATE)?', rest, re.IGNORECASE)
                if sign_match:
                    field['sign_clause'] = sign_match.group(0).upper()

                # Extract SYNCHRONIZED
                if 'SYNCHRONIZED' in rest.upper() or 'SYNC' in rest.upper():
                    field['synchronized'] = True

                # Extract INDEXED BY
                indexed_match = re.search(r'INDEXED\s+BY\s+([A-Z0-9\-]+)', rest, re.IGNORECASE)
                if indexed_match:
                    field['indexed_by'] = indexed_match.group(1)

                fields.append(field)

        return fields

    def _calculate_pic_length(self, pic_clause: str) -> int:
        """Calculate byte length from PIC clause"""
        if not pic_clause:
            return 0

        total_length = 0

        # Remove V (assumed decimal point - doesn't consume space)
        pic_clause = pic_clause.replace('V', '')

        # Handle repeated characters: X(10), 9(5), etc.
        for match in re.finditer(r'([A-Z\$\+\-,\.])(?:\((\d+)\))?', pic_clause):
            char = match.group(1)
            count = int(match.group(2)) if match.group(2) else 1

            # All display characters are 1 byte each
            total_length += count

        return total_length

    def _extract_redefines(self, fields: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract REDEFINES relationships"""
        redefines = []

        for field in fields:
            if field['redefines_field']:
                # Find the original field
                original = next((f for f in fields if f['field_name'] == field['redefines_field']), None)

                redefines.append({
                    'redefines_field_name': field['field_name'],
                    'original_field_name': field['redefines_field'],
                    'line': field['line'],
                    'original_found': original is not None
                })

        return redefines

    def _extract_constants(self, fields: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract VALUE clauses and 88-level conditions"""
        constants = []

        for field in fields:
            if field['level_number'] == 88:
                # 88-level condition name
                constants.append({
                    'constant_name': field['field_name'],
                    'constant_value': field['value_clause'],
                    'constant_type': '88_LEVEL',
                    'line': field['line']
                })
            elif field['value_clause'] and field['level_number'] in [1, 77]:
                # Top-level constant
                constants.append({
                    'constant_name': field['field_name'],
                    'constant_value': field['value_clause'],
                    'constant_type': 'VALUE_CLAUSE',
                    'line': field['line']
                })

        return constants

    def _extract_arrays(self, fields: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract OCCURS clauses (arrays)"""
        arrays = []

        for field in fields:
            if field['occurs_count']:
                arrays.append({
                    'array_name': field['field_name'],
                    'occurs_min': field['occurs_count'],
                    'occurs_max': field['occurs_count'],
                    'depending_on_field': field['occurs_depending'],
                    'indexed_by': field['indexed_by'],
                    'line': field['line']
                })

        return arrays

    def _extract_includes(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract COPY statements (nested copybooks)"""
        includes = []

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

            # COPY statement
            copy_match = re.search(r'\bCOPY\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if copy_match:
                copybook_name = copy_match.group(1)

                # Check for REPLACING clause
                replacing = None
                replacing_match = re.search(r'REPLACING\s+(.+)$', clean_line, re.IGNORECASE)
                if replacing_match:
                    replacing = replacing_match.group(1)

                includes.append({
                    'included_copybook_name': copybook_name,
                    'line': i,
                    'replacing_clause': replacing
                })

        return includes

    def _extract_comp_fields(self, fields: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract computational fields (COMP, COMP-3, etc.)"""
        comp_fields = []

        comp_descriptions = {
            'COMP': 'Binary',
            'COMP-1': 'Single-precision floating point',
            'COMP-2': 'Double-precision floating point',
            'COMP-3': 'Packed decimal',
            'COMP-4': 'Binary',
            'COMP-5': 'Native binary',
            'BINARY': 'Binary',
            'PACKED-DECIMAL': 'Packed decimal'
        }

        for field in fields:
            if field['usage_clause'] and field['usage_clause'] in comp_descriptions:
                comp_type = field['usage_clause']

                # Calculate bytes used
                bytes_used = self._calculate_comp_bytes(comp_type, field['pic_clause'])

                comp_fields.append({
                    'field_name': field['field_name'],
                    'comp_type': comp_type,
                    'pic_clause': field['pic_clause'],
                    'bytes_used': bytes_used,
                    'description': comp_descriptions[comp_type],
                    'line': field['line']
                })

        return comp_fields

    def _calculate_comp_bytes(self, comp_type: str, pic_clause: Optional[str]) -> int:
        """Calculate bytes used by computational fields"""
        if not pic_clause:
            return 0

        # Count digits in PIC clause
        digits = 0
        for match in re.finditer(r'9(?:\((\d+)\))?', pic_clause):
            count = int(match.group(1)) if match.group(1) else 1
            digits += count

        if comp_type == 'COMP-3' or comp_type == 'PACKED-DECIMAL':
            # Packed decimal: (digits / 2) + 1
            return (digits // 2) + 1
        elif comp_type in ['COMP', 'COMP-4', 'COMP-5', 'BINARY']:
            # Binary: based on digit count
            if digits <= 4:
                return 2  # Half-word
            elif digits <= 9:
                return 4  # Full-word
            else:
                return 8  # Double-word
        elif comp_type == 'COMP-1':
            return 4  # Single precision float
        elif comp_type == 'COMP-2':
            return 8  # Double precision float
        else:
            return 0

    def _calculate_record_length(self, fields: List[Dict[str, Any]]) -> int:
        """Estimate total record length"""
        # This is a simplified calculation
        # Real calculation would need to account for REDEFINES, hierarchy, etc.
        total = 0

        for field in fields:
            if field['level_number'] == 1:
                # Only count top-level fields to avoid double-counting
                if field['computed_length']:
                    total += field['computed_length']

        return total

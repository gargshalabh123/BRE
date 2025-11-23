"""
BMS (Basic Mapping Support) Analyzer
Handles analysis of CICS BMS screen definitions
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from .base_analyzer import BaseFileAnalyzer


class BMSAnalyzer(BaseFileAnalyzer):
    """Specialized analyzer for BMS mapsets"""

    def can_analyze(self, file_path: Path) -> bool:
        """Check if this analyzer can handle the file"""
        return file_path.suffix.lower() == '.bms'

    def get_file_types(self) -> List[str]:
        """Get supported file types"""
        return ['.bms']

    def get_analyzer_name(self) -> str:
        """Get analyzer name"""
        return 'BMS'

    def get_priority(self) -> int:
        """Get analyzer priority"""
        return 2

    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Analyze a BMS source file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # Extract BMS-specific data
            mapsets = self._extract_mapsets(lines)
            maps = self._extract_maps(lines)
            fields = self._extract_fields(lines)

            data = {
                'mapsets': mapsets,
                'maps': maps,
                'fields': fields,
                'statistics': {
                    'total_mapsets': len(mapsets),
                    'total_maps': len(maps),
                    'total_fields': len(fields),
                    'avg_fields_per_map': round(len(fields) / len(maps), 2) if maps else 0
                }
            }

            # Return standardized result
            return self.create_result(
                file_path=file_path,
                file_type='BMS Mapset',
                language='BMS',
                data=data
            )
        except Exception as e:
            return self.create_result(
                file_path=file_path,
                file_type='BMS Mapset',
                language='BMS',
                data={},
                error=str(e)
            )

    def _extract_mapsets(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract DFHMSD (Mapset Definition) macros"""
        mapsets = []

        for i, line in enumerate(lines, 1):
            clean_line = line.strip()

            # Look for DFHMSD macro
            if 'DFHMSD' in clean_line.upper():
                mapset = {
                    'line': i,
                    'name': None,
                    'type': 'DSECT',
                    'lang': None,
                    'mode': None,
                    'storage': None,
                    'ctrl': [],
                    'raw': clean_line
                }

                # Extract mapset name (first operand or TYPE= parameter)
                name_match = re.search(r'DFHMSD\s+TYPE=(\w+)', clean_line, re.IGNORECASE)
                if not name_match:
                    # Try to get name from label
                    parts = clean_line.split()
                    if len(parts) > 1 and parts[0].upper() != 'DFHMSD':
                        mapset['name'] = parts[0]

                # Extract TYPE parameter
                type_match = re.search(r'TYPE=(\w+)', clean_line, re.IGNORECASE)
                if type_match:
                    mapset['type'] = type_match.group(1).upper()

                # Extract LANG parameter
                lang_match = re.search(r'LANG=(\w+)', clean_line, re.IGNORECASE)
                if lang_match:
                    mapset['lang'] = lang_match.group(1).upper()

                # Extract MODE parameter
                mode_match = re.search(r'MODE=(\w+)', clean_line, re.IGNORECASE)
                if mode_match:
                    mapset['mode'] = mode_match.group(1).upper()

                # Extract STORAGE parameter
                storage_match = re.search(r'STORAGE=(\w+)', clean_line, re.IGNORECASE)
                if storage_match:
                    mapset['storage'] = storage_match.group(1).upper()

                # Extract CTRL parameters
                ctrl_match = re.search(r'CTRL=\(([^)]+)\)', clean_line, re.IGNORECASE)
                if ctrl_match:
                    mapset['ctrl'] = [c.strip() for c in ctrl_match.group(1).split(',')]

                mapsets.append(mapset)

        return mapsets

    def _extract_maps(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract DFHMDI (Map Definition) macros"""
        maps = []

        for i, line in enumerate(lines, 1):
            clean_line = line.strip()

            # Look for DFHMDI macro
            if 'DFHMDI' in clean_line.upper():
                map_def = {
                    'line': i,
                    'name': None,
                    'size': None,
                    'line_size': None,
                    'column_size': None,
                    'justify': None,
                    'raw': clean_line
                }

                # Extract map name (from label)
                parts = clean_line.split()
                if len(parts) > 1 and parts[0].upper() != 'DFHMDI':
                    map_def['name'] = parts[0]

                # Extract SIZE parameter
                size_match = re.search(r'SIZE=\((\d+),(\d+)\)', clean_line, re.IGNORECASE)
                if size_match:
                    map_def['line_size'] = int(size_match.group(1))
                    map_def['column_size'] = int(size_match.group(2))
                    map_def['size'] = f"{map_def['line_size']}x{map_def['column_size']}"

                # Extract JUSTIFY parameter
                justify_match = re.search(r'JUSTIFY=(\w+)', clean_line, re.IGNORECASE)
                if justify_match:
                    map_def['justify'] = justify_match.group(1).upper()

                maps.append(map_def)

        return maps

    def _extract_fields(self, lines: List[str]) -> List[Dict[str, Any]]:
        """Extract DFHMDF (Field Definition) macros"""
        fields = []

        for i, line in enumerate(lines, 1):
            clean_line = line.strip()

            # Look for DFHMDF macro
            if 'DFHMDF' in clean_line.upper():
                field = {
                    'line': i,
                    'name': None,
                    'position': None,
                    'pos_line': None,
                    'pos_column': None,
                    'length': None,
                    'attrb': [],
                    'initial': None,
                    'picin': None,
                    'picout': None,
                    'justify': None,
                    'color': None,
                    'hilight': None,
                    'raw': clean_line
                }

                # Extract field name (from label)
                parts = clean_line.split()
                if len(parts) > 1 and parts[0].upper() != 'DFHMDF':
                    field['name'] = parts[0]

                # Extract POS parameter
                pos_match = re.search(r'POS=\((\d+),(\d+)\)', clean_line, re.IGNORECASE)
                if pos_match:
                    field['pos_line'] = int(pos_match.group(1))
                    field['pos_column'] = int(pos_match.group(2))
                    field['position'] = f"({field['pos_line']},{field['pos_column']})"

                # Extract LENGTH parameter
                length_match = re.search(r'LENGTH=(\d+)', clean_line, re.IGNORECASE)
                if length_match:
                    field['length'] = int(length_match.group(1))

                # Extract ATTRB parameter
                attrb_match = re.search(r'ATTRB=\(([^)]+)\)', clean_line, re.IGNORECASE)
                if attrb_match:
                    field['attrb'] = [a.strip() for a in attrb_match.group(1).split(',')]
                elif re.search(r'ATTRB=(\w+)', clean_line, re.IGNORECASE):
                    single_attrb = re.search(r'ATTRB=(\w+)', clean_line, re.IGNORECASE)
                    field['attrb'] = [single_attrb.group(1)]

                # Extract INITIAL parameter
                initial_match = re.search(r"INITIAL='([^']*)'", clean_line, re.IGNORECASE)
                if initial_match:
                    field['initial'] = initial_match.group(1)

                # Extract PICIN parameter
                picin_match = re.search(r"PICIN='([^']*)'", clean_line, re.IGNORECASE)
                if picin_match:
                    field['picin'] = picin_match.group(1)

                # Extract PICOUT parameter
                picout_match = re.search(r"PICOUT='([^']*)'", clean_line, re.IGNORECASE)
                if picout_match:
                    field['picout'] = picout_match.group(1)

                # Extract JUSTIFY parameter
                justify_match = re.search(r'JUSTIFY=(\w+)', clean_line, re.IGNORECASE)
                if justify_match:
                    field['justify'] = justify_match.group(1).upper()

                # Extract COLOR parameter
                color_match = re.search(r'COLOR=(\w+)', clean_line, re.IGNORECASE)
                if color_match:
                    field['color'] = color_match.group(1).upper()

                # Extract HILIGHT parameter
                hilight_match = re.search(r'HILIGHT=(\w+)', clean_line, re.IGNORECASE)
                if hilight_match:
                    field['hilight'] = hilight_match.group(1).upper()

                fields.append(field)

        return fields

"""
Enhanced Dependency Analyzer
Tracks detailed program calls, method invocations, and dependencies with signatures
"""
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict


class DependencyAnalyzer:
    """Analyzes code dependencies with detailed type and signature information"""

    @staticmethod
    def analyze_cobol_calls(lines: List[str], file_path: str) -> List[Dict[str, Any]]:
        """
        Extract COBOL program calls and paragraph performs with details

        Returns list of dependencies with:
        - target: The program/paragraph being called
        - type: 'CALL', 'PERFORM', 'COPYBOOK', 'XCTL', 'LINK', 'CICS_RETURN', 'BMS_MAP', 'IMS_PSB'
        - line: Line number
        - signature: Full call statement
        - parameters: List of parameters if available
        """
        dependencies = []

        for i, line in enumerate(lines, 1):
            # Skip empty lines
            if not line.strip():
                continue

            # COBOL comment detection - comments have * or / in column 7 (index 6)
            # Also check for inline comments starting with *
            if len(line) >= 7:
                # Fixed-format COBOL: column 7 (index 6) contains comment indicator
                indicator_col = line[6] if len(line) > 6 else ''
                if indicator_col in ['*', '/', '$']:
                    continue  # Skip comment lines
                clean_line = line[6:72].strip()
            else:
                # Short line or free-format, check if starts with comment
                if line.strip().startswith('*'):
                    continue
                clean_line = line.strip()

            # CALL statement (external program invocation)
            call_match = re.search(
                r'\bCALL\s+[\'"]?([A-Z0-9\-]+)[\'"]?\s*(USING\s+(.+?))?(?:END-CALL|\.|$)',
                clean_line,
                re.IGNORECASE
            )
            if call_match:
                program_name = call_match.group(1)
                using_clause = call_match.group(3) if call_match.group(3) else None
                parameters = []

                if using_clause:
                    # Extract parameter names from USING clause
                    params = re.findall(r'([A-Z0-9\-]+)', using_clause, re.IGNORECASE)
                    parameters = [p for p in params if p.upper() not in ['BY', 'REFERENCE', 'CONTENT', 'VALUE']]

                dependencies.append({
                    'target': program_name,
                    'type': 'PROGRAM_CALL',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': parameters,
                    'description': f'Calls external program {program_name}'
                })

            # EXEC CICS XCTL (transfer control to another program)
            xctl_match = re.search(
                r'EXEC\s+CICS\s+XCTL\s+PROGRAM\([\'"]?([A-Z0-9\-]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if xctl_match:
                program_name = xctl_match.group(1)
                dependencies.append({
                    'target': program_name,
                    'type': 'CICS_XCTL',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [],
                    'description': f'CICS XCTL to program {program_name}'
                })

            # EXEC CICS LINK (link to another program) - Track as CICS_OP
            link_match = re.search(
                r'EXEC\s+CICS\s+LINK\s+PROGRAM\([\'"]?([A-Z0-9\-]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if link_match:
                program_name = link_match.group(1)
                # Extract COMMAREA if present
                commarea_match = re.search(r'COMMAREA\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                commarea = commarea_match.group(1) if commarea_match else ''

                params = ['LINK', program_name]
                if commarea:
                    params.append(f'COMMAREA:{commarea}')

                dependencies.append({
                    'target': program_name,
                    'type': 'CICS_OP',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': params,
                    'description': f'CICS LINK to program {program_name}'
                })

            # EXEC CICS RETURN (with or without TRANSID)
            return_transid_match = re.search(
                r'EXEC\s+CICS\s+RETURN\s+TRANSID\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if return_transid_match:
                transid = return_transid_match.group(1)
                dependencies.append({
                    'target': transid,
                    'type': 'CICS_RETURN_TRANSID',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [transid],
                    'description': f'Returns to CICS transaction {transid}'
                })
            elif re.search(r'EXEC\s+CICS\s+RETURN(?!\s+TRANSID)', clean_line, re.IGNORECASE):
                # Plain RETURN without TRANSID
                dependencies.append({
                    'target': 'RETURN',
                    'type': 'CICS_RETURN',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [],
                    'description': f'Returns control to CICS'
                })

            # EXEC CICS START TRANSID (start a new transaction)
            start_transid_match = re.search(
                r'EXEC\s+CICS\s+START\s+TRANSID\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if start_transid_match:
                transid = start_transid_match.group(1)
                dependencies.append({
                    'target': transid,
                    'type': 'CICS_START_TRANSID',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [],
                    'description': f'Starts CICS transaction {transid}'
                })

            # EXEC CICS SYNCPOINT - Track as CICS_OP
            syncpoint_match = re.search(
                r'EXEC\s+CICS\s+SYNCPOINT(?:\s+ROLLBACK)?',
                clean_line,
                re.IGNORECASE
            )
            if syncpoint_match:
                is_rollback = 'ROLLBACK' in clean_line.upper()
                op_type = 'SYNCPOINT ROLLBACK' if is_rollback else 'SYNCPOINT'

                dependencies.append({
                    'target': op_type,
                    'type': 'CICS_OP',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [op_type],
                    'description': f'CICS {op_type} operation'
                })

            # BMS Map usage - EXEC CICS SEND MAP / RECEIVE MAP
            send_map_match = re.search(
                r'EXEC\s+CICS\s+SEND\s+MAP\([\'"]?([A-Z0-9]+)[\'"]?\)\s+MAPSET\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if send_map_match:
                map_name = send_map_match.group(1)
                mapset_name = send_map_match.group(2)
                dependencies.append({
                    'target': f"{mapset_name}/{map_name}",
                    'type': 'BMS_SEND_MAP',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [mapset_name, map_name],
                    'description': f'Sends BMS map {map_name} from mapset {mapset_name}'
                })

            receive_map_match = re.search(
                r'EXEC\s+CICS\s+RECEIVE\s+MAP\([\'"]?([A-Z0-9]+)[\'"]?\)\s+MAPSET\([\'"]?([A-Z0-9]+)[\'"]?\)',
                clean_line,
                re.IGNORECASE
            )
            if receive_map_match:
                map_name = receive_map_match.group(1)
                mapset_name = receive_map_match.group(2)
                dependencies.append({
                    'target': f"{mapset_name}/{map_name}",
                    'type': 'BMS_RECEIVE_MAP',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [mapset_name, map_name],
                    'description': f'Receives BMS map {map_name} from mapset {mapset_name}'
                })

            # CICS SEND (non-BMS) - data/text transmission
            if 'EXEC' in clean_line.upper() and 'CICS' in clean_line.upper() and 'SEND' in clean_line.upper():
                # Check if it's NOT a BMS SEND MAP (already handled above)
                if not re.search(r'SEND\s+MAP\(', clean_line, re.IGNORECASE):
                    # EXEC CICS SEND FROM(...) or SEND TEXT(...)
                    send_from_match = re.search(r'FROM\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                    send_text_match = re.search(r'TEXT\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)

                    target = None
                    params = []
                    if send_from_match:
                        target = send_from_match.group(1)
                        params.append(f'FROM:{target}')
                    if send_text_match:
                        target = send_text_match.group(1)
                        params.append(f'TEXT:{target}')

                    if target or 'SEND' in clean_line.upper():
                        dependencies.append({
                            'target': target or 'SEND',
                            'type': 'CICS_SEND',
                            'line': i,
                            'signature': clean_line[:200],
                            'parameters': params,
                            'description': f'CICS SEND data transmission'
                        })

            # CICS RECEIVE (non-BMS) - data reception
            if 'EXEC' in clean_line.upper() and 'CICS' in clean_line.upper() and 'RECEIVE' in clean_line.upper():
                # Check if it's NOT a BMS RECEIVE MAP (already handled above)
                if not re.search(r'RECEIVE\s+MAP\(', clean_line, re.IGNORECASE):
                    # EXEC CICS RECEIVE INTO(...)
                    receive_into_match = re.search(r'INTO\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)

                    target = None
                    params = []
                    if receive_into_match:
                        target = receive_into_match.group(1)
                        params.append(f'INTO:{target}')

                    if target or 'RECEIVE' in clean_line.upper():
                        dependencies.append({
                            'target': target or 'RECEIVE',
                            'type': 'CICS_RECEIVE',
                            'line': i,
                            'signature': clean_line[:200],
                            'parameters': params,
                            'description': f'CICS RECEIVE data reception'
                        })

            # IMS DL/I calls - GU, GN, GHU, GHN, ISRT, DLET, REPL, CHKP
            ims_call_match = re.search(
                r'\bCALL\s+[\'"]?CBLTDLI[\'"]?\s+USING\s+([A-Z0-9\-]+)',
                clean_line,
                re.IGNORECASE
            )
            if ims_call_match:
                function = ims_call_match.group(1)
                dependencies.append({
                    'target': function,
                    'type': 'IMS_DLI_CALL',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [],
                    'description': f'IMS DL/I call using function {function}'
                })

            # 88-Level Conditions - Track all COBOL conditional data definitions
            condition_88_match = re.match(r'^\s*88\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if condition_88_match:
                condition_name = condition_88_match.group(1)
                # Extract VALUE if present
                value_match = re.search(r'VALUE\s+(.*?)(?:\.|$)', clean_line, re.IGNORECASE)
                value = value_match.group(1).strip() if value_match else ''
                dependencies.append({
                    'target': condition_name,
                    'type': 'CONDITION_88',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [value] if value else [],
                    'description': f'88-level condition {condition_name}'
                })
                continue  # Skip further processing for this line

            # Message Usage - Track error text and variable references (DISPLAY, STRING, MOVE ... TO ERROR-MSG, etc.)
            # Look for DISPLAY statements with error messages
            display_match = re.search(r'DISPLAY\s+([\'"].+?[\'"]|[A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if display_match and ('ERROR' in clean_line.upper() or 'FAIL' in clean_line.upper() or 'INVALID' in clean_line.upper()):
                message_text = display_match.group(1)
                # Extract variable names used in the message
                var_matches = re.findall(r'\b([A-Z][A-Z0-9\-]{2,})\b', clean_line, re.IGNORECASE)
                variables = [v for v in var_matches if v.upper() not in ['DISPLAY', 'ERROR', 'MESSAGE', 'UPON']]
                dependencies.append({
                    'target': message_text[:50],  # First 50 chars of message
                    'type': 'MESSAGE_USAGE',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': variables,
                    'description': f'Error/diagnostic message'
                })

            # IMS DL/I Operations - EXEC DLI commands (GU, GNP, REPL, SCHD, TERM)
            ims_dli_op_match = re.search(
                r'EXEC\s+DLI\s+(GU|GNP|GN|GHU|GHN|REPL|ISRT|DLET|SCHD|TERM)',
                clean_line,
                re.IGNORECASE
            )
            if ims_dli_op_match:
                op_type = ims_dli_op_match.group(1).upper()

                # Extract PCB, segment, layout, WHERE clause, PSB (for SCHD/TERM)
                pcb_match = re.search(r'PCB\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                segment_match = re.search(r'SEGMENT\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                layout_match = re.search(r'INTO\(([A-Z0-9\-]+)\)|FROM\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)
                where_match = re.search(r'WHERE\([\'"]?([A-Z0-9\-]+)[\'"]?\)', clean_line, re.IGNORECASE)
                psb_match = re.search(r'PSB\(([A-Z0-9\-]+)\)', clean_line, re.IGNORECASE)

                pcb = pcb_match.group(1) if pcb_match else ''
                segment = segment_match.group(1) if segment_match else ''
                layout = layout_match.group(1) or layout_match.group(2) if layout_match else ''
                where = where_match.group(1) if where_match else ''
                psb = psb_match.group(1) if psb_match else ''

                # Build parameter list
                params = [op_type]
                if pcb:
                    params.append(f'PCB:{pcb}')
                if segment:
                    params.append(f'SEG:{segment}')
                if layout:
                    params.append(f'LAYOUT:{layout}')
                if where:
                    params.append(f'WHERE:{where}')
                if psb:
                    params.append(f'PSB:{psb}')

                dependencies.append({
                    'target': f'{op_type}',
                    'type': 'IMS_DLI_OP',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': params,
                    'description': f'IMS DL/I {op_type} operation'
                })

            # IMS PSB (Program Specification Block) - only ENTRY statements
            # Skip 88-levels and VALUE clauses (now tracked separately as CONDITION_88)
            if not re.match(r'^\s*88\s', clean_line, re.IGNORECASE) and 'VALUE' not in clean_line.upper():
                # Only look for actual ENTRY statements for PSB scheduling
                psb_match = re.search(
                    r'ENTRY\s+[\'"]([A-Z0-9]+)[\'"]',
                    clean_line,
                    re.IGNORECASE
                )
                if psb_match:
                    psb_name = psb_match.group(1)
                    dependencies.append({
                        'target': psb_name,
                        'type': 'IMS_PSB',
                        'line': i,
                        'signature': clean_line[:200],
                        'parameters': [],
                        'description': f'Uses IMS PSB {psb_name}'
                    })

            # PERFORM statement (paragraph/section invocation)
            perform_match = re.search(
                r'\bPERFORM\s+([A-Z0-9\-]+)(?:\s+THRU\s+([A-Z0-9\-]+))?',
                clean_line,
                re.IGNORECASE
            )
            if perform_match and not re.search(r'\bPERFORM\s+(UNTIL|VARYING|TIMES)', clean_line, re.IGNORECASE):
                paragraph_name = perform_match.group(1)
                thru_paragraph = perform_match.group(2)

                # Skip if it looks like a keyword rather than a paragraph name
                if paragraph_name.upper() not in ['UNTIL', 'VARYING', 'TIMES', 'WITH', 'TEST']:
                    target = f"{paragraph_name}"
                    if thru_paragraph:
                        target += f" THRU {thru_paragraph}"

                    dependencies.append({
                        'target': paragraph_name,
                        'type': 'PERFORM_PARAGRAPH',
                        'line': i,
                        'signature': clean_line[:200],
                        'parameters': [],
                        'description': f'Performs paragraph {target}'
                    })

            # COPY statement (copybook include)
            copy_match = re.search(r'\bCOPY\s+([A-Z0-9\-]+)', clean_line, re.IGNORECASE)
            if copy_match:
                copybook_name = copy_match.group(1)
                dependencies.append({
                    'target': copybook_name,
                    'type': 'COPYBOOK',
                    'line': i,
                    'signature': clean_line[:200],
                    'parameters': [],
                    'description': f'Includes copybook {copybook_name}'
                })

        return dependencies

    @staticmethod
    def analyze_python_calls(content: str, file_path: str) -> List[Dict[str, Any]]:
        """
        Extract Python imports and function calls

        Returns detailed dependency information including:
        - Module imports
        - Function calls with signatures
        """
        dependencies = []
        lines = content.split('\n')

        for i, line in enumerate(lines, 1):
            # Import statements
            import_match = re.search(r'^\s*import\s+(\S+)', line)
            if import_match:
                module = import_match.group(1)
                dependencies.append({
                    'target': module,
                    'type': 'IMPORT',
                    'line': i,
                    'signature': line.strip(),
                    'parameters': [],
                    'description': f'Imports module {module}'
                })

            # From ... import statements
            from_match = re.search(r'^\s*from\s+(\S+)\s+import\s+(.+)', line)
            if from_match:
                module = from_match.group(1)
                imports = from_match.group(2).strip()
                dependencies.append({
                    'target': module,
                    'type': 'FROM_IMPORT',
                    'line': i,
                    'signature': line.strip(),
                    'parameters': [imp.strip() for imp in imports.split(',')],
                    'description': f'Imports {imports} from {module}'
                })

            # Function calls with parameters
            func_call_match = re.search(r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)', line)
            if func_call_match:
                func_name = func_call_match.group(1)
                params = func_call_match.group(2).strip()

                # Skip common built-ins and keywords
                if func_name not in ['if', 'for', 'while', 'def', 'class', 'print', 'len', 'range', 'str', 'int', 'float']:
                    param_list = [p.strip() for p in params.split(',') if p.strip()] if params else []
                    dependencies.append({
                        'target': func_name,
                        'type': 'FUNCTION_CALL',
                        'line': i,
                        'signature': f"{func_name}({params})",
                        'parameters': param_list,
                        'description': f'Calls function {func_name}'
                    })

        return dependencies

    @staticmethod
    def analyze_java_calls(content: str, file_path: str) -> List[Dict[str, Any]]:
        """Extract Java imports and method calls"""
        dependencies = []
        lines = content.split('\n')

        for i, line in enumerate(lines, 1):
            # Import statements
            import_match = re.search(r'^\s*import\s+([^;]+);', line)
            if import_match:
                import_path = import_match.group(1).strip()
                class_name = import_path.split('.')[-1]
                dependencies.append({
                    'target': class_name,
                    'type': 'IMPORT',
                    'line': i,
                    'signature': line.strip(),
                    'parameters': [],
                    'description': f'Imports {import_path}'
                })

            # Method calls (simplified - doesn't handle all cases)
            method_match = re.search(r'([a-zA-Z_][a-zA-Z0-9_]*)\s*\.\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)', line)
            if method_match:
                object_name = method_match.group(1)
                method_name = method_match.group(2)
                params = method_match.group(3).strip()

                param_list = [p.strip() for p in params.split(',') if p.strip()] if params else []
                dependencies.append({
                    'target': f"{object_name}.{method_name}",
                    'type': 'METHOD_CALL',
                    'line': i,
                    'signature': f"{object_name}.{method_name}({params})",
                    'parameters': param_list,
                    'description': f'Calls method {method_name} on {object_name}'
                })

        return dependencies

    @staticmethod
    def analyze_javascript_calls(content: str, file_path: str) -> List[Dict[str, Any]]:
        """Extract JavaScript/TypeScript imports and function calls"""
        dependencies = []
        lines = content.split('\n')

        for i, line in enumerate(lines, 1):
            # ES6 import
            import_match = re.search(r'import\s+.*?from\s+[\'"]([^\'"]+)[\'"]', line)
            if import_match:
                module = import_match.group(1)
                dependencies.append({
                    'target': module,
                    'type': 'IMPORT',
                    'line': i,
                    'signature': line.strip(),
                    'parameters': [],
                    'description': f'Imports from {module}'
                })

            # require()
            require_match = re.search(r'require\([\'"]([^\'"]+)[\'"]\)', line)
            if require_match:
                module = require_match.group(1)
                dependencies.append({
                    'target': module,
                    'type': 'REQUIRE',
                    'line': i,
                    'signature': line.strip(),
                    'parameters': [],
                    'description': f'Requires module {module}'
                })

            # Function calls
            func_call_match = re.search(r'([a-zA-Z_$][a-zA-Z0-9_$]*)\s*\(([^)]*)\)', line)
            if func_call_match:
                func_name = func_call_match.group(1)
                params = func_call_match.group(2).strip()

                # Skip common keywords
                if func_name not in ['if', 'for', 'while', 'function', 'return', 'console']:
                    param_list = [p.strip() for p in params.split(',') if p.strip()] if params else []
                    dependencies.append({
                        'target': func_name,
                        'type': 'FUNCTION_CALL',
                        'line': i,
                        'signature': f"{func_name}({params})",
                        'parameters': param_list,
                        'description': f'Calls function {func_name}'
                    })

        return dependencies

    @staticmethod
    def analyze_file_dependencies(file_path: Path, content: str = None) -> List[Dict[str, Any]]:
        """
        Main entry point for analyzing file dependencies

        Returns list of detailed dependencies with type, signature, and parameters
        """
        ext = file_path.suffix.lower()

        if content is None:
            try:
                with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
            except Exception:
                return []

        # Route to appropriate analyzer based on file type
        if ext in ['.cbl', '.cob', '.cobol', '.cpy']:
            lines = content.split('\n')
            return DependencyAnalyzer.analyze_cobol_calls(lines, str(file_path))
        elif ext == '.py':
            return DependencyAnalyzer.analyze_python_calls(content, str(file_path))
        elif ext == '.java':
            return DependencyAnalyzer.analyze_java_calls(content, str(file_path))
        elif ext in ['.js', '.ts', '.jsx', '.tsx']:
            return DependencyAnalyzer.analyze_javascript_calls(content, str(file_path))

        return []

    @staticmethod
    def group_dependencies_by_type(dependencies: List[Dict[str, Any]]) -> Dict[str, List[Dict[str, Any]]]:
        """Group dependencies by their type for easier display"""
        grouped = defaultdict(list)
        for dep in dependencies:
            grouped[dep['type']].append(dep)
        return dict(grouped)

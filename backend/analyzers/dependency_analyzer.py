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
        - type: 'CALL', 'PERFORM', 'COPYBOOK'
        - line: Line number
        - signature: Full call statement
        - parameters: List of parameters if available
        """
        dependencies = []

        for i, line in enumerate(lines, 1):
            clean_line = line[6:72].strip() if len(line) >= 7 else line.strip()

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

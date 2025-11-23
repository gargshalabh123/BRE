"""
Search and discovery tools for finding code patterns across the codebase

All searches restricted to analyzed files only
"""
from pathlib import Path
from typing import Dict, List, Optional, Any
from fnmatch import fnmatch
import re
import ast

from .base_tool import ToolBase, FileAccessError


class SearchTool(ToolBase):
    """Search tools that respect analyzed files only"""

    async def search_code(
        self,
        query: str,
        search_type: str = "text",
        file_pattern: str = "*",
        max_results: int = 20
    ) -> List[Dict[str, Any]]:
        """
        Search for code patterns ONLY in analyzed files

        Args:
            query: Search query (regex for text, plain text for semantic, syntax for ast)
            search_type: "text" (regex), "semantic" (AI-based, future), "ast" (syntax tree)
            file_pattern: Limit to specific files (e.g., "*.py")
            max_results: Maximum number of results

        Returns:
            List of matches with file, line, code snippet, and score
        """
        if search_type == "text":
            return await self._text_search(query, file_pattern, max_results)
        elif search_type == "ast":
            return await self._ast_search(query, file_pattern, max_results)
        elif search_type == "semantic":
            # Placeholder for future RAG/vector search implementation
            return [{"error": "Semantic search not yet implemented. Use 'text' or 'ast' instead."}]
        else:
            return [{"error": f"Unknown search type: {search_type}"}]

    async def _text_search(
        self,
        query: str,
        pattern: str,
        limit: int
    ) -> List[Dict[str, Any]]:
        """Text/regex search restricted to analyzed files"""
        results = []

        # Compile regex once
        try:
            regex = re.compile(query, re.IGNORECASE)
        except re.error as e:
            return [{"error": f"Invalid regex pattern: {str(e)}"}]

        # IMPORTANT: Only search in analyzed files
        for file_path in self.analyzed_files:
            # Apply pattern filter
            if pattern != "*" and not fnmatch(file_path, pattern):
                continue

            try:
                full_path = self.upload_dir / file_path

                with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                    lines = f.readlines()

                # Search each line
                for i, line in enumerate(lines, 1):
                    if regex.search(line):
                        results.append({
                            'file': file_path,
                            'line': i,
                            'code': line.strip(),
                            'context': self._get_context_lines(lines, i, 3),
                            'score': 1.0
                        })

                        if len(results) >= limit:
                            return results

            except Exception:
                # Skip files that can't be read
                continue

        return results

    async def _ast_search(
        self,
        query: str,
        pattern: str,
        limit: int
    ) -> List[Dict[str, Any]]:
        """
        AST-based search for Python functions, classes, etc.

        Query format:
        - "function:name" - find function definitions
        - "class:name" - find class definitions
        - "import:module" - find imports
        """
        results = []

        # Parse query
        if ':' in query:
            search_type, search_term = query.split(':', 1)
            search_type = search_type.lower()
        else:
            search_type = 'any'
            search_term = query

        search_term = search_term.lower()

        # Only search Python files
        for file_path in self.analyzed_files:
            if not file_path.endswith('.py'):
                continue

            # Apply pattern filter
            if pattern != "*" and not fnmatch(file_path, pattern):
                continue

            try:
                full_path = self.upload_dir / file_path

                with open(full_path, 'r', encoding='utf-8') as f:
                    content = f.read()

                tree = ast.parse(content)

                # Search for different node types
                for node in ast.walk(tree):
                    if search_type in ('function', 'any') and isinstance(node, ast.FunctionDef):
                        if search_term in node.name.lower():
                            results.append({
                                'file': file_path,
                                'line': node.lineno,
                                'type': 'function',
                                'name': node.name,
                                'signature': self._get_function_signature(node),
                                'code': f"def {node.name}(...)"
                            })

                    elif search_type in ('class', 'any') and isinstance(node, ast.ClassDef):
                        if search_term in node.name.lower():
                            results.append({
                                'file': file_path,
                                'line': node.lineno,
                                'type': 'class',
                                'name': node.name,
                                'code': f"class {node.name}"
                            })

                    elif search_type == 'import' and isinstance(node, (ast.Import, ast.ImportFrom)):
                        if isinstance(node, ast.Import):
                            for alias in node.names:
                                if search_term in alias.name.lower():
                                    results.append({
                                        'file': file_path,
                                        'line': node.lineno,
                                        'type': 'import',
                                        'name': alias.name,
                                        'code': f"import {alias.name}"
                                    })
                        elif isinstance(node, ast.ImportFrom):
                            module = node.module or ''
                            if search_term in module.lower():
                                results.append({
                                    'file': file_path,
                                    'line': node.lineno,
                                    'type': 'import',
                                    'name': module,
                                    'code': f"from {module} import ..."
                                })

                    if len(results) >= limit:
                        return results

            except Exception:
                # Skip files with syntax errors or other issues
                continue

        return results

    async def find_similar_code(
        self,
        code_snippet: str,
        similarity_threshold: float = 0.7,
        max_results: int = 10
    ) -> List[Dict[str, Any]]:
        """
        Find code similar to a given snippet

        Uses simple token-based similarity for now
        Future: Use embeddings for semantic similarity

        Args:
            code_snippet: Example code to match
            similarity_threshold: 0.0 to 1.0 (default: 0.7)
            max_results: Maximum results

        Returns:
            Similar code with similarity scores
        """
        results = []

        # Tokenize query
        query_tokens = self._tokenize_code(code_snippet)

        if not query_tokens:
            return [{"error": "Code snippet too short"}]

        # Search across all analyzed files
        for file_path in self.analyzed_files:
            try:
                full_path = self.upload_dir / file_path

                with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                    lines = f.readlines()

                # Check each line
                for i, line in enumerate(lines, 1):
                    line_tokens = self._tokenize_code(line)

                    if not line_tokens:
                        continue

                    # Calculate similarity
                    similarity = self._jaccard_similarity(query_tokens, line_tokens)

                    if similarity >= similarity_threshold:
                        results.append({
                            'file': file_path,
                            'line': i,
                            'code': line.strip(),
                            'similarity': round(similarity, 3),
                            'context': self._get_context_lines(lines, i, 2)
                        })

            except Exception:
                continue

        # Sort by similarity
        results.sort(key=lambda x: x['similarity'], reverse=True)

        return results[:max_results]

    async def find_definition(
        self,
        name: str,
        def_type: str = "any"
    ) -> Optional[Dict[str, Any]]:
        """
        Find the definition of a function, class, or variable

        Args:
            name: Name to search for
            def_type: "function", "class", "variable", or "any"

        Returns:
            Definition location with code, or None if not found
        """
        # Use AST search for Python files
        query = f"{def_type}:{name}" if def_type != "any" else name

        results = await self._ast_search(query, "*.py", limit=1)

        if results:
            result = results[0]

            # Enhance with actual code
            try:
                code_lines = await self._read_definition_code(
                    result['file'],
                    result['line']
                )
                result['full_code'] = code_lines
            except Exception:
                pass

            return result

        return None

    def _get_context_lines(
        self,
        lines: List[str],
        target_line: int,
        context: int
    ) -> str:
        """Get surrounding lines for context"""
        start = max(0, target_line - context - 1)
        end = min(len(lines), target_line + context)

        context_lines = []
        for i in range(start, end):
            prefix = ">>> " if i == target_line - 1 else "    "
            context_lines.append(f"{prefix}{i+1:4d} | {lines[i].rstrip()}")

        return '\n'.join(context_lines)

    def _get_function_signature(self, node: ast.FunctionDef) -> str:
        """Extract function signature from AST node"""
        args = []

        # Regular args
        for arg in node.args.args:
            arg_str = arg.arg
            if arg.annotation:
                arg_str += f": {ast.unparse(arg.annotation)}"
            args.append(arg_str)

        # Return type
        returns = ""
        if node.returns:
            returns = f" -> {ast.unparse(node.returns)}"

        return f"def {node.name}({', '.join(args)}){returns}"

    def _tokenize_code(self, code: str) -> set:
        """Simple tokenization for similarity comparison"""
        # Remove common punctuation, split on whitespace
        code = re.sub(r'[(){}\[\],;.]', ' ', code)
        tokens = code.lower().split()

        # Remove very common words
        stopwords = {'the', 'a', 'an', 'and', 'or', 'if', 'else', 'for', 'while', 'return'}
        tokens = [t for t in tokens if t not in stopwords and len(t) > 1]

        return set(tokens)

    def _jaccard_similarity(self, set1: set, set2: set) -> float:
        """Calculate Jaccard similarity between two sets"""
        if not set1 or not set2:
            return 0.0

        intersection = len(set1 & set2)
        union = len(set1 | set2)

        return intersection / union if union > 0 else 0.0

    async def _read_definition_code(
        self,
        file_path: str,
        start_line: int,
        max_lines: int = 50
    ) -> str:
        """Read function/class definition code"""
        try:
            full_path = self.upload_dir / file_path

            with open(full_path, 'r', encoding='utf-8') as f:
                lines = f.readlines()

            # Read until we find the end of the definition
            # (simple heuristic: until next def/class or dedent)
            end_line = min(start_line + max_lines, len(lines))

            code_lines = []
            for i in range(start_line - 1, end_line):
                code_lines.append(lines[i].rstrip())

            return '\n'.join(code_lines)

        except Exception:
            return ""

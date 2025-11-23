"""
Graph and relationship tools for analyzing dependencies and call graphs

These tools help trace how code components relate to each other
"""
from typing import Dict, List, Optional, Any
from collections import defaultdict, deque

from .base_tool import ToolBase


class GraphTool(ToolBase):
    """Tools for analyzing code relationships and dependencies"""

    async def get_dependencies(
        self,
        file_path: str,
        direction: str = "outgoing",
        depth: int = 1
    ) -> List[Dict[str, Any]]:
        """
        Get file dependencies (imports/requires)

        Args:
            file_path: Path to file
            direction: "outgoing" (what this imports) or "incoming" (what imports this)
            depth: How many levels deep (default: 1, max: 3)

        Returns:
            List of dependencies with details
        """
        try:
            self._validate_file_access(file_path)
        except Exception as e:
            return [{"error": str(e)}]

        normalized_path = self._normalize_path(file_path)
        depth = min(depth, 3)  # Cap at 3 levels

        if direction == "outgoing":
            return await self._get_outgoing_dependencies(normalized_path, depth)
        else:
            return await self._get_incoming_dependencies(normalized_path, depth)

    async def _get_outgoing_dependencies(
        self,
        file_path: str,
        depth: int
    ) -> List[Dict[str, Any]]:
        """Get what this file imports/depends on"""
        detailed_deps = self.analysis_results.get('detailed_dependencies', {})

        visited = set()
        result = []

        def traverse(path: str, current_depth: int):
            if current_depth > depth or path in visited:
                return

            visited.add(path)

            # Get direct dependencies
            for dep in detailed_deps.get(path, []):
                dep_info = {
                    'target': dep.get('target', ''),
                    'type': dep.get('type', 'unknown'),
                    'line': dep.get('line', 0),
                    'signature': dep.get('signature', ''),
                    'depth': current_depth
                }

                result.append(dep_info)

                # Recurse if target is also an analyzed file
                target = dep.get('target', '')
                if current_depth < depth:
                    # Try to find matching file
                    matching_file = self._find_matching_file(target)
                    if matching_file:
                        traverse(matching_file, current_depth + 1)

        traverse(file_path, 1)

        return result

    async def _get_incoming_dependencies(
        self,
        file_path: str,
        depth: int
    ) -> List[Dict[str, Any]]:
        """Get what files import/depend on this file"""
        detailed_deps = self.analysis_results.get('detailed_dependencies', {})

        visited = set()
        result = []

        def traverse(target_path: str, current_depth: int):
            if current_depth > depth:
                return

            # Find files that depend on target_path
            for file, deps in detailed_deps.items():
                if file in visited:
                    continue

                for dep in deps:
                    dep_target = dep.get('target', '')

                    # Check if this dependency points to our target
                    if target_path in dep_target or dep_target in target_path:
                        dep_info = {
                            'file': file,
                            'line': dep.get('line', 0),
                            'type': dep.get('type', 'unknown'),
                            'signature': dep.get('signature', ''),
                            'depth': current_depth
                        }

                        result.append(dep_info)
                        visited.add(file)

                        # Recurse
                        if current_depth < depth:
                            traverse(file, current_depth + 1)

        traverse(file_path, 1)

        return result

    async def trace_function_calls(
        self,
        function_name: str,
        direction: str = "callers",
        max_depth: int = 3
    ) -> Dict[str, Any]:
        """
        Trace function call graph

        Args:
            function_name: Name of function to trace
            direction: "callers" (who calls this) or "callees" (what this calls)
            max_depth: Maximum depth to trace

        Returns:
            Call graph tree structure
        """
        # Find function definition first
        from .search_tools import SearchTool

        search_tool = SearchTool(self.upload_dir, self.analysis_results)
        definition = await search_tool.find_definition(function_name, "function")

        if not definition:
            return {"error": f"Function '{function_name}' not found"}

        # Build call graph
        if direction == "callers":
            return await self._trace_callers(function_name, definition, max_depth)
        else:
            return await self._trace_callees(function_name, definition, max_depth)

    async def _trace_callers(
        self,
        function_name: str,
        definition: Dict[str, Any],
        max_depth: int
    ) -> Dict[str, Any]:
        """Find who calls this function"""
        from .search_tools import SearchTool

        search_tool = SearchTool(self.upload_dir, self.analysis_results)

        # Search for calls to this function
        query = f"{function_name}\\s*\\("
        matches = await search_tool.search_code(query, "text", max_results=50)

        # Filter out the definition itself
        callers = [
            m for m in matches
            if not (m['file'] == definition['file'] and m['line'] == definition['line'])
        ]

        return {
            'function': function_name,
            'file': definition['file'],
            'line': definition['line'],
            'signature': definition.get('signature', ''),
            'caller_count': len(callers),
            'callers': [
                {
                    'file': c['file'],
                    'line': c['line'],
                    'code': c['code']
                }
                for c in callers[:20]  # Limit to 20 callers
            ]
        }

    async def _trace_callees(
        self,
        function_name: str,
        definition: Dict[str, Any],
        max_depth: int
    ) -> Dict[str, Any]:
        """Find what functions this calls"""
        from .file_tools import FileTool

        file_tool = FileTool(self.upload_dir, self.analysis_results)

        # Read the function body
        function_code = await file_tool.read_file(
            definition['file'],
            start_line=definition['line'],
            end_line=definition['line'] + 50  # Read next 50 lines
        )

        # Extract function calls (simple regex-based)
        import re
        call_pattern = r'(\w+)\s*\('
        calls = re.findall(call_pattern, function_code)

        # Remove duplicates and common keywords
        keywords = {'if', 'for', 'while', 'def', 'class', 'return', 'print', 'len', 'str', 'int'}
        unique_calls = list(set(c for c in calls if c not in keywords))

        return {
            'function': function_name,
            'file': definition['file'],
            'line': definition['line'],
            'callees': unique_calls[:20],  # Limit to 20
            'note': 'Simple pattern matching - may include false positives'
        }

    async def build_dependency_graph(
        self,
        format: str = "summary"
    ) -> Dict[str, Any]:
        """
        Build complete dependency graph for codebase

        Args:
            format: "summary" (counts), "detailed" (full graph), or "clusters" (module groups)

        Returns:
            Dependency graph structure
        """
        detailed_deps = self.analysis_results.get('detailed_dependencies', {})

        if format == "summary":
            return self._build_summary_graph(detailed_deps)
        elif format == "detailed":
            return self._build_detailed_graph(detailed_deps)
        elif format == "clusters":
            return self._build_clustered_graph(detailed_deps)
        else:
            return {"error": f"Unknown format: {format}"}

    def _build_summary_graph(
        self,
        detailed_deps: Dict[str, List[Dict]]
    ) -> Dict[str, Any]:
        """Build summary statistics of dependencies"""
        total_files = len(detailed_deps)
        total_deps = sum(len(deps) for deps in detailed_deps.values())

        # Find most connected files
        dep_counts = [
            (file, len(deps))
            for file, deps in detailed_deps.items()
        ]
        dep_counts.sort(key=lambda x: x[1], reverse=True)

        # Find files with no dependencies (potential entry points)
        no_deps = [file for file, deps in detailed_deps.items() if len(deps) == 0]

        return {
            'total_files': total_files,
            'total_dependencies': total_deps,
            'average_dependencies_per_file': round(total_deps / total_files, 2) if total_files > 0 else 0,
            'most_connected': [
                {'file': file, 'dependency_count': count}
                for file, count in dep_counts[:10]
            ],
            'files_with_no_dependencies': no_deps[:10]
        }

    def _build_detailed_graph(
        self,
        detailed_deps: Dict[str, List[Dict]]
    ) -> Dict[str, Any]:
        """Build full dependency graph"""
        nodes = []
        edges = []

        # Create nodes
        for file in self.analyzed_files:
            nodes.append({
                'id': file,
                'type': 'file'
            })

        # Create edges
        for source_file, deps in detailed_deps.items():
            for dep in deps:
                edges.append({
                    'source': source_file,
                    'target': dep.get('target', ''),
                    'type': dep.get('type', 'unknown')
                })

        return {
            'nodes': nodes,
            'edges': edges[:500],  # Limit edges for performance
            'note': 'Limited to 500 edges for performance'
        }

    def _build_clustered_graph(
        self,
        detailed_deps: Dict[str, List[Dict]]
    ) -> Dict[str, Any]:
        """Group files into clusters/modules"""
        clusters = defaultdict(list)

        # Simple clustering by directory
        for file in self.analyzed_files:
            # Get first directory component
            parts = file.split('/')
            if len(parts) > 1:
                cluster = parts[0]
            else:
                cluster = 'root'

            clusters[cluster].append(file)

        return {
            'total_clusters': len(clusters),
            'clusters': [
                {
                    'name': name,
                    'file_count': len(files),
                    'files': files[:10]  # Limit to 10 files per cluster
                }
                for name, files in sorted(clusters.items(), key=lambda x: len(x[1]), reverse=True)
            ]
        }

    def _find_matching_file(self, target: str) -> Optional[str]:
        """Find a file matching the target dependency"""
        # Normalize target
        target = target.replace('.', '/').strip()

        for file in self.analyzed_files:
            if target in file or file.endswith(f"{target}.py"):
                return file

        return None

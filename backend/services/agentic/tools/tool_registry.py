"""
Central tool registry for agentic code analysis

Manages all tools and provides unified interface for agents
"""
from pathlib import Path
from typing import Dict, Any, Optional, Callable
import os

from .file_tools import FileTool
from .search_tools import SearchTool
from .analysis_tools import AnalysisTool
from .graph_tools import GraphTool


class ToolRegistry:
    """
    Central registry for all analysis tools

    Provides unified interface for agents to access tools with access control
    """

    def __init__(self, upload_id: str, analysis_results: Dict[str, Any]):
        """
        Initialize tool registry

        Args:
            upload_id: Unique identifier for uploaded codebase
            analysis_results: Complete analysis results from database
        """
        self.upload_id = upload_id
        self.analysis_results = analysis_results

        # Determine upload directory
        self.upload_dir = self._get_upload_dir(upload_id)

        # Initialize all tool categories with access control
        self.file_tool = FileTool(self.upload_dir, analysis_results)
        self.search_tool = SearchTool(self.upload_dir, analysis_results)
        self.analysis_tool = AnalysisTool(self.upload_dir, analysis_results)
        self.graph_tool = GraphTool(self.upload_dir, analysis_results)

        # Build tool map with all available tools
        self.tools = self._register_all_tools()

        print(f"[ToolRegistry] Initialized for upload {upload_id}")
        print(f"[ToolRegistry] Upload directory: {self.upload_dir}")
        print(f"[ToolRegistry] Accessible files: {len(self.file_tool.analyzed_files)}")
        print(f"[ToolRegistry] Available tools: {len(self.tools)}")

    def _get_upload_dir(self, upload_id: str) -> Path:
        """Get the extracted directory for this upload"""
        base_dir = Path(os.getenv("UPLOAD_DIR", "../uploads"))
        upload_dir = base_dir / upload_id

        # Check for extracted subdirectory
        extracted_dir = upload_dir / "extracted"
        if extracted_dir.exists():
            return extracted_dir

        return upload_dir

    def _register_all_tools(self) -> Dict[str, Callable]:
        """Register all available tools"""
        return {
            # ========== File Operations ==========
            "get_full_codebase_content": self.file_tool.get_full_codebase_content,
            "read_file": self.file_tool.read_file,
            "get_file_list": self.file_tool.get_file_list,
            "get_file_context": self.file_tool.get_file_context,
            "get_file_info": self.file_tool.get_file_info,
            "get_codebase_structure": self.file_tool.get_codebase_structure,
            "read_multiple_files": self.file_tool.read_multiple_files,

            # ========== Search & Discovery ==========
            "search_code": self.search_tool.search_code,
            "find_similar_code": self.search_tool.find_similar_code,
            "find_definition": self.search_tool.find_definition,

            # ========== Analysis & Metrics ==========
            "get_file_metrics": self.analysis_tool.get_file_metrics,
            "get_codebase_summary": self.analysis_tool.get_codebase_summary,
            "query_database_operations": self.analysis_tool.query_database_operations,
            "get_business_rules": self.analysis_tool.get_business_rules,
            "get_complexity_hotspots": self.analysis_tool.get_complexity_hotspots,
            "get_files_by_type": self.analysis_tool.get_files_by_type,
            "get_database_statistics": self.analysis_tool.get_database_statistics,
            "get_business_rule_statistics": self.analysis_tool.get_business_rule_statistics,
            "find_duplicate_code_patterns": self.analysis_tool.find_duplicate_code_patterns,

            # ========== Graph & Relationships ==========
            "get_dependencies": self.graph_tool.get_dependencies,
            "trace_function_calls": self.graph_tool.trace_function_calls,
            "build_dependency_graph": self.graph_tool.build_dependency_graph,
        }

    def get_tool(self, tool_name: str) -> Optional[Callable]:
        """
        Get a tool by name

        Args:
            tool_name: Name of the tool

        Returns:
            Tool function or None if not found
        """
        return self.tools.get(tool_name)

    def list_tools(self) -> Dict[str, str]:
        """
        List all available tools with descriptions

        Returns:
            Dict mapping tool names to descriptions
        """
        return {
            # File Operations
            "get_full_codebase_content": "⭐ BEST FOR ARCHITECTURE/BUSINESS LOGIC: Get ALL files content in ONE call - most efficient! Reads entire codebase and concatenates all files with separators",
            "read_file": "Read source code from a file (with optional line range)",
            "get_file_list": "List files matching pattern/criteria",
            "get_file_context": "Get code context around a specific line",
            "get_file_info": "Get detailed file metadata and metrics",
            "get_codebase_structure": "Get complete overview of ALL files in codebase (organized by directory/type)",
            "read_multiple_files": "Read multiple files at once. Pass file_paths as JSON array string like '[\"file1.py\", \"file2.py\"]' or comma-separated",

            # Search & Discovery
            "search_code": "Search for code patterns (text/regex/AST)",
            "find_similar_code": "Find code similar to a given snippet",
            "find_definition": "Find function/class definition",

            # Analysis & Metrics
            "get_file_metrics": "Get metrics for a specific file",
            "get_codebase_summary": "Get overall codebase statistics",
            "query_database_operations": "Query database operations (SQL, etc.)",
            "get_business_rules": "Get business rules with filters",
            "get_complexity_hotspots": "Find files with highest complexity",
            "get_files_by_type": "Get files of a specific type",
            "get_database_statistics": "Get database operation statistics",
            "get_business_rule_statistics": "Get business rule statistics",
            "find_duplicate_code_patterns": "Find potential code duplication",

            # Graph & Relationships
            "get_dependencies": "Get file dependencies (incoming/outgoing)",
            "trace_function_calls": "Trace function call graph",
            "build_dependency_graph": "Build complete dependency graph",
        }

    def list_accessible_files(self) -> list:
        """
        Get list of files that can be accessed

        Returns:
            Sorted list of accessible file paths
        """
        return self.file_tool.list_accessible_files()

    def get_tool_categories(self) -> Dict[str, list]:
        """
        Get tools organized by category

        Returns:
            Dict mapping categories to tool lists
        """
        return {
            "File Operations": [
                "get_full_codebase_content",
                "read_file",
                "get_file_list",
                "get_file_context",
                "get_file_info",
                "get_codebase_structure",
                "read_multiple_files"
            ],
            "Search & Discovery": [
                "search_code",
                "find_similar_code",
                "find_definition"
            ],
            "Analysis & Metrics": [
                "get_file_metrics",
                "get_codebase_summary",
                "query_database_operations",
                "get_business_rules",
                "get_complexity_hotspots",
                "get_files_by_type",
                "get_database_statistics",
                "get_business_rule_statistics",
                "find_duplicate_code_patterns"
            ],
            "Graph & Relationships": [
                "get_dependencies",
                "trace_function_calls",
                "build_dependency_graph"
            ]
        }

    async def execute_tool(
        self,
        tool_name: str,
        **kwargs
    ) -> Any:
        """
        Execute a tool by name with parameters

        Args:
            tool_name: Name of the tool to execute
            **kwargs: Tool parameters

        Returns:
            Tool execution result

        Raises:
            ValueError: If tool not found
        """
        tool = self.get_tool(tool_name)

        if tool is None:
            raise ValueError(
                f"Unknown tool: {tool_name}. "
                f"Available tools: {', '.join(self.tools.keys())}"
            )

        try:
            # Execute tool (async)
            result = await tool(**kwargs)
            return result
        except Exception as e:
            return {"error": f"Tool execution failed: {str(e)}"}

    def get_tool_usage_stats(self) -> Dict[str, Any]:
        """
        Get statistics about tool availability and codebase

        Returns:
            Stats dict
        """
        return {
            "upload_id": self.upload_id,
            "upload_directory": str(self.upload_dir),
            "accessible_files": len(self.file_tool.analyzed_files),
            "total_tools": len(self.tools),
            "tool_categories": len(self.get_tool_categories()),
            "codebase_summary": self.analysis_results.get('summary', {})
        }

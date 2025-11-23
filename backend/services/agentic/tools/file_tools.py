"""
File operation tools for reading and accessing analyzed code files

All operations respect file access control - only analyzed files can be accessed
"""
from pathlib import Path
from typing import Dict, List, Optional, Any
from fnmatch import fnmatch

from .base_tool import ToolBase, FileAccessError


class FileTool(ToolBase):
    """File operations with strict access control"""

    async def read_file(
        self,
        file_path: str,
        start_line: Optional[int] = None,
        end_line: Optional[int] = None
    ) -> str:
        """
        Read source code from analyzed files ONLY

        Args:
            file_path: Relative path to file
            start_line: Optional starting line number (1-indexed)
            end_line: Optional ending line number (1-indexed, inclusive)

        Returns:
            Source code content with line numbers

        Security: Only files that were analyzed can be read
        """
        try:
            # CRITICAL: Validate access first
            self._validate_file_access(file_path)

            # Construct full path
            full_path = self.upload_dir / file_path

            # Additional check: file must physically exist
            if not full_path.exists():
                return f"ERROR: File '{file_path}' not found in uploaded codebase"

            # Read file
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()

            # Return requested range
            if start_line and end_line:
                start_idx = max(0, start_line - 1)
                end_idx = min(len(lines), end_line)
                selected = lines[start_idx:end_idx]

                result = []
                for i, line in enumerate(selected):
                    line_num = start_line + i
                    result.append(f"{line_num:5d} | {line.rstrip()}")

                return '\n'.join(result)
            else:
                # Limit full file reads to prevent token overload
                max_lines = 500
                if len(lines) > max_lines:
                    result = []
                    for i, line in enumerate(lines[:max_lines]):
                        result.append(f"{i+1:5d} | {line.rstrip()}")
                    result.append(f"\n... (truncated, showing {max_lines}/{len(lines)} lines)")
                    result.append(f"Use start_line and end_line parameters to read specific sections")
                    return '\n'.join(result)

                result = []
                for i, line in enumerate(lines):
                    result.append(f"{i+1:5d} | {line.rstrip()}")
                return '\n'.join(result)

        except FileAccessError as e:
            return f"ACCESS DENIED: {str(e)}"
        except Exception as e:
            return f"ERROR: {str(e)}"

    async def get_file_list(
        self,
        pattern: str = "*",
        extension: Optional[str] = None,
        min_loc: Optional[int] = None
    ) -> List[Dict[str, Any]]:
        """
        List analyzed files only

        Args:
            pattern: Glob pattern to filter (e.g., "*.py", "api/**/*.js")
                     Use "*" or "**/" to get all files
            extension: Filter by extension (e.g., ".py")
            min_loc: Minimum lines of code

        Returns:
            List of analyzed files matching criteria
        """
        results = []

        # Only iterate through analyzed files
        for file_path in self.analyzed_files:
            # Apply pattern filter
            # Special case: "**/" or "**" means all files
            if pattern in ["*", "**/", "**"]:
                pass  # Include all files
            elif not fnmatch(file_path, pattern):
                continue

            # Apply extension filter
            if extension and not file_path.endswith(extension):
                continue

            # Get metadata from analysis results
            file_info = self._get_file_metadata(file_path)

            # Apply LOC filter
            loc = file_info.get('loc', 0)
            if min_loc and loc < min_loc:
                continue

            results.append({
                'path': file_path,
                'extension': file_info.get('extension', ''),
                'type': file_info.get('type', 'Unknown'),
                'size': file_info.get('size', 0),
                'loc': loc,
                'sloc': file_info.get('sloc', 0),
                'complexity': file_info.get('complexity', 0)
            })

        return sorted(results, key=lambda x: x['path'])

    async def get_file_context(
        self,
        file_path: str,
        line_number: int,
        context_lines: int = 10
    ) -> str:
        """
        Get code context around a specific line number

        Args:
            file_path: Path to file
            line_number: Target line (1-indexed)
            context_lines: Lines before/after to include (default: 10)

        Returns:
            Code with context, highlighting the target line
        """
        try:
            # Validate access
            self._validate_file_access(file_path)

            # Construct full path
            full_path = self.upload_dir / file_path

            if not full_path.exists():
                return f"ERROR: File '{file_path}' not found"

            # Read file
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()

            # Calculate range
            start = max(0, line_number - context_lines - 1)
            end = min(len(lines), line_number + context_lines)

            # Build context with highlighting
            context = []
            for i in range(start, end):
                actual_line_num = i + 1
                line_content = lines[i].rstrip()

                # Highlight target line
                if actual_line_num == line_number:
                    prefix = ">>> "
                else:
                    prefix = "    "

                context.append(f"{prefix}{actual_line_num:5d} | {line_content}")

            return '\n'.join(context)

        except FileAccessError as e:
            return f"ACCESS DENIED: {str(e)}"
        except Exception as e:
            return f"ERROR: {str(e)}"

    async def get_file_info(self, file_path: str) -> Dict[str, Any]:
        """
        Get detailed information about a file

        Args:
            file_path: Path to file

        Returns:
            File metadata and metrics
        """
        try:
            self._validate_file_access(file_path)

            metadata = self._get_file_metadata(file_path)

            if not metadata:
                return {"error": f"No metadata found for {file_path}"}

            return {
                'path': file_path,
                'name': Path(file_path).name,
                'extension': metadata.get('extension', ''),
                'type': metadata.get('type', 'Unknown'),
                'size': metadata.get('size', 0),
                'loc': metadata.get('loc', 0),
                'sloc': metadata.get('sloc', 0),
                'comments': metadata.get('comments', 0),
                'blank': metadata.get('blank', 0),
                'complexity': metadata.get('complexity', 0),
                'complexity_rating': metadata.get('complexity_rating', 'Unknown'),
                'accessible': True
            }

        except FileAccessError as e:
            return {
                'path': file_path,
                'accessible': False,
                'error': str(e)
            }

    async def get_codebase_structure(self) -> Dict[str, Any]:
        """
        Get comprehensive overview of entire codebase structure

        This gives the agent a complete picture of all files, their organization,
        and key metrics. Essential for architecture and business logic questions.

        Returns:
            Complete codebase structure with all files and their metadata
        """
        # Group files by directory
        by_directory = {}
        by_extension = {}
        all_files = []

        total_loc = 0
        total_complexity = 0

        for file_path in sorted(self.analyzed_files):
            metadata = self._get_file_metadata(file_path)

            # Extract directory
            path_obj = Path(file_path)
            directory = str(path_obj.parent) if path_obj.parent != Path('.') else 'root'
            extension = path_obj.suffix or 'no_extension'

            file_info = {
                'path': file_path,
                'name': path_obj.name,
                'extension': extension,
                'loc': metadata.get('loc', 0),
                'sloc': metadata.get('sloc', 0),
                'complexity': metadata.get('complexity', 0),
                'functions': metadata.get('functions', 0)
            }

            # Group by directory
            if directory not in by_directory:
                by_directory[directory] = []
            by_directory[directory].append(file_info)

            # Group by extension
            if extension not in by_extension:
                by_extension[extension] = []
            by_extension[extension].append(file_info)

            all_files.append(file_info)
            total_loc += file_info['loc']
            total_complexity += file_info['complexity']

        return {
            'summary': {
                'total_files': len(all_files),
                'total_loc': total_loc,
                'total_complexity': total_complexity,
                'avg_complexity': round(total_complexity / max(len(all_files), 1), 2),
                'directories': len(by_directory),
                'file_types': len(by_extension)
            },
            'by_directory': by_directory,
            'by_extension': by_extension,
            'all_files': all_files[:50]  # First 50 files for quick reference
        }

    async def read_multiple_files(
        self,
        file_paths: List[str],
        max_lines_per_file: int = 100
    ) -> Dict[str, str]:
        """
        Read multiple files at once (useful for getting full architecture context)

        Args:
            file_paths: List of file paths to read (can also accept comma-separated string)
            max_lines_per_file: Max lines to read from each file (default: 100)

        Returns:
            Dict mapping file paths to their content
        """
        # Handle case where agent passes string instead of list
        if isinstance(file_paths, str):
            import json
            # Try parsing as JSON list
            try:
                file_paths = json.loads(file_paths)
            except:
                # Try comma-separated
                file_paths = [p.strip() for p in file_paths.split(',')]

        if not isinstance(file_paths, list):
            return {"error": "file_paths must be a list or comma-separated string"}

        results = {}

        for file_path in file_paths[:20]:  # Limit to 20 files to avoid token overflow
            try:
                self._validate_file_access(file_path)
                full_path = self.upload_dir / file_path

                if not full_path.exists():
                    results[file_path] = f"ERROR: File not found"
                    continue

                with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                    lines = f.readlines()

                # Read first N lines
                content_lines = []
                for i, line in enumerate(lines[:max_lines_per_file]):
                    content_lines.append(f"{i+1:5d} | {line.rstrip()}")

                if len(lines) > max_lines_per_file:
                    content_lines.append(f"\n... ({len(lines)} total lines, showing first {max_lines_per_file})")

                results[file_path] = '\n'.join(content_lines)

            except FileAccessError:
                results[file_path] = "ACCESS DENIED"
            except Exception as e:
                results[file_path] = f"ERROR: {str(e)}"

        return results

    async def get_full_codebase_content(
        self,
        max_files: int = 1000,
        max_lines_per_file: int = 10000,
        file_extensions: Optional[List[str]] = None
    ) -> str:
        """
        Get COMPLETE codebase content in one call - all files concatenated

        This is the MOST EFFICIENT way to get full codebase for architecture/business logic analysis.
        Instead of multiple tool calls, this reads everything at once.

        Args:
            max_files: Maximum number of files to read (default: 1000 - essentially unlimited)
            max_lines_per_file: Max lines per file (default: 10000 - essentially unlimited)
            file_extensions: Optional list of extensions to filter (e.g., [".py", ".js"])

        Returns:
            Complete codebase content with clear file separators
        """
        result_parts = []
        files_read = 0
        total_lines = 0

        result_parts.append("=" * 80)
        result_parts.append("COMPLETE CODEBASE CONTENT")
        result_parts.append(f"Total analyzed files: {len(self.analyzed_files)}")
        result_parts.append("=" * 80)
        result_parts.append("")

        # Sort files for consistent ordering
        sorted_files = sorted(self.analyzed_files)

        for file_path in sorted_files:
            if files_read >= max_files:
                result_parts.append(f"\n... (Limit reached: {max_files} files shown, {len(sorted_files) - max_files} files omitted)")
                break

            # Filter by extension if specified
            if file_extensions:
                if not any(file_path.endswith(ext) for ext in file_extensions):
                    continue

            try:
                full_path = self.upload_dir / file_path

                if not full_path.exists():
                    continue

                with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                    lines = f.readlines()

                # Add file header
                result_parts.append("\n" + "=" * 80)
                result_parts.append(f"FILE: {file_path}")
                result_parts.append(f"Lines: {len(lines)}")
                result_parts.append("=" * 80)

                # Add file content (limited)
                lines_to_show = min(len(lines), max_lines_per_file)
                for i, line in enumerate(lines[:lines_to_show]):
                    result_parts.append(f"{i+1:5d} | {line.rstrip()}")

                if len(lines) > max_lines_per_file:
                    result_parts.append(f"\n... ({len(lines) - max_lines_per_file} more lines omitted)")

                result_parts.append("")  # Blank line between files

                files_read += 1
                total_lines += len(lines)

            except Exception as e:
                result_parts.append(f"\nERROR reading {file_path}: {str(e)}\n")

        # Add summary footer
        result_parts.append("\n" + "=" * 80)
        result_parts.append(f"SUMMARY: Read {files_read} files, {total_lines} total lines")
        result_parts.append("=" * 80)

        return '\n'.join(result_parts)

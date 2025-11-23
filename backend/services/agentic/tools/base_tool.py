"""
Base class for all tools with file access control

Ensures agents can only access files that were analyzed
"""
from pathlib import Path
from typing import Dict, Set, List, Any, Optional
import os


class FileAccessError(Exception):
    """Raised when trying to access non-analyzed file"""
    pass


class ToolBase:
    """
    Base class for all tools with file access control

    All tools inherit from this to ensure proper access control
    """

    def __init__(self, upload_dir: Path, analysis_results: Dict[str, Any]):
        self.upload_dir = upload_dir
        self.analysis_results = analysis_results

        # Build whitelist of analyzed files
        self.analyzed_files = self._build_file_whitelist()

        print(f"[ToolBase] Initialized with {len(self.analyzed_files)} analyzed files")
        print(f"[ToolBase] Upload directory: {self.upload_dir}")

    def _build_file_whitelist(self) -> Set[str]:
        """
        Build set of files that were actually analyzed
        Only these files can be accessed by tools
        """
        whitelist = set()

        # DEBUG: Check what keys exist in analysis_results
        print(f"[ToolBase] analysis_results keys: {list(self.analysis_results.keys())}")

        # Method 1: From 'files' array
        files_array = self.analysis_results.get('files', [])
        print(f"[ToolBase] files array count: {len(files_array)}")
        for file_info in files_array:
            whitelist.add(file_info['path'])

        # Method 2: From 'metrics.by_file' (backup/additional)
        metrics = self.analysis_results.get('metrics', {})
        by_file = metrics.get('by_file', [])
        print(f"[ToolBase] metrics.by_file count: {len(by_file)}")

        if len(by_file) > 0:
            print(f"[ToolBase] Sample by_file entry: {by_file[0]}")

        for file_metric in by_file:
            file_path = file_metric.get('file')
            if file_path:
                whitelist.add(file_path)
            else:
                print(f"[ToolBase] WARNING: by_file entry missing 'file' key: {file_metric}")

        print(f"[ToolBase] Raw whitelist size before normalization: {len(whitelist)}")

        # Normalize paths (handle both / and \ on Windows)
        normalized = set()
        for path in whitelist:
            # Convert to forward slashes for consistency
            normalized_path = str(Path(path)).replace('\\', '/')
            normalized.add(normalized_path)

        return normalized

    def _normalize_path(self, file_path: str) -> str:
        """Normalize path for comparison"""
        return str(Path(file_path)).replace('\\', '/')

    def _is_file_accessible(self, file_path: str) -> bool:
        """Check if file was analyzed and can be accessed"""
        normalized_path = self._normalize_path(file_path)
        return normalized_path in self.analyzed_files

    def _validate_file_access(self, file_path: str) -> None:
        """
        Validate file access, raise error if not allowed

        Raises:
            FileAccessError: If file was not analyzed
        """
        if not self._is_file_accessible(file_path):
            raise FileAccessError(
                f"File '{file_path}' was not analyzed and cannot be accessed. "
                f"Only files matching selected extensions can be read. "
                f"Total analyzed files: {len(self.analyzed_files)}"
            )

    def _get_file_metadata(self, file_path: str) -> Dict[str, Any]:
        """Get file metadata from analysis results"""
        normalized_path = self._normalize_path(file_path)

        # From files array
        for file_info in self.analysis_results.get('files', []):
            if self._normalize_path(file_info['path']) == normalized_path:
                return file_info

        # From metrics
        for file_metric in self.analysis_results.get('metrics', {}).get('by_file', []):
            if self._normalize_path(file_metric['file']) == normalized_path:
                return file_metric

        return {}

    def list_accessible_files(self) -> List[str]:
        """Return list of files that can be accessed"""
        return sorted(self.analyzed_files)

"""
Base analyzer interface for file-type specific analyzers
Provides a plugin architecture for modular, extensible code analysis
"""
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Dict, List, Any, Optional


class BaseFileAnalyzer(ABC):
    """
    Abstract base class for all file-type specific analyzers

    This provides a consistent interface for:
    - BMS (Basic Mapping Support)
    - JCL (Job Control Language)
    - Copybooks
    - COBOL programs
    - SQL scripts
    - And any future file types
    """

    def __init__(self):
        """Initialize the analyzer"""
        self.name = self.get_analyzer_name()
        self.file_types = self.get_file_types()
        self.enabled = True

    @abstractmethod
    def can_analyze(self, file_path: Path) -> bool:
        """
        Determine if this analyzer can handle the given file

        Args:
            file_path: Path to the file to analyze

        Returns:
            bool: True if this analyzer can handle the file
        """
        pass

    @abstractmethod
    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Analyze a file and extract relevant information

        Args:
            file_path: Path to the file to analyze
            context: Optional context information (upload_id, analysis_run_id, etc.)

        Returns:
            Dict containing analysis results with standard structure:
            {
                'file': str,
                'file_type': str,
                'language': str,
                'analyzer': str,
                'success': bool,
                'error': Optional[str],
                'data': Dict[str, Any]  # Analyzer-specific data
            }
        """
        pass

    @abstractmethod
    def get_file_types(self) -> List[str]:
        """
        Get the file types (extensions) this analyzer handles

        Returns:
            List of file extensions (e.g., ['.bms', '.BMS'])
        """
        pass

    @abstractmethod
    def get_analyzer_name(self) -> str:
        """
        Get the name of this analyzer

        Returns:
            str: Analyzer name (e.g., 'BMS', 'JCL', 'COBOL')
        """
        pass

    def get_priority(self) -> int:
        """
        Get the priority of this analyzer (lower number = higher priority)
        Used when multiple analyzers can handle the same file type

        Returns:
            int: Priority (default 100)
        """
        return 100

    def is_enabled(self) -> bool:
        """
        Check if this analyzer is enabled

        Returns:
            bool: True if enabled
        """
        return self.enabled

    def set_enabled(self, enabled: bool):
        """
        Enable or disable this analyzer

        Args:
            enabled: True to enable, False to disable
        """
        self.enabled = enabled

    def validate_result(self, result: Dict[str, Any]) -> bool:
        """
        Validate that the analysis result has the required structure

        Args:
            result: Analysis result dictionary

        Returns:
            bool: True if result is valid
        """
        required_keys = ['file', 'file_type', 'language', 'analyzer', 'success']
        return all(key in result for key in required_keys)

    def create_result(
        self,
        file_path: Path,
        file_type: str,
        language: str,
        data: Dict[str, Any],
        error: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Helper method to create a standardized result dictionary

        Args:
            file_path: Path to the analyzed file
            file_type: Type of file (e.g., 'BMS Mapset')
            language: Language (e.g., 'BMS', 'JCL')
            data: Analyzer-specific data
            error: Optional error message

        Returns:
            Standardized result dictionary
        """
        return {
            'file': str(file_path),
            'file_type': file_type,
            'language': language,
            'analyzer': self.get_analyzer_name(),
            'success': error is None,
            'error': error,
            'data': data
        }

    def __repr__(self):
        return f"<{self.get_analyzer_name()}Analyzer enabled={self.enabled}>"


class AnalyzerMetadata:
    """Metadata about an analyzer for registry purposes"""

    def __init__(
        self,
        name: str,
        analyzer_class: type,
        file_types: List[str],
        priority: int = 100,
        enabled: bool = True
    ):
        self.name = name
        self.analyzer_class = analyzer_class
        self.file_types = file_types
        self.priority = priority
        self.enabled = enabled
        self.instance = None

    def get_instance(self) -> BaseFileAnalyzer:
        """Get or create an instance of the analyzer"""
        if self.instance is None:
            self.instance = self.analyzer_class()
            self.instance.set_enabled(self.enabled)
        return self.instance

    def __repr__(self):
        return f"<AnalyzerMetadata name={self.name} types={self.file_types} enabled={self.enabled}>"

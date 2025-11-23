"""
Analyzer Registry - Auto-discovery and routing for file analyzers
Provides a plug-and-play system for enabling/disabling analyzers
"""
import os
import importlib
import inspect
from pathlib import Path
from typing import Dict, List, Optional, Type, Any
import yaml
import logging

from .base_analyzer import BaseFileAnalyzer, AnalyzerMetadata


logger = logging.getLogger(__name__)


class AnalyzerRegistry:
    """
    Central registry for all file analyzers
    - Auto-discovers analyzer classes
    - Routes files to appropriate analyzers
    - Manages analyzer configuration and lifecycle
    """

    _instance = None

    def __new__(cls):
        """Singleton pattern - only one registry instance"""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        """Initialize the registry (only once)"""
        if self._initialized:
            return

        self.analyzers: Dict[str, AnalyzerMetadata] = {}
        self.file_type_map: Dict[str, List[str]] = {}  # extension -> [analyzer_names]
        self.config_path = Path(__file__).parent.parent / 'config' / 'analyzer_config.yaml'
        self.config = {}

        self._load_config()
        self._discover_analyzers()
        self._initialized = True

        logger.info(f"AnalyzerRegistry initialized with {len(self.analyzers)} analyzers")

    def _load_config(self):
        """Load analyzer configuration from YAML file"""
        try:
            if self.config_path.exists():
                with open(self.config_path, 'r') as f:
                    self.config = yaml.safe_load(f) or {}
                logger.info(f"Loaded analyzer config from {self.config_path}")
            else:
                logger.warning(f"Config file not found: {self.config_path}")
                self.config = {'analyzers': {}}
        except Exception as e:
            logger.error(f"Failed to load config: {e}")
            self.config = {'analyzers': {}}

    def _discover_analyzers(self):
        """
        Auto-discover all analyzer classes in the analyzers directory
        Looks for classes that extend BaseFileAnalyzer
        """
        analyzers_dir = Path(__file__).parent
        logger.info(f"Discovering analyzers in {analyzers_dir}")

        # Get all Python files in analyzers directory
        for file_path in analyzers_dir.glob('*.py'):
            if file_path.stem in ['__init__', 'base_analyzer', 'analyzer_registry']:
                continue

            module_name = f"analyzers.{file_path.stem}"

            try:
                # Import the module
                module = importlib.import_module(module_name)

                # Find all classes that extend BaseFileAnalyzer
                for name, obj in inspect.getmembers(module, inspect.isclass):
                    if (obj != BaseFileAnalyzer and
                        issubclass(obj, BaseFileAnalyzer) and
                        not inspect.isabstract(obj)):

                        # Register the analyzer
                        self._register_analyzer(obj)
                        logger.info(f"Discovered analyzer: {name} from {module_name}")

            except Exception as e:
                logger.error(f"Error discovering analyzers in {module_name}: {e}")

    def _register_analyzer(self, analyzer_class: Type[BaseFileAnalyzer]):
        """
        Register an analyzer class

        Args:
            analyzer_class: The analyzer class to register
        """
        try:
            # Create a temporary instance to get metadata
            temp_instance = analyzer_class()
            name = temp_instance.get_analyzer_name()
            file_types = temp_instance.get_file_types()
            priority = temp_instance.get_priority()

            # Check config for this analyzer
            analyzer_config = self.config.get('analyzers', {}).get(name.lower(), {})
            enabled = analyzer_config.get('enabled', True)
            priority = analyzer_config.get('priority', priority)

            # Create metadata
            metadata = AnalyzerMetadata(
                name=name,
                analyzer_class=analyzer_class,
                file_types=file_types,
                priority=priority,
                enabled=enabled
            )

            # Store in registry
            self.analyzers[name] = metadata

            # Map file extensions to this analyzer
            for ext in file_types:
                ext_lower = ext.lower()
                if ext_lower not in self.file_type_map:
                    self.file_type_map[ext_lower] = []
                self.file_type_map[ext_lower].append(name)

            logger.info(f"Registered analyzer: {name} for {file_types} (priority={priority}, enabled={enabled})")

        except Exception as e:
            logger.error(f"Failed to register analyzer {analyzer_class}: {e}")

    def get_analyzer_for_file(self, file_path: Path) -> Optional[BaseFileAnalyzer]:
        """
        Get the appropriate analyzer for a given file

        Args:
            file_path: Path to the file

        Returns:
            BaseFileAnalyzer instance or None if no analyzer found
        """
        extension = file_path.suffix.lower()

        # Get all analyzers that can handle this extension
        analyzer_names = self.file_type_map.get(extension, [])

        if not analyzer_names:
            logger.debug(f"No analyzer found for extension: {extension}")
            return None

        # Sort by priority (lower number = higher priority)
        analyzer_names = sorted(
            analyzer_names,
            key=lambda name: self.analyzers[name].priority
        )

        # Return the first enabled analyzer that can handle the file
        for name in analyzer_names:
            metadata = self.analyzers[name]
            if metadata.enabled:
                instance = metadata.get_instance()
                if instance.can_analyze(file_path):
                    logger.debug(f"Selected {name} analyzer for {file_path}")
                    return instance

        logger.debug(f"No enabled analyzer found for {file_path}")
        return None

    def get_analyzer_by_name(self, name: str) -> Optional[BaseFileAnalyzer]:
        """
        Get an analyzer by name

        Args:
            name: Name of the analyzer

        Returns:
            BaseFileAnalyzer instance or None
        """
        metadata = self.analyzers.get(name)
        if metadata and metadata.enabled:
            return metadata.get_instance()
        return None

    def get_all_analyzers(self) -> List[BaseFileAnalyzer]:
        """
        Get all enabled analyzers

        Returns:
            List of enabled analyzer instances
        """
        return [
            meta.get_instance()
            for meta in self.analyzers.values()
            if meta.enabled
        ]

    def get_supported_file_types(self) -> Dict[str, List[str]]:
        """
        Get all supported file types grouped by analyzer

        Returns:
            Dict mapping analyzer name to list of file extensions
        """
        result = {}
        for name, metadata in self.analyzers.items():
            if metadata.enabled:
                result[name] = metadata.file_types
        return result

    def enable_analyzer(self, name: str):
        """Enable an analyzer by name"""
        if name in self.analyzers:
            self.analyzers[name].enabled = True
            instance = self.analyzers[name].get_instance()
            instance.set_enabled(True)
            logger.info(f"Enabled analyzer: {name}")

    def disable_analyzer(self, name: str):
        """Disable an analyzer by name"""
        if name in self.analyzers:
            self.analyzers[name].enabled = False
            instance = self.analyzers[name].get_instance()
            instance.set_enabled(False)
            logger.info(f"Disabled analyzer: {name}")

    def get_registry_info(self) -> Dict[str, Any]:
        """
        Get information about the registry state

        Returns:
            Dict with registry information
        """
        return {
            'total_analyzers': len(self.analyzers),
            'enabled_analyzers': sum(1 for m in self.analyzers.values() if m.enabled),
            'analyzers': {
                name: {
                    'enabled': meta.enabled,
                    'priority': meta.priority,
                    'file_types': meta.file_types
                }
                for name, meta in self.analyzers.items()
            },
            'supported_extensions': list(self.file_type_map.keys())
        }

    def __repr__(self):
        enabled_count = sum(1 for m in self.analyzers.values() if m.enabled)
        return f"<AnalyzerRegistry analyzers={len(self.analyzers)} enabled={enabled_count}>"


# Global registry instance
_registry = None


def get_registry() -> AnalyzerRegistry:
    """Get the global analyzer registry instance"""
    global _registry
    if _registry is None:
        _registry = AnalyzerRegistry()
    return _registry


def reset_registry():
    """Reset the global registry (mainly for testing)"""
    global _registry
    _registry = None

"""
Language Router - Automatically routes to the appropriate language analyzer
Uses the modular analyzer registry for plug-and-play support
"""
from pathlib import Path
from typing import Dict, Any, Optional
from .analyzer_registry import get_registry
from .sql_analyzer import SQLAnalyzer
from .as400_analyzer import AS400Analyzer


class LanguageRouter:
    """Routes files to appropriate language-specific analyzers"""

    def __init__(self):
        # Use the new analyzer registry for modular file type support
        self.registry = get_registry()

        # Keep legacy analyzers for SQL and AS400 (not yet migrated)
        self.legacy_analyzers = {
            'sql': SQLAnalyzer(),
            'as400': AS400Analyzer(),
        }

        # Legacy language map for AS400/SQL (will be migrated later)
        self.legacy_language_map = {
            '.sql': 'sql',
            '.ddl': 'sql',
            '.dml': 'sql',
            '.pls': 'plsql',
            '.pkb': 'plsql',
            '.pks': 'plsql',
            '.rpg': 'as400',
            '.rpgle': 'as400',
            '.rpglec': 'as400',
            '.sqlrpgle': 'as400',
            '.rpg4': 'as400',
            '.rpgiv': 'as400',
            '.dspf': 'as400',
            '.prtf': 'as400',
            '.lf': 'as400',
            '.pf': 'as400',
            '.cmd': 'as400',
            '.clle': 'as400',
        }

    def analyze_file(self, file_path: Path, context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Analyze a file using the appropriate language analyzer
        First tries the new modular registry, then falls back to legacy analyzers
        """
        ext = file_path.suffix.lower()

        # Try the new modular registry first (COBOL, BMS, JCL, Copybook)
        analyzer = self.registry.get_analyzer_for_file(file_path)
        if analyzer:
            return analyzer.analyze_file(file_path, context)

        # Fall back to legacy analyzers for SQL and AS400
        language = self.legacy_language_map.get(ext, 'unknown')
        if language in self.legacy_analyzers:
            legacy_analyzer = self.legacy_analyzers[language]
            return legacy_analyzer.analyze_file(file_path)

        # Final fallback to basic analysis
        return self._basic_analysis(file_path, ext)

    def _basic_analysis(self, file_path: Path, language: str) -> Dict[str, Any]:
        """
        Provide basic analysis for unsupported languages
        """
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            return {
                'file': str(file_path),
                'language': language,
                'supported': False,
                'metrics': {
                    'total_lines': len(lines),
                    'blank_lines': sum(1 for line in lines if not line.strip()),
                    'size_bytes': len(content)
                },
                'note': f'Specialized analyzer not available for {language}. Basic metrics only.'
            }
        except Exception as e:
            return {
                'file': str(file_path),
                'language': language,
                'error': str(e)
            }

    def get_supported_languages(self) -> Dict[str, list]:
        """
        Get list of supported languages and their extensions
        Uses the new modular analyzer registry
        """
        registry_info = self.registry.get_registry_info()

        fully_supported = {}

        # Add modular analyzers from registry
        for analyzer_name, info in registry_info['analyzers'].items():
            if info['enabled']:
                fully_supported[analyzer_name] = info['file_types']

        # Add legacy analyzers
        fully_supported['SQL'] = ['.sql', '.ddl', '.dml', '.pls', '.pkb', '.pks']
        fully_supported['AS400/RPG'] = ['.rpg', '.rpgle', '.rpglec', '.sqlrpgle', '.rpg4', '.rpgiv',
                                        '.dspf', '.prtf', '.lf', '.pf', '.cmd', '.clle']

        supported = {
            'fully_supported': fully_supported,
            'basic_support': {},
            'registry_info': registry_info
        }
        return supported

    def batch_analyze(self, file_paths: list) -> Dict[str, Any]:
        """
        Analyze multiple files and aggregate results
        """
        results = {
            'files': [],
            'summary': {
                'total_files': len(file_paths),
                'by_language': {},
                'fully_analyzed': 0,
                'basic_analyzed': 0,
                'errors': 0
            }
        }

        for file_path in file_paths:
            try:
                analysis = self.analyze_file(Path(file_path))
                results['files'].append(analysis)

                # Update summary
                language = analysis.get('language', 'unknown')
                if language not in results['summary']['by_language']:
                    results['summary']['by_language'][language] = 0
                results['summary']['by_language'][language] += 1

                if analysis.get('supported', True):
                    results['summary']['fully_analyzed'] += 1
                else:
                    results['summary']['basic_analyzed'] += 1

                if 'error' in analysis:
                    results['summary']['errors'] += 1

            except Exception as e:
                results['files'].append({
                    'file': str(file_path),
                    'error': str(e)
                })
                results['summary']['errors'] += 1

        return results


# Convenience function for single file analysis
def analyze_code_file(file_path: str) -> Dict[str, Any]:
    """
    Analyze a single code file

    Args:
        file_path: Path to the code file

    Returns:
        Dictionary containing analysis results
    """
    router = LanguageRouter()
    return router.analyze_file(Path(file_path))


# Convenience function for batch analysis
def analyze_codebase(directory: str, extensions: Optional[list] = None) -> Dict[str, Any]:
    """
    Analyze all code files in a directory

    Args:
        directory: Path to the directory
        extensions: List of file extensions to analyze (e.g., ['.py', '.java'])
                   If None, analyzes all supported extensions

    Returns:
        Dictionary containing aggregated analysis results
    """
    router = LanguageRouter()
    base_path = Path(directory)

    # Collect all files
    files = []
    for file_path in base_path.rglob('*'):
        if file_path.is_file():
            if extensions is None or file_path.suffix.lower() in extensions:
                files.append(file_path)

    return router.batch_analyze(files)

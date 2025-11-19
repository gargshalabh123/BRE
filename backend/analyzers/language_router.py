"""
Language Router - Automatically routes to the appropriate language analyzer
"""
from pathlib import Path
from typing import Dict, Any, Optional
from .cobol_analyzer import COBOLAnalyzer
from .sql_analyzer import SQLAnalyzer
from .as400_analyzer import AS400Analyzer


class LanguageRouter:
    """Routes files to appropriate language-specific analyzers"""

    LANGUAGE_MAP = {
        # COBOL and mainframe files
        '.cbl': 'cobol',
        '.cob': 'cobol',
        '.cobol': 'cobol',
        '.cpy': 'cobol',      # COBOL Copybook
        '.jcl': 'jcl',        # Job Control Language
        '.bms': 'bms',        # Basic Mapping Support (CICS)
        '.prc': 'cobol',      # COBOL Procedure
        '.dclgen': 'cobol',   # DB2 DCLGEN
        '.mfs': 'mfs',        # IMS MFS

        # SQL and Database
        '.sql': 'sql',
        '.ddl': 'sql',
        '.dml': 'sql',
        '.pls': 'plsql',      # PL/SQL
        '.pkb': 'plsql',      # PL/SQL Package Body
        '.pks': 'plsql',      # PL/SQL Package Spec

        # AS400/RPG
        '.rpg': 'as400',
        '.rpgle': 'as400',
        '.rpglec': 'as400',
        '.sqlrpgle': 'as400',
        '.rpg4': 'as400',
        '.rpgiv': 'as400',
        '.dspf': 'as400',     # Display file
        '.prtf': 'as400',     # Printer file
        '.lf': 'as400',       # Logical file
        '.pf': 'as400',       # Physical file
        '.cmd': 'as400',      # CL Command
        '.clle': 'as400',     # CL ILE
    }

    def __init__(self):
        self.analyzers = {
            'cobol': COBOLAnalyzer(),
            'sql': SQLAnalyzer(),
            'as400': AS400Analyzer(),
        }

    def analyze_file(self, file_path: Path) -> Dict[str, Any]:
        """
        Analyze a file using the appropriate language analyzer
        """
        ext = file_path.suffix.lower()
        language = self.LANGUAGE_MAP.get(ext, 'unknown')

        # Route to specialized analyzer if available
        if language in self.analyzers:
            analyzer = self.analyzers[language]
            return analyzer.analyze_file(file_path)

        # Fallback to basic analysis
        return self._basic_analysis(file_path, language)

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
        """
        supported = {
            'fully_supported': {
                'COBOL': ['.cbl', '.cob', '.cobol', '.cpy'],
                'SQL': ['.sql', '.ddl', '.dml'],
                'AS400/RPG': ['.rpg', '.rpgle', '.rpglec', '.sqlrpgle', '.rpg4', '.rpgiv', '.dspf', '.prtf', '.lf', '.pf'],
            },
            'basic_support': {}
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

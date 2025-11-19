"""
Complexity Analyzer - Calculates code complexity metrics
"""
import re
from typing import Dict, List


class ComplexityAnalyzer:
    """Analyzes code complexity using Cyclomatic Complexity approach"""

    # Language-specific patterns for decision points
    COMPLEXITY_PATTERNS = {
        'cobol': {
            'if': r'\bIF\b',
            'else_if': r'\bELSE\s+IF\b',
            'evaluate': r'\bEVALUATE\b',
            'when': r'\bWHEN\b',
            'perform_until': r'\bPERFORM\b.*\bUNTIL\b',
            'perform_varying': r'\bPERFORM\s+VARYING\b',
            'and': r'\bAND\b',
            'or': r'\bOR\b',
            'search': r'\bSEARCH\b',
        },
        'python': {
            'if': r'\bif\b',
            'elif': r'\belif\b',
            'for': r'\bfor\b',
            'while': r'\bwhile\b',
            'except': r'\bexcept\b',
            'and': r'\band\b',
            'or': r'\bor\b',
            'with': r'\bwith\b',
            'assert': r'\bassert\b',
            'lambda': r'\blambda\b',
        },
        'java': {
            'if': r'\bif\b',
            'else_if': r'\belse\s+if\b',
            'for': r'\bfor\b',
            'while': r'\bwhile\b',
            'do': r'\bdo\b',
            'case': r'\bcase\b',
            'catch': r'\bcatch\b',
            'and': r'&&',
            'or': r'\|\|',
            'ternary': r'\?',
        },
        'javascript': {
            'if': r'\bif\b',
            'else_if': r'\belse\s+if\b',
            'for': r'\bfor\b',
            'while': r'\bwhile\b',
            'do': r'\bdo\b',
            'case': r'\bcase\b',
            'catch': r'\bcatch\b',
            'and': r'&&',
            'or': r'\|\|',
            'ternary': r'\?',
        },
        'c': {
            'if': r'\bif\b',
            'else_if': r'\belse\s+if\b',
            'for': r'\bfor\b',
            'while': r'\bwhile\b',
            'do': r'\bdo\b',
            'case': r'\bcase\b',
            'and': r'&&',
            'or': r'\|\|',
            'ternary': r'\?',
        },
        'cpp': {
            'if': r'\bif\b',
            'else_if': r'\belse\s+if\b',
            'for': r'\bfor\b',
            'while': r'\bwhile\b',
            'do': r'\bdo\b',
            'case': r'\bcase\b',
            'catch': r'\bcatch\b',
            'and': r'&&',
            'or': r'\|\|',
            'ternary': r'\?',
        },
    }

    @staticmethod
    def detect_language(file_path: str) -> str:
        """Detect programming language from file extension"""
        ext = file_path.lower().split('.')[-1]

        language_map = {
            'cbl': 'cobol',
            'cob': 'cobol',
            'cobol': 'cobol',
            'cpy': 'cobol',
            'py': 'python',
            'java': 'java',
            'js': 'javascript',
            'ts': 'javascript',
            'c': 'c',
            'cpp': 'cpp',
            'cc': 'cpp',
            'cxx': 'cpp',
            'h': 'c',
            'hpp': 'cpp',
        }

        return language_map.get(ext, 'unknown')

    @staticmethod
    def calculate_complexity(content: str, language: str) -> int:
        """
        Calculate Cyclomatic Complexity for given content

        Formula: Complexity = 1 (base) + number of decision points

        Args:
            content: Source code content
            language: Programming language

        Returns:
            Complexity score (integer)
        """
        if language not in ComplexityAnalyzer.COMPLEXITY_PATTERNS:
            return 0  # Unknown language

        complexity = 1  # Base complexity
        patterns = ComplexityAnalyzer.COMPLEXITY_PATTERNS[language]

        # Remove comments to avoid false positives
        cleaned_content = ComplexityAnalyzer._remove_comments(content, language)

        # Count decision points
        for pattern_name, pattern in patterns.items():
            try:
                matches = re.findall(pattern, cleaned_content, re.IGNORECASE if language == 'cobol' else 0)
                complexity += len(matches)
            except re.error:
                # Skip if regex pattern is invalid
                continue

        return complexity

    @staticmethod
    def _remove_comments(content: str, language: str) -> str:
        """Remove comments from code to avoid counting them in complexity"""

        if language == 'cobol':
            # Remove COBOL comments (lines starting with * in column 7)
            lines = content.split('\n')
            cleaned_lines = []
            for line in lines:
                # Skip comment lines (*)
                if len(line) > 6 and line[6] in ('*', '/'):
                    continue
                cleaned_lines.append(line)
            return '\n'.join(cleaned_lines)

        elif language == 'python':
            # Remove Python comments (# to end of line)
            content = re.sub(r'#.*$', '', content, flags=re.MULTILINE)
            # Remove docstrings
            content = re.sub(r'""".*?"""', '', content, flags=re.DOTALL)
            content = re.sub(r"'''.*?'''", '', content, flags=re.DOTALL)
            return content

        elif language in ('java', 'javascript', 'c', 'cpp'):
            # Remove single-line comments
            content = re.sub(r'//.*$', '', content, flags=re.MULTILINE)
            # Remove multi-line comments
            content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
            return content

        return content

    @staticmethod
    def get_complexity_rating(complexity: int) -> Dict[str, str]:
        """
        Get complexity rating and color based on score

        Returns:
            Dict with 'rating' and 'color' keys
        """
        if complexity <= 10:
            return {'rating': 'Simple', 'color': 'green'}
        elif complexity <= 20:
            return {'rating': 'Moderate', 'color': 'yellow'}
        elif complexity <= 50:
            return {'rating': 'Complex', 'color': 'orange'}
        else:
            return {'rating': 'Very Complex', 'color': 'red'}

    @staticmethod
    def analyze_file(file_path: str, content: str) -> Dict:
        """
        Analyze a single file and return complexity metrics

        Args:
            file_path: Path to the file
            content: File content

        Returns:
            Dict with complexity metrics
        """
        language = ComplexityAnalyzer.detect_language(file_path)
        complexity = ComplexityAnalyzer.calculate_complexity(content, language)
        rating_info = ComplexityAnalyzer.get_complexity_rating(complexity)

        return {
            'file': file_path,
            'language': language,
            'complexity': complexity,
            'rating': rating_info['rating'],
            'color': rating_info['color']
        }

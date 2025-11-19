"""
Core code analysis module for extracting metrics, patterns, and business rules
"""
import os
import re
from pathlib import Path
from typing import Dict, List, Any, Optional
from collections import defaultdict
import json

try:
    from radon.complexity import cc_visit
    from radon.metrics import mi_visit, h_visit
    from radon.raw import analyze
except ImportError:
    print("Warning: radon not installed. Some metrics will be unavailable.")

try:
    import lizard
except ImportError:
    print("Warning: lizard not installed. Some metrics will be unavailable.")

try:
    from .language_router import LanguageRouter
except ImportError:
    print("Warning: language_router not available. Using basic analysis only.")
    LanguageRouter = None

try:
    from .complexity_analyzer import ComplexityAnalyzer
except ImportError:
    print("Warning: complexity_analyzer not available. Complexity metrics disabled.")
    ComplexityAnalyzer = None

try:
    from .dependency_analyzer import DependencyAnalyzer
except ImportError:
    print("Warning: dependency_analyzer not available. Basic dependencies only.")
    DependencyAnalyzer = None


class CodeAnalyzer:
    """Main code analysis engine"""

    # Binary/archive files to exclude from analysis
    BINARY_EXTENSIONS = {
        '.png', '.jpg', '.jpeg', '.gif', '.bmp', '.ico', '.svg', '.webp',  # Images
        '.zip', '.tar', '.gz', '.bz2', '.7z', '.rar', '.jar', '.war', '.ear',  # Archives
        '.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx',  # Documents
        '.exe', '.dll', '.so', '.dylib', '.bin', '.dat',  # Binaries
        '.mp3', '.mp4', '.avi', '.mov', '.wav', '.flac',  # Media
        '.class', '.o', '.a', '.pyc', '.pyo'  # Compiled code
    }

    # File type mappings (code and config files only)
    FILE_EXTENSIONS = {
        # COBOL and mainframe
        '.cbl': 'COBOL Program',
        '.cob': 'COBOL Program',
        '.cobol': 'COBOL Program',
        '.cpy': 'COBOL Copybook',
        '.jcl': 'JCL (Job Control Language)',
        '.bms': 'BMS (Basic Mapping Support)',
        '.prc': 'COBOL Procedure',
        '.dclgen': 'DB2 DCLGEN',
        '.mfs': 'IMS MFS',
        # AS/400 and RPG
        '.rpg': 'RPG',
        '.rpgle': 'RPG ILE',
        '.rpglec': 'RPG ILE',
        '.sqlrpgle': 'RPG with SQL',
        '.dspf': 'Display File',
        '.prtf': 'Print File',
        '.lf': 'Logical File',
        '.pf': 'Physical File',
        '.cmd': 'CL Command',
        '.clle': 'CL ILE',
        # SQL and Database
        '.sql': 'SQL',
        '.ddl': 'DDL (Data Definition)',
        '.dml': 'DML (Data Manipulation)',
        '.pls': 'PL/SQL',
        '.pkb': 'PL/SQL Package Body',
        '.pks': 'PL/SQL Package Spec',
        # Modern languages
        '.java': 'Java',
        '.py': 'Python',
        '.js': 'JavaScript',
        '.jsx': 'React JSX',
        '.ts': 'TypeScript',
        '.tsx': 'React TypeScript',
        '.c': 'C',
        '.cpp': 'C++',
        '.h': 'C/C++ Header',
        '.cs': 'C#',
        '.pl': 'Perl',
        '.rb': 'Ruby',
        '.php': 'PHP',
        '.vb': 'Visual Basic',
        '.go': 'Go',
        '.rs': 'Rust',
        # Scripts
        '.sh': 'Shell Script',
        '.bash': 'Bash Script',
        '.bat': 'Batch Script',
        '.ps1': 'PowerShell',
        # Config files
        '.xml': 'XML',
        '.json': 'JSON',
        '.yaml': 'YAML',
        '.yml': 'YAML',
        '.properties': 'Properties',
        '.conf': 'Config',
        '.cfg': 'Config',
        '.ini': 'INI Config',
    }

    def __init__(self, base_path: str):
        self.base_path = Path(base_path)
        self.files = []
        self.analysis_results = {}
        self.language_router = LanguageRouter() if LanguageRouter else None
        self.specialized_analysis_cache = {}

    def scan_directory(self) -> List[Dict[str, Any]]:
        """Scan directory and catalog all files (code and config only, exclude binaries)"""
        files_info = []

        for root, dirs, files in os.walk(self.base_path):
            # Skip hidden directories and common ignore patterns
            dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['node_modules', '__pycache__', 'venv', 'target', 'build']]

            for file in files:
                if file.startswith('.'):
                    continue

                file_path = Path(root) / file
                rel_path = file_path.relative_to(self.base_path)
                ext = file_path.suffix.lower()

                # Skip binary files (images, archives, compiled files, etc.)
                if ext in self.BINARY_EXTENSIONS:
                    continue

                file_info = {
                    'path': str(rel_path),
                    'name': file,
                    'extension': ext,
                    'type': self.FILE_EXTENSIONS.get(ext, 'Unknown'),
                    'size': file_path.stat().st_size,
                }

                files_info.append(file_info)
                self.files.append(file_path)

        return files_info

    def analyze_all(self) -> Dict[str, Any]:
        """Run comprehensive analysis on all files"""
        files_info = self.scan_directory()

        results = {
            'summary': self._generate_summary(files_info),
            'files': files_info,
            'metrics': self._calculate_metrics(),
            'dependencies': self._analyze_dependencies(),
            'detailed_dependencies': self._analyze_detailed_dependencies(),
            'database_operations': self._extract_database_operations(),
            'business_rules': self._extract_business_rules(),
        }

        return results

    def _generate_summary(self, files_info: List[Dict]) -> Dict[str, Any]:
        """Generate overall summary statistics"""
        type_counts = defaultdict(int)
        total_size = 0
        total_loc = 0

        for file_info in files_info:
            type_counts[file_info['type']] += 1
            total_size += file_info['size']

        return {
            'total_files': len(files_info),
            'total_size_bytes': total_size,
            'total_size_mb': round(total_size / (1024 * 1024), 2),
            'file_types': dict(type_counts),
        }

    def _calculate_metrics(self) -> Dict[str, Any]:
        """Calculate LOC and complexity metrics"""
        metrics = {
            'total_loc': 0,
            'total_sloc': 0,  # Source lines (non-blank, non-comment)
            'total_comments': 0,
            'total_blank': 0,
            'by_file': [],
            'complexity': {}
        }

        for file_path in self.files:
            try:
                file_metrics = self._analyze_file_metrics(file_path)
                if file_metrics:
                    metrics['by_file'].append(file_metrics)
                    metrics['total_loc'] += file_metrics.get('loc', 0)
                    metrics['total_sloc'] += file_metrics.get('sloc', 0)
                    metrics['total_comments'] += file_metrics.get('comments', 0)
                    metrics['total_blank'] += file_metrics.get('blank', 0)
            except Exception as e:
                print(f"Error analyzing {file_path}: {e}")

        return metrics

    def _analyze_file_metrics(self, file_path: Path) -> Optional[Dict[str, Any]]:
        """Analyze metrics for a single file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            rel_path = file_path.relative_to(self.base_path)
            ext = file_path.suffix.lower()

            metrics = {
                'file': str(rel_path),
                'loc': len(lines),
                'sloc': 0,
                'comments': 0,
                'blank': 0,
            }

            # Count blank lines
            for line in lines:
                stripped = line.strip()
                if not stripped:
                    metrics['blank'] += 1
                elif self._is_comment_line(stripped, ext):
                    metrics['comments'] += 1
                else:
                    metrics['sloc'] += 1

            # Calculate complexity using our ComplexityAnalyzer
            if ComplexityAnalyzer:
                try:
                    language = ComplexityAnalyzer.detect_language(str(rel_path))
                    complexity = ComplexityAnalyzer.calculate_complexity(content, language)
                    metrics['complexity'] = complexity

                    # Get complexity rating for additional metadata
                    rating_info = ComplexityAnalyzer.get_complexity_rating(complexity)
                    metrics['complexity_rating'] = rating_info['rating']
                    metrics['complexity_color'] = rating_info['color']
                except Exception as e:
                    # Fallback to 0 if complexity calculation fails
                    metrics['complexity'] = 0
            else:
                # Fallback: Try to get complexity for Python files using radon
                if ext == '.py':
                    try:
                        cc_results = cc_visit(content)
                        if cc_results:
                            metrics['complexity'] = sum(cc.complexity for cc in cc_results)
                            metrics['functions'] = len(cc_results)
                    except:
                        pass

            return metrics

        except Exception as e:
            return None

    def _is_comment_line(self, line: str, ext: str) -> bool:
        """Check if line is a comment based on file type"""
        comment_patterns = {
            '.py': ['#'],
            '.java': ['//', '/*', '*'],
            '.js': ['//', '/*', '*'],
            '.ts': ['//', '/*', '*'],
            '.c': ['//', '/*', '*'],
            '.cpp': ['//', '/*', '*'],
            '.cs': ['//', '/*', '*'],
            '.sql': ['--', '/*', '*'],
            '.cbl': ['*', 'C '],
            '.cob': ['*', 'C '],
        }

        patterns = comment_patterns.get(ext, [])
        return any(line.startswith(p) for p in patterns)

    def _get_specialized_analysis(self, file_path: Path) -> Optional[Dict[str, Any]]:
        """Get specialized analysis for a file if available"""
        if not self.language_router:
            return None

        # Check cache first
        file_key = str(file_path)
        if file_key in self.specialized_analysis_cache:
            return self.specialized_analysis_cache[file_key]

        try:
            analysis = self.language_router.analyze_file(file_path)
            # Only cache if it's a supported language with full analysis
            if analysis.get('supported', True) and 'error' not in analysis:
                self.specialized_analysis_cache[file_key] = analysis
                return analysis
        except Exception as e:
            print(f"Specialized analysis failed for {file_path}: {e}")

        return None

    def _analyze_dependencies(self) -> Dict[str, List[str]]:
        """Extract dependencies and imports (simple format for backward compatibility)"""
        dependencies = defaultdict(list)

        for file_path in self.files:
            try:
                # Try specialized analysis first for copybooks, etc.
                specialized = self._get_specialized_analysis(file_path)
                if specialized and 'copybooks' in specialized:
                    # Extract copybook dependencies for COBOL files
                    copybooks = specialized['copybooks']
                    if copybooks:
                        rel_path = str(file_path.relative_to(self.base_path))
                        dependencies[rel_path] = [cb['name'] for cb in copybooks]
                        continue

                # Fallback to generic dependency extraction
                deps = self._extract_file_dependencies(file_path)
                if deps:
                    rel_path = str(file_path.relative_to(self.base_path))
                    dependencies[rel_path] = deps
            except Exception as e:
                pass

        return dict(dependencies)

    def _analyze_detailed_dependencies(self) -> Dict[str, List[Dict[str, Any]]]:
        """
        Extract detailed dependencies with type, signature, and parameters

        Returns:
            Dict mapping file paths to lists of detailed dependency objects
        """
        detailed_dependencies = {}

        for file_path in self.files:
            try:
                rel_path = str(file_path.relative_to(self.base_path))

                if DependencyAnalyzer:
                    # Use enhanced dependency analyzer
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()

                    deps = DependencyAnalyzer.analyze_file_dependencies(file_path, content)
                    if deps:
                        detailed_dependencies[rel_path] = deps
                else:
                    # Fallback: convert simple dependencies to detailed format
                    specialized = self._get_specialized_analysis(file_path)
                    if specialized and 'copybooks' in specialized:
                        copybooks = specialized['copybooks']
                        detailed_dependencies[rel_path] = [
                            {
                                'target': cb['name'],
                                'type': 'COPYBOOK',
                                'line': cb.get('line', 0),
                                'signature': f"COPY {cb['name']}",
                                'parameters': [],
                                'description': f"Includes copybook {cb['name']}"
                            }
                            for cb in copybooks
                        ]
            except Exception as e:
                print(f"Error analyzing dependencies for {file_path}: {e}")
                pass

        return detailed_dependencies

    def _extract_file_dependencies(self, file_path: Path) -> List[str]:
        """Extract dependencies from a single file"""
        ext = file_path.suffix.lower()
        deps = []

        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()

            # Python imports
            if ext == '.py':
                deps.extend(re.findall(r'^\s*import\s+(\S+)', content, re.MULTILINE))
                deps.extend(re.findall(r'^\s*from\s+(\S+)\s+import', content, re.MULTILINE))

            # Java imports
            elif ext == '.java':
                deps.extend(re.findall(r'^\s*import\s+([^;]+);', content, re.MULTILINE))

            # JavaScript/TypeScript imports
            elif ext in ['.js', '.ts']:
                deps.extend(re.findall(r'import\s+.*?from\s+[\'"]([^\'"]+)[\'"]', content))
                deps.extend(re.findall(r'require\([\'"]([^\'"]+)[\'"]\)', content))

            # C/C++ includes
            elif ext in ['.c', '.cpp', '.h']:
                deps.extend(re.findall(r'#include\s+[<"]([^>"]+)[>"]', content))

        except Exception as e:
            pass

        return list(set(deps))

    def _extract_database_operations(self) -> Dict[str, Any]:
        """Extract database queries and operations"""
        db_operations = {
            'queries': [],
            'total_count': 0,
            'by_type': defaultdict(int),
        }

        specialized_count = 0
        generic_count = 0

        for file_path in self.files:
            try:
                # Try specialized analysis first
                specialized = self._get_specialized_analysis(file_path)
                if specialized and 'database' in specialized:
                    # Use specialized analyzer's database operations
                    db_ops = specialized['database']
                    if db_ops:
                        specialized_count += 1
                    for op in db_ops:
                        db_operations['queries'].append({
                            'file': str(file_path.relative_to(self.base_path)),
                            'query': op.get('statement', op.get('query', '')),
                            'type': op.get('type', 'UNKNOWN'),
                            'line': op.get('line', 0),
                            'category': op.get('category', 'SQL')
                        })
                        db_operations['by_type'][op.get('type', 'UNKNOWN')] += 1
                else:
                    # Fallback to generic SQL extraction
                    queries = self._extract_sql_from_file(file_path)
                    if queries:
                        generic_count += 1
                    for query in queries:
                        db_operations['queries'].append({
                            'file': str(file_path.relative_to(self.base_path)),
                            'query': query['query'],
                            'type': query['type'],
                            'line': query.get('line', 0)
                        })
                        db_operations['by_type'][query['type']] += 1
            except Exception as e:
                pass

        db_operations['total_count'] = len(db_operations['queries'])
        db_operations['by_type'] = dict(db_operations['by_type'])

        # Debug info
        print(f"[DEBUG] DB Operations - Specialized files: {specialized_count}, Generic files: {generic_count}, Total ops: {db_operations['total_count']}")

        return db_operations

    def _extract_sql_from_file(self, file_path: Path) -> List[Dict[str, Any]]:
        """Extract SQL queries from file"""
        queries = []

        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                lines = content.split('\n')

            # SQL patterns
            patterns = [
                (r'\b(SELECT\s+.+?(?:FROM|;))', 'SELECT'),
                (r'\b(INSERT\s+INTO\s+.+?(?:VALUES|;))', 'INSERT'),
                (r'\b(UPDATE\s+.+?SET\s+.+?(?:WHERE|;))', 'UPDATE'),
                (r'\b(DELETE\s+FROM\s+.+?(?:WHERE|;))', 'DELETE'),
                (r'\b(CREATE\s+(?:TABLE|INDEX|VIEW)\s+.+?;)', 'CREATE'),
                (r'\b(ALTER\s+TABLE\s+.+?;)', 'ALTER'),
                (r'\b(DROP\s+(?:TABLE|INDEX|VIEW)\s+.+?;)', 'DROP'),
            ]

            for pattern, query_type in patterns:
                matches = re.finditer(pattern, content, re.IGNORECASE | re.DOTALL)
                for match in matches:
                    query_text = match.group(1).strip()
                    # Find line number
                    line_num = content[:match.start()].count('\n') + 1

                    queries.append({
                        'query': query_text[:200],  # Truncate long queries
                        'type': query_type,
                        'line': line_num
                    })

        except Exception as e:
            pass

        return queries

    def _extract_business_rules(self) -> List[Dict[str, Any]]:
        """Extract potential business rules using patterns"""
        rules = []

        # Common business rule patterns
        patterns = [
            (r'\bif\s+.*?(?:amount|price|total|balance|rate|discount)', 'Financial Rule'),
            (r'\bif\s+.*?(?:age|date|year|month|day)', 'Date/Age Rule'),
            (r'\bif\s+.*?(?:status|state|flag|enabled|disabled)', 'Status Rule'),
            (r'\bvalidate\w*\s*\(', 'Validation Rule'),
            (r'\bcalculate\w*\s*\(', 'Calculation Rule'),
            (r'\bcheck\w*\s*\(', 'Check Rule'),
            (r'(?:MIN|MAX|LIMIT)\s*=\s*\d+', 'Threshold Rule'),
        ]

        for file_path in self.files:
            try:
                # Try specialized analysis first
                specialized = self._get_specialized_analysis(file_path)
                if specialized and 'business_rules' in specialized:
                    # Use specialized analyzer's business rules
                    for rule in specialized['business_rules']:
                        rules.append({
                            'file': str(file_path.relative_to(self.base_path)),
                            'line': rule.get('line', 0),
                            'type': rule.get('type', 'Business Rule'),
                            'code': rule.get('code', '')[:150]
                        })
                else:
                    # Fallback to generic pattern matching
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        lines = content.split('\n')

                    for i, line in enumerate(lines, 1):
                        for pattern, rule_type in patterns:
                            if re.search(pattern, line, re.IGNORECASE):
                                rules.append({
                                    'file': str(file_path.relative_to(self.base_path)),
                                    'line': i,
                                    'type': rule_type,
                                    'code': line.strip()[:150]
                                })

            except Exception as e:
                pass

        return rules[:100]  # Limit to first 100 rules

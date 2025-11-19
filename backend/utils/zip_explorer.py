"""
ZIP File Explorer and Analyzer
Provides comprehensive analysis of zip file contents including:
- File/folder structure
- Lines of Code (LOC) metrics
- File metadata (size, type, dates)
- Language detection
- Summary statistics
"""
import zipfile
import os
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Any, Optional
from datetime import datetime
from collections import defaultdict
import mimetypes

from analyzers.language_router import LanguageRouter


class ZipExplorer:
    """Explore and analyze contents of ZIP files"""

    def __init__(self, zip_path: str):
        """
        Initialize ZIP explorer

        Args:
            zip_path: Path to the ZIP file
        """
        self.zip_path = Path(zip_path)
        if not self.zip_path.exists():
            raise FileNotFoundError(f"ZIP file not found: {zip_path}")

        self.language_router = LanguageRouter()
        self.temp_dir = None

    def explore(self, extract: bool = False) -> Dict[str, Any]:
        """
        Explore the ZIP file and return comprehensive analysis

        Args:
            extract: If True, extract files for detailed analysis

        Returns:
            Dictionary containing complete analysis results
        """
        try:
            with zipfile.ZipFile(self.zip_path, 'r') as zip_ref:
                # Get basic ZIP info
                zip_info = self._get_zip_info(zip_ref)

                # Get file tree structure
                file_tree = self._build_file_tree(zip_ref)

                # Get file list with metadata
                files_metadata = self._get_files_metadata(zip_ref)

                # Calculate statistics
                statistics = self._calculate_statistics(files_metadata)

                # Get language distribution
                language_dist = self._get_language_distribution(files_metadata)

                # Detailed analysis if extraction is enabled
                detailed_analysis = None
                if extract:
                    detailed_analysis = self._perform_detailed_analysis(zip_ref)

                return {
                    'zip_info': zip_info,
                    'file_tree': file_tree,
                    'files': files_metadata,
                    'statistics': statistics,
                    'language_distribution': language_dist,
                    'detailed_analysis': detailed_analysis
                }
        except zipfile.BadZipFile:
            raise ValueError(f"Invalid ZIP file: {self.zip_path}")
        except Exception as e:
            raise Exception(f"Failed to explore ZIP file: {str(e)}")

    def _get_zip_info(self, zip_ref: zipfile.ZipFile) -> Dict[str, Any]:
        """Get basic information about the ZIP file"""
        zip_stat = os.stat(self.zip_path)

        return {
            'filename': self.zip_path.name,
            'path': str(self.zip_path.absolute()),
            'size_bytes': zip_stat.st_size,
            'size_mb': round(zip_stat.st_size / (1024 * 1024), 2),
            'created': datetime.fromtimestamp(zip_stat.st_ctime).isoformat(),
            'modified': datetime.fromtimestamp(zip_stat.st_mtime).isoformat(),
            'total_files': len(zip_ref.filelist),
            'compression': 'ZIP_DEFLATED' if zip_ref.compression == zipfile.ZIP_DEFLATED else 'ZIP_STORED'
        }

    def _build_file_tree(self, zip_ref: zipfile.ZipFile) -> Dict[str, Any]:
        """Build a hierarchical tree structure of files and folders"""
        tree = {'name': 'root', 'type': 'folder', 'children': {}}

        for file_info in zip_ref.filelist:
            parts = file_info.filename.split('/')
            current = tree['children']

            for i, part in enumerate(parts):
                if not part:  # Skip empty parts
                    continue

                is_file = i == len(parts) - 1 and not file_info.is_dir()

                if part not in current:
                    if is_file:
                        current[part] = {
                            'name': part,
                            'type': 'file',
                            'size': file_info.file_size,
                            'compressed_size': file_info.compress_size,
                            'full_path': file_info.filename
                        }
                    else:
                        current[part] = {
                            'name': part,
                            'type': 'folder',
                            'children': {}
                        }

                if not is_file and 'children' in current[part]:
                    current = current[part]['children']

        return self._tree_to_list(tree)

    def _tree_to_list(self, node: Dict[str, Any]) -> Dict[str, Any]:
        """Convert tree structure to a more readable format"""
        if node['type'] == 'file':
            return node

        result = {
            'name': node['name'],
            'type': node['type'],
            'children': []
        }

        for child in node.get('children', {}).values():
            result['children'].append(self._tree_to_list(child))

        return result

    def _get_files_metadata(self, zip_ref: zipfile.ZipFile) -> List[Dict[str, Any]]:
        """Get detailed metadata for all files in the ZIP"""
        files = []

        for file_info in zip_ref.filelist:
            if file_info.is_dir():
                continue

            # Get file extension and language
            path = Path(file_info.filename)
            ext = path.suffix.lower()
            language = self._detect_language(ext)

            # Calculate compression ratio
            compression_ratio = 0
            if file_info.file_size > 0:
                compression_ratio = round(
                    (1 - file_info.compress_size / file_info.file_size) * 100, 2
                )

            # Get MIME type
            mime_type, _ = mimetypes.guess_type(file_info.filename)

            # Determine if text file
            is_text = self._is_text_file(ext, mime_type)

            # Quick LOC estimation (only for text files, skip binary/images)
            estimated_loc = 0
            if is_text and language != 'unknown':
                estimated_loc = self._estimate_loc(file_info, zip_ref)

            files.append({
                'path': file_info.filename,
                'name': path.name,
                'directory': str(path.parent) if str(path.parent) != '.' else '',
                'extension': ext,
                'language': language,
                'size_bytes': file_info.file_size,
                'size_kb': round(file_info.file_size / 1024, 2),
                'compressed_size': file_info.compress_size,
                'compression_ratio': compression_ratio,
                'date_time': datetime(*file_info.date_time).isoformat() if file_info.date_time else None,
                'mime_type': mime_type or 'unknown',
                'estimated_loc': estimated_loc,
                'is_text': is_text
            })

        return files

    def _detect_language(self, extension: str) -> str:
        """Detect programming language from file extension"""
        lang_map = self.language_router.LANGUAGE_MAP
        return lang_map.get(extension, 'unknown')

    def _is_text_file(self, extension: str, mime_type: Optional[str]) -> bool:
        """Determine if file is a text file (code or config only)"""
        # Binary/archive files to explicitly exclude
        binary_extensions = {
            '.png', '.jpg', '.jpeg', '.gif', '.bmp', '.ico', '.svg', '.webp',  # Images
            '.zip', '.tar', '.gz', '.bz2', '.7z', '.rar', '.jar', '.war', '.ear',  # Archives
            '.pdf', '.doc', '.docx', '.xls', '.xlsx', '.ppt', '.pptx',  # Documents
            '.exe', '.dll', '.so', '.dylib', '.bin', '.dat',  # Binaries
            '.mp3', '.mp4', '.avi', '.mov', '.wav', '.flac',  # Media
            '.class', '.o', '.a', '.pyc', '.pyo'  # Compiled code
        }

        # Only allow code and config files
        text_extensions = {
            '.txt', '.md', '.rst', '.log', '.cfg', '.conf', '.ini', '.xml', '.json',
            '.yaml', '.yml', '.toml', '.sh', '.bash', '.bat', '.cmd', '.ps1', '.env', '.properties',
            # COBOL and mainframe
            '.cbl', '.cob', '.cobol', '.cpy', '.jcl', '.bms', '.prc', '.dclgen', '.mfs',
            # SQL and Database
            '.sql', '.ddl', '.dml', '.pls', '.pkb', '.pks',
            # RPG and AS/400
            '.rpg', '.rpgle', '.rpglec', '.sqlrpgle', '.rpg4', '.rpgiv',
            '.dspf', '.prtf', '.lf', '.pf', '.clle',
            # C/C++
            '.c', '.h', '.cpp', '.hpp', '.cc', '.cxx',
            # JVM languages
            '.java', '.kt', '.scala',
            # Python
            '.py', '.pyw', '.pyx',
            # JavaScript/TypeScript
            '.js', '.jsx', '.ts', '.tsx', '.mjs',
            # .NET
            '.cs', '.vb', '.fs',
            # Other languages
            '.go', '.rs', '.swift', '.rb', '.php', '.pl', '.r'
        }

        # Explicitly reject binary files first
        if extension in binary_extensions:
            return False

        # Accept known code/config extensions
        if extension in text_extensions:
            return True

        # Don't rely on mime type alone - be conservative
        return False

    def _estimate_loc(self, file_info: zipfile.ZipInfo, zip_ref: zipfile.ZipFile) -> int:
        """Estimate lines of code by counting newlines"""
        try:
            # Only estimate for files smaller than 10MB
            if file_info.file_size > 10 * 1024 * 1024:
                return 0

            content = zip_ref.read(file_info.filename)
            try:
                text = content.decode('utf-8', errors='ignore')
                return len(text.split('\n'))
            except:
                return 0
        except:
            return 0

    def _calculate_statistics(self, files: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Calculate comprehensive statistics"""
        total_files = len(files)
        total_size = sum(f['size_bytes'] for f in files)
        total_compressed = sum(f['compressed_size'] for f in files)
        total_loc = sum(f['estimated_loc'] for f in files)

        # Count by extension
        by_extension = defaultdict(lambda: {'count': 0, 'size': 0, 'loc': 0})
        for file in files:
            ext = file['extension'] or 'no_extension'
            by_extension[ext]['count'] += 1
            by_extension[ext]['size'] += file['size_bytes']
            by_extension[ext]['loc'] += file['estimated_loc']

        # Count by directory
        by_directory = defaultdict(lambda: {'count': 0, 'size': 0, 'loc': 0})
        for file in files:
            dir_path = file['directory'] or 'root'
            by_directory[dir_path]['count'] += 1
            by_directory[dir_path]['size'] += file['size_bytes']
            by_directory[dir_path]['loc'] += file['estimated_loc']

        # Largest files by LOC (exclude binary/image files)
        code_files = [f for f in files if f['is_text'] and f['estimated_loc'] > 0]
        largest_files = sorted(code_files, key=lambda x: x['estimated_loc'], reverse=True)[:10]
        largest_files_info = [
            {'path': f['path'], 'size_kb': f['size_kb'], 'loc': f['estimated_loc']}
            for f in largest_files
        ]

        # Text vs binary files
        text_files = sum(1 for f in files if f['is_text'])
        binary_files = total_files - text_files

        return {
            'total_files': total_files,
            'total_size_bytes': total_size,
            'total_size_mb': round(total_size / (1024 * 1024), 2),
            'total_compressed_bytes': total_compressed,
            'total_compressed_mb': round(total_compressed / (1024 * 1024), 2),
            'overall_compression_ratio': round((1 - total_compressed / total_size) * 100, 2) if total_size > 0 else 0,
            'total_lines_of_code': total_loc,
            'text_files': text_files,
            'binary_files': binary_files,
            'by_extension': dict(by_extension),
            'by_directory': dict(by_directory),
            'largest_files': largest_files_info,
            'unique_directories': len(by_directory),
            'unique_extensions': len(by_extension)
        }

    def _get_language_distribution(self, files: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Get distribution of programming languages"""
        by_language = defaultdict(lambda: {
            'file_count': 0,
            'total_size': 0,
            'total_loc': 0,
            'files': []
        })

        for file in files:
            lang = file['language']
            by_language[lang]['file_count'] += 1
            by_language[lang]['total_size'] += file['size_bytes']
            by_language[lang]['total_loc'] += file['estimated_loc']
            by_language[lang]['files'].append(file['path'])

        # Sort by LOC
        sorted_languages = sorted(
            by_language.items(),
            key=lambda x: x[1]['total_loc'],
            reverse=True
        )

        return {
            'languages': dict(by_language),
            'languages_sorted': [
                {
                    'language': lang,
                    'file_count': data['file_count'],
                    'total_size_kb': round(data['total_size'] / 1024, 2),
                    'total_loc': data['total_loc'],
                    'percentage': round(data['total_loc'] / sum(f['estimated_loc'] for f in files) * 100, 2) if sum(f['estimated_loc'] for f in files) > 0 else 0
                }
                for lang, data in sorted_languages
            ]
        }

    def _perform_detailed_analysis(self, zip_ref: zipfile.ZipFile) -> Dict[str, Any]:
        """Perform detailed analysis by extracting and analyzing files"""
        # Create temporary directory
        self.temp_dir = tempfile.mkdtemp(prefix='zip_analysis_')

        try:
            # Extract all files
            zip_ref.extractall(self.temp_dir)

            # Analyze supported files
            supported_files = []
            analysis_results = []

            for root, dirs, files in os.walk(self.temp_dir):
                for file in files:
                    file_path = Path(root) / file
                    ext = file_path.suffix.lower()

                    # Check if file is supported for analysis
                    if self._detect_language(ext) in ['cobol', 'sql', 'as400']:
                        supported_files.append(file_path)

            # Analyze each supported file
            for file_path in supported_files:
                try:
                    analysis = self.language_router.analyze_file(file_path)
                    # Make path relative to temp directory
                    rel_path = file_path.relative_to(self.temp_dir)
                    analysis['relative_path'] = str(rel_path)
                    analysis_results.append(analysis)
                except Exception as e:
                    analysis_results.append({
                        'file': str(file_path),
                        'error': str(e)
                    })

            return {
                'total_analyzed_files': len(analysis_results),
                'supported_files': [str(f.relative_to(self.temp_dir)) for f in supported_files],
                'analysis_results': analysis_results
            }

        finally:
            # Cleanup temporary directory
            if self.temp_dir and os.path.exists(self.temp_dir):
                shutil.rmtree(self.temp_dir)
                self.temp_dir = None

    def extract_to(self, destination: str) -> str:
        """
        Extract ZIP contents to a destination folder

        Args:
            destination: Path to extract files to

        Returns:
            Path to extracted directory
        """
        dest_path = Path(destination)
        dest_path.mkdir(parents=True, exist_ok=True)

        with zipfile.ZipFile(self.zip_path, 'r') as zip_ref:
            zip_ref.extractall(dest_path)

        return str(dest_path)

    def get_file_content(self, file_path: str) -> str:
        """
        Get content of a specific file from the ZIP

        Args:
            file_path: Path to file within ZIP

        Returns:
            File content as string
        """
        with zipfile.ZipFile(self.zip_path, 'r') as zip_ref:
            try:
                content = zip_ref.read(file_path)
                return content.decode('utf-8', errors='ignore')
            except KeyError:
                raise FileNotFoundError(f"File not found in ZIP: {file_path}")


def explore_zip(zip_path: str, extract: bool = False) -> Dict[str, Any]:
    """
    Convenience function to explore a ZIP file

    Args:
        zip_path: Path to ZIP file
        extract: Whether to extract and perform detailed analysis

    Returns:
        Dictionary containing analysis results
    """
    explorer = ZipExplorer(zip_path)
    return explorer.explore(extract=extract)


def get_zip_summary(zip_path: str) -> Dict[str, Any]:
    """
    Get a quick summary of ZIP file contents

    Args:
        zip_path: Path to ZIP file

    Returns:
        Dictionary containing summary information
    """
    explorer = ZipExplorer(zip_path)
    full_analysis = explorer.explore(extract=False)

    return {
        'filename': full_analysis['zip_info']['filename'],
        'total_files': full_analysis['zip_info']['total_files'],
        'total_size_mb': full_analysis['zip_info']['size_mb'],
        'total_loc': full_analysis['statistics']['total_lines_of_code'],
        'languages': full_analysis['language_distribution']['languages_sorted'][:5],
        'top_extensions': sorted(
            full_analysis['statistics']['by_extension'].items(),
            key=lambda x: x[1]['count'],
            reverse=True
        )[:5]
    }

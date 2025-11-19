"""
Language-specific analysis API endpoints
"""
from fastapi import APIRouter, HTTPException
from pathlib import Path
import os
from typing import Dict, Any

from analyzers import LanguageRouter

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))


@router.get("/supported-languages")
async def get_supported_languages() -> Dict[str, Any]:
    """
    Get list of supported languages and their extensions
    """
    router_instance = LanguageRouter()
    return router_instance.get_supported_languages()


@router.post("/{upload_id}/analyze-file/{file_path:path}")
async def analyze_specific_file(upload_id: str, file_path: str) -> Dict[str, Any]:
    """
    Analyze a specific file using language-specific analyzer
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    # Find the analysis path
    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    full_file_path = analysis_path / file_path

    if not full_file_path.exists() or not full_file_path.is_file():
        raise HTTPException(status_code=404, detail="File not found")

    try:
        router_instance = LanguageRouter()
        result = router_instance.analyze_file(full_file_path)
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@router.post("/{upload_id}/analyze-by-language/{language}")
async def analyze_by_language(upload_id: str, language: str) -> Dict[str, Any]:
    """
    Analyze all files of a specific language

    Languages: cobol, java, python, sql, c, cpp, javascript, typescript
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    # Map language to extensions
    language_extensions = {
        'cobol': ['.cbl', '.cob', '.cobol', '.cpy'],
        'java': ['.java'],
        'python': ['.py', '.pyw'],
        'sql': ['.sql', '.ddl', '.dml'],
        'c': ['.c', '.h'],
        'cpp': ['.cpp', '.cxx', '.cc', '.hpp'],
        'javascript': ['.js', '.jsx'],
        'typescript': ['.ts', '.tsx'],
        'csharp': ['.cs'],
    }

    if language.lower() not in language_extensions:
        raise HTTPException(status_code=400, detail=f"Unsupported language: {language}")

    extensions = language_extensions[language.lower()]

    try:
        router_instance = LanguageRouter()

        # Find all files with matching extensions
        files = []
        for ext in extensions:
            files.extend(list(analysis_path.rglob(f'*{ext}')))

        if not files:
            return {
                'language': language,
                'files_found': 0,
                'message': f'No {language} files found'
            }

        # Analyze each file
        results = router_instance.batch_analyze(files)
        results['language_filter'] = language

        return results

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@router.post("/{upload_id}/deep-analysis")
async def deep_analysis(upload_id: str) -> Dict[str, Any]:
    """
    Perform deep language-specific analysis on entire codebase
    Groups results by language and provides detailed insights
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        router_instance = LanguageRouter()

        # Get all code files
        all_files = [f for f in analysis_path.rglob('*') if f.is_file() and f.suffix]

        # Analyze all files
        batch_results = router_instance.batch_analyze(all_files)

        # Group by language and aggregate
        by_language = {}

        for file_result in batch_results['files']:
            lang = file_result.get('language', 'unknown')

            if lang not in by_language:
                by_language[lang] = {
                    'files': [],
                    'total_files': 0,
                    'total_lines': 0,
                    'business_rules': [],
                    'database_operations': [],
                }

            by_language[lang]['files'].append(file_result)
            by_language[lang]['total_files'] += 1

            # Aggregate metrics
            if 'metrics' in file_result:
                by_language[lang]['total_lines'] += file_result['metrics'].get('total_lines', 0)

            # Aggregate business rules
            if 'business_rules' in file_result:
                by_language[lang]['business_rules'].extend(file_result['business_rules'])

            # Aggregate database operations
            if 'database' in file_result:
                if isinstance(file_result['database'], list):
                    by_language[lang]['database_operations'].extend(file_result['database'])

        # Calculate summaries
        for lang in by_language:
            by_language[lang]['business_rules_count'] = len(by_language[lang]['business_rules'])
            by_language[lang]['database_operations_count'] = len(by_language[lang]['database_operations'])

        return {
            'upload_id': upload_id,
            'total_files_analyzed': batch_results['summary']['total_files'],
            'languages': by_language,
            'summary': batch_results['summary']
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Deep analysis failed: {str(e)}")


@router.get("/{upload_id}/language-summary")
async def get_language_summary(upload_id: str) -> Dict[str, Any]:
    """
    Get a quick summary of languages used in the codebase
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    router_instance = LanguageRouter()
    language_counts = {}
    total_files = 0

    for file_path in analysis_path.rglob('*'):
        if file_path.is_file():
            ext = file_path.suffix.lower()
            lang = router_instance.LANGUAGE_MAP.get(ext, 'other')

            if lang not in language_counts:
                language_counts[lang] = {'count': 0, 'extensions': set()}

            language_counts[lang]['count'] += 1
            language_counts[lang]['extensions'].add(ext)
            total_files += 1

    # Convert sets to lists for JSON serialization
    for lang in language_counts:
        language_counts[lang]['extensions'] = list(language_counts[lang]['extensions'])

    return {
        'upload_id': upload_id,
        'total_files': total_files,
        'languages': language_counts,
        'unique_languages': len(language_counts)
    }

"""
ZIP File Analysis API endpoints
"""
from fastapi import APIRouter, UploadFile, File, HTTPException, BackgroundTasks
from pathlib import Path
import os
import uuid
from typing import Dict, Any, Optional
import shutil

from utils.zip_explorer import ZipExplorer, explore_zip, get_zip_summary

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))
UPLOAD_DIR.mkdir(parents=True, exist_ok=True)


@router.post("/upload")
async def upload_zip(file: UploadFile = File(...)) -> Dict[str, Any]:
    """
    Upload a ZIP file for analysis

    Returns upload_id for subsequent analysis requests
    """
    import time
    start_time = time.time()

    # Validate file is a ZIP
    if not file.filename.endswith('.zip'):
        raise HTTPException(status_code=400, detail="Only ZIP files are supported")

    # Generate unique upload ID
    upload_id = str(uuid.uuid4())
    upload_path = UPLOAD_DIR / upload_id
    upload_path.mkdir(parents=True, exist_ok=True)

    # Save uploaded file
    zip_file_path = upload_path / file.filename

    try:
        with open(zip_file_path, 'wb') as f:
            content = await file.read()
            f.write(content)

        # Extract the ZIP file immediately
        extract_dir = upload_path / "extracted"
        extract_dir.mkdir(exist_ok=True)

        import zipfile
        with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
            zip_ref.extractall(extract_dir)

        print(f"[INFO] Successfully extracted {file.filename} to {extract_dir}")

        # Scan for available file extensions
        extension_counts = {}
        total_files = 0
        for root, dirs, files in os.walk(extract_dir):
            for f in files:
                total_files += 1
                ext = os.path.splitext(f)[1].lower()
                if not ext:
                    ext = '.none'  # Files without extension
                extension_counts[ext] = extension_counts.get(ext, 0) + 1

        # Determine recommended extensions (common legacy code extensions)
        recommended_extensions = []
        legacy_extensions = {'.cbl', '.cob', '.cobol', '.cpy', '.sql', '.ddl', '.dml',
                           '.rpg', '.rpgle', '.sqlrpgle', '.rpg4', '.rpgiv',
                           '.dspf', '.prtf', '.lf', '.pf'}
        for ext in extension_counts:
            if ext in legacy_extensions:
                recommended_extensions.append(ext)

        response_data = {
            'upload_id': upload_id,
            'filename': file.filename,
            'size_bytes': len(content),
            'status': 'uploaded_and_extracted',
            'total_files': total_files,
            'available_extensions': extension_counts,
            'recommended_extensions': sorted(recommended_extensions)
        }

        elapsed_time = time.time() - start_time
        print(f"[DEBUG] ZIP Upload Response:")
        print(f"  - upload_id: {upload_id}")
        print(f"  - total_files: {total_files}")
        print(f"  - available_extensions count: {len(extension_counts)}")
        print(f"  - available_extensions: {list(extension_counts.keys())[:10]}")  # First 10
        print(f"  - recommended_extensions: {sorted(recommended_extensions)}")
        print(f"  - [TIMING] ZIP Upload took: {elapsed_time:.2f} seconds")

        return response_data
    except Exception as e:
        # Cleanup on error
        if upload_path.exists():
            shutil.rmtree(upload_path)
        raise HTTPException(status_code=500, detail=f"Upload failed: {str(e)}")


@router.get("/{upload_id}/explore")
async def explore_uploaded_zip(
    upload_id: str,
    detailed: bool = False
) -> Dict[str, Any]:
    """
    Explore ZIP file contents with comprehensive metadata

    Args:
        upload_id: The upload ID from /upload endpoint
        detailed: If True, extract and perform detailed code analysis
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    # Find the ZIP file
    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]

    try:
        explorer = ZipExplorer(str(zip_file_path))
        results = explorer.explore(extract=detailed)
        return results
    except Exception as e:
        import traceback
        error_details = traceback.format_exc()
        print(f"[ERROR] Exploration failed: {error_details}")
        raise HTTPException(status_code=500, detail=f"Exploration failed: {str(e)}")


@router.get("/{upload_id}/summary")
async def get_summary(upload_id: str) -> Dict[str, Any]:
    """
    Get a quick summary of ZIP file contents

    Returns high-level statistics without detailed analysis
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]

    try:
        summary = get_zip_summary(str(zip_file_path))
        return summary
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Summary generation failed: {str(e)}")


@router.get("/{upload_id}/files")
async def list_files(
    upload_id: str,
    language: Optional[str] = None,
    extension: Optional[str] = None
) -> Dict[str, Any]:
    """
    List all files in the ZIP with optional filtering

    Args:
        upload_id: The upload ID
        language: Filter by language (cobol, sql, as400)
        extension: Filter by file extension (e.g., .cbl, .sql)
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]

    try:
        explorer = ZipExplorer(str(zip_file_path))
        results = explorer.explore(extract=False)

        files = results['files']

        # Apply filters
        if language:
            files = [f for f in files if f['language'] == language.lower()]

        if extension:
            if not extension.startswith('.'):
                extension = '.' + extension
            files = [f for f in files if f['extension'] == extension.lower()]

        return {
            'total_files': len(files),
            'files': files
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"File listing failed: {str(e)}")


@router.get("/{upload_id}/file/{file_path:path}")
async def get_file(upload_id: str, file_path: str) -> Dict[str, Any]:
    """
    Get content of a specific file from the ZIP

    Args:
        upload_id: The upload ID
        file_path: Path to file within the ZIP
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]

    try:
        explorer = ZipExplorer(str(zip_file_path))
        content = explorer.get_file_content(file_path)

        return {
            'path': file_path,
            'content': content,
            'lines': len(content.split('\n')),
            'size': len(content)
        }
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to read file: {str(e)}")


@router.post("/{upload_id}/extract")
async def extract_zip(upload_id: str) -> Dict[str, Any]:
    """
    Extract ZIP file to server for further analysis

    Returns path to extracted directory
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]
    extract_dir = upload_path / "extracted"

    # Check if already extracted
    if extract_dir.exists():
        return {
            'status': 'already_extracted',
            'path': str(extract_dir),
            'upload_id': upload_id
        }

    try:
        explorer = ZipExplorer(str(zip_file_path))
        extracted_path = explorer.extract_to(str(extract_dir))

        return {
            'status': 'extracted',
            'path': extracted_path,
            'upload_id': upload_id
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Extraction failed: {str(e)}")


@router.delete("/{upload_id}")
async def delete_upload(upload_id: str) -> Dict[str, Any]:
    """
    Delete uploaded ZIP and extracted files

    Args:
        upload_id: The upload ID to delete
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    try:
        shutil.rmtree(upload_path)
        return {
            'status': 'deleted',
            'upload_id': upload_id
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Deletion failed: {str(e)}")


@router.get("/{upload_id}/statistics")
async def get_statistics(upload_id: str) -> Dict[str, Any]:
    """
    Get detailed statistics about the ZIP contents

    Returns comprehensive metrics and breakdowns
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    zip_files = list(upload_path.glob('*.zip'))
    if not zip_files:
        raise HTTPException(status_code=404, detail="ZIP file not found")

    zip_file_path = zip_files[0]

    try:
        explorer = ZipExplorer(str(zip_file_path))
        results = explorer.explore(extract=False)

        return {
            'zip_info': results['zip_info'],
            'statistics': results['statistics'],
            'language_distribution': results['language_distribution']
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Statistics generation failed: {str(e)}")

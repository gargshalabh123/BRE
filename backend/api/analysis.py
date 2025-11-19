"""
Analysis API endpoints
"""
from fastapi import APIRouter, HTTPException
from pathlib import Path
import os
from typing import Dict, Any

from analyzers.code_analyzer import CodeAnalyzer

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))


@router.post("/{upload_id}/full")
async def analyze_full(upload_id: str) -> Dict[str, Any]:
    """
    Run comprehensive analysis on uploaded codebase
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    # Find the analysis path (extracted folder or upload folder)
    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        analyzer = CodeAnalyzer(str(analysis_path))
        results = analyzer.analyze_all()

        # TEST MARKER - If you see this, the server reloaded successfully!
        results['TEST_SERVER_RELOADED'] = True
        results['TEST_MESSAGE'] = 'SERVER CODE UPDATED - V2.0'
        results['DEBUG_ANALYSIS_PATH'] = str(analysis_path)
        results['DEBUG_EXTRACTED_EXISTS'] = extract_dir.exists()

        return results
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")


@router.post("/{upload_id}/metrics")
async def analyze_metrics(upload_id: str) -> Dict[str, Any]:
    """
    Get code metrics (LOC, complexity, etc.)
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        analyzer = CodeAnalyzer(str(analysis_path))
        analyzer.scan_directory()
        metrics = analyzer._calculate_metrics()
        return metrics
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Metrics analysis failed: {str(e)}")


@router.post("/{upload_id}/dependencies")
async def analyze_dependencies(upload_id: str) -> Dict[str, Any]:
    """
    Extract dependency information
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        analyzer = CodeAnalyzer(str(analysis_path))
        analyzer.scan_directory()
        dependencies = analyzer._analyze_dependencies()

        # Create dependency graph data
        nodes = set()
        edges = []

        for file, deps in dependencies.items():
            nodes.add(file)
            for dep in deps:
                nodes.add(dep)
                edges.append({"source": file, "target": dep})

        return {
            "dependencies": dependencies,
            "graph": {
                "nodes": [{"id": node} for node in nodes],
                "edges": edges
            }
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Dependency analysis failed: {str(e)}")


@router.post("/{upload_id}/database")
async def analyze_database(upload_id: str) -> Dict[str, Any]:
    """
    Extract database operations
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        analyzer = CodeAnalyzer(str(analysis_path))
        analyzer.scan_directory()
        db_ops = analyzer._extract_database_operations()
        return db_ops
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Database analysis failed: {str(e)}")


@router.post("/{upload_id}/business-rules")
async def extract_business_rules(upload_id: str) -> Dict[str, Any]:
    """
    Extract business rules using pattern matching
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        analyzer = CodeAnalyzer(str(analysis_path))
        analyzer.scan_directory()
        rules = analyzer._extract_business_rules()
        return {"rules": rules, "total_count": len(rules)}
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Business rule extraction failed: {str(e)}")


@router.get("/{upload_id}/file/{file_path:path}")
async def get_file_content(upload_id: str, file_path: str) -> Dict[str, Any]:
    """
    Get content of a specific file
    """
    upload_path = UPLOAD_DIR / upload_id
    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    full_path = analysis_path / file_path

    if not full_path.exists() or not full_path.is_file():
        raise HTTPException(status_code=404, detail="File not found")

    try:
        with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()

        return {
            "path": file_path,
            "content": content,
            "lines": len(content.split('\n')),
            "size": len(content)
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to read file: {str(e)}")

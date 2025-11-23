"""
Analysis API endpoints
"""
from fastapi import APIRouter, HTTPException, Body
from pathlib import Path
import os
from typing import Dict, Any, List, Optional
from pydantic import BaseModel

from analyzers.code_analyzer import CodeAnalyzer
from analyzers.enhanced_dependency_analyzer import EnhancedDependencyAnalyzer
from database.db_manager import DatabaseManager
from database.persistence_service import PersistenceService

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))

# Database manager will be created per request to avoid connection pooling issues


class AnalysisRequest(BaseModel):
    selected_extensions: Optional[List[str]] = None


@router.post("/{upload_id}/full")
async def analyze_full(upload_id: str, request: AnalysisRequest = Body(default=None)) -> Dict[str, Any]:
    """
    Run comprehensive analysis on uploaded codebase
    Checks database cache first, falls back to analysis if not found
    """
    import time
    start_time = time.time()

    # Create single database manager for this request
    db_manager = DatabaseManager()
    persistence_service = PersistenceService(db_manager)

    try:
        # Try loading from database first
        try:
            cached_results = persistence_service.load_analysis_results(upload_id)
            if cached_results:
                print(f"[INFO] Loaded cached analysis for upload_id: {upload_id}")

                # Check if enhanced dependencies exist for this analysis
                analysis_run_id = cached_results.get('analysis_run_id')
                if analysis_run_id:
                    programs_check = db_manager.execute_query(
                        "SELECT COUNT(*) as count FROM programs WHERE analysis_run_id = ?",
                        (analysis_run_id,)
                    )
                    has_enhanced_deps = programs_check[0]['count'] > 0 if programs_check else False

                    if not has_enhanced_deps:
                        print(f"[INFO] Cached analysis missing enhanced dependencies, running now...")
                        # Run enhanced analyzer for cached results
                        upload_path = UPLOAD_DIR / upload_id
                        extract_dir = upload_path / "extracted"
                        analysis_path = extract_dir if extract_dir.exists() else upload_path

                        if analysis_path.exists():
                            try:
                                enhanced_analyzer = EnhancedDependencyAnalyzer(
                                    analysis_run_id=analysis_run_id,
                                    db_manager=db_manager
                                )
                                analyzed_files = [Path(analysis_path) / f['path'] for f in cached_results.get('files', [])]
                                dep_results = enhanced_analyzer.analyze_codebase(
                                    files=analyzed_files,
                                    upload_dir=Path(analysis_path)
                                )
                                print(f"[INFO] Enhanced dependencies complete:")
                                print(f"  - Programs: {dep_results['programs_found']}")
                                print(f"  - DB Objects: {dep_results['db_objects_found']}")
                                print(f"  - Copybooks: {dep_results['copybooks_found']}")
                                cached_results['enhanced_dependencies'] = dep_results
                            except Exception as e:
                                print(f"[WARN] Enhanced dependency analysis failed: {e}")
                                import traceback
                                traceback.print_exc()

                elapsed_time = time.time() - start_time
                print(f"[TIMING] Analysis (cached) took: {elapsed_time:.2f} seconds")
                return cached_results
        except Exception as e:
            print(f"[WARN] Failed to load from database, will analyze: {e}")

        # Not in database, perform fresh analysis
        upload_path = UPLOAD_DIR / upload_id

        if not upload_path.exists():
            raise HTTPException(status_code=404, detail="Upload not found")

        # Find the analysis path (extracted folder or upload folder)
        extract_dir = upload_path / "extracted"
        analysis_path = extract_dir if extract_dir.exists() else upload_path

        # Get selected extensions from request
        selected_extensions = None
        if request and request.selected_extensions:
            selected_extensions = request.selected_extensions
            print(f"[INFO] Analyzing with selected extensions: {selected_extensions}")

        # Perform analysis with optional file filtering
        analyzer = CodeAnalyzer(str(analysis_path), selected_extensions=selected_extensions)
        results = analyzer.analyze_all()

        # Save to database for future use
        try:
            # Get upload filename
            upload_filename = None
            zip_files = list(upload_path.glob("*.zip"))
            if zip_files:
                upload_filename = zip_files[0].name

            analysis_run_id = persistence_service.save_analysis_results(
                upload_id=upload_id,
                analysis_results=results,
                project_name="Default Project",
                upload_filename=upload_filename,
                user_id=None,  # TODO: Get from session/auth
                selected_extensions=selected_extensions
            )

            results['analysis_run_id'] = analysis_run_id
            results['SAVED_TO_DB'] = True

            # Run enhanced dependency analysis
            try:
                print("[INFO] Running enhanced dependency analysis...")
                enhanced_analyzer = EnhancedDependencyAnalyzer(
                    analysis_run_id=analysis_run_id,
                    db_manager=db_manager
                )

                # Get list of analyzed file paths
                analyzed_files = [Path(analysis_path) / f['path'] for f in results.get('files', [])]

                dep_results = enhanced_analyzer.analyze_codebase(
                    files=analyzed_files,
                    upload_dir=Path(analysis_path)
                )

                print(f"[INFO] Enhanced dependencies complete:")
                print(f"  - Programs: {dep_results['programs_found']}")
                print(f"  - DB Objects: {dep_results['db_objects_found']}")
                print(f"  - Copybooks: {dep_results['copybooks_found']}")

                results['enhanced_dependencies'] = dep_results
            except Exception as dep_error:
                print(f"[WARN] Enhanced dependency analysis failed: {dep_error}")
                import traceback
                traceback.print_exc()
                results['enhanced_dependencies_error'] = str(dep_error)

        except Exception as db_error:
            print(f"[ERROR] Failed to save to database: {db_error}")
            results['SAVE_TO_DB_ERROR'] = str(db_error)

        # TEST MARKER - If you see this, the server reloaded successfully!
        results['TEST_SERVER_RELOADED'] = True
        results['TEST_MESSAGE'] = 'SERVER CODE UPDATED - V3.0 WITH DATABASE'
        results['DEBUG_ANALYSIS_PATH'] = str(analysis_path)
        results['DEBUG_EXTRACTED_EXISTS'] = extract_dir.exists()

        elapsed_time = time.time() - start_time
        print(f"[TIMING] Analysis (fresh) took: {elapsed_time:.2f} seconds")

        return results
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Analysis failed: {str(e)}")
    finally:
        db_manager.close()


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


@router.get("/saved")
async def get_saved_analyses() -> Dict[str, Any]:
    """
    Get list of all saved analyses from database
    """
    db_manager = DatabaseManager()
    try:
        # Query all analysis runs with project info
        query = """
            SELECT
                ar.id,
                ar.upload_id,
                ar.upload_filename,
                ar.analysis_date,
                ar.status,
                ar.total_files,
                ar.total_loc,
                ar.total_sloc,
                ar.total_comments,
                ar.avg_complexity,
                p.name as project_name,
                p.description as project_description
            FROM analysis_runs ar
            LEFT JOIN projects p ON ar.project_id = p.id
            ORDER BY ar.analysis_date DESC
        """

        results = db_manager.execute_query(query)

        analyses = []
        for row in results:
            analyses.append({
                "id": row['id'],
                "upload_id": row['upload_id'],
                "upload_filename": row['upload_filename'],
                "analysis_date": row['analysis_date'],
                "status": row['status'],
                "total_files": row['total_files'],
                "total_loc": row['total_loc'],
                "total_sloc": row['total_sloc'],
                "total_comments": row['total_comments'],
                "avg_complexity": row['avg_complexity'],
                "project_name": row['project_name'],
                "project_description": row['project_description']
            })

        return {
            "analyses": analyses,
            "total_count": len(analyses)
        }
    except Exception as e:
        print(f"[ERROR] Failed to get saved analyses: {e}")
        raise HTTPException(status_code=500, detail=f"Failed to retrieve saved analyses: {str(e)}")
    finally:
        db_manager.close()


@router.delete("/saved/{upload_id}")
async def delete_saved_analysis(upload_id: str) -> Dict[str, Any]:
    """
    Delete a saved analysis by upload_id
    """
    db_manager = DatabaseManager()
    try:
        # Delete from database
        deleted = db_manager.delete_analysis_run_by_upload_id(upload_id)

        if not deleted:
            raise HTTPException(status_code=404, detail=f"Analysis with upload_id {upload_id} not found")

        return {
            "message": "Analysis deleted successfully",
            "upload_id": upload_id
        }
    except HTTPException:
        raise
    except Exception as e:
        print(f"[ERROR] Failed to delete analysis {upload_id}: {e}")
        raise HTTPException(status_code=500, detail=f"Failed to delete analysis: {str(e)}")
    finally:
        db_manager.close()


@router.delete("/saved")
async def delete_all_saved_analyses() -> Dict[str, Any]:
    """
    Delete all saved analyses from database
    """
    db_manager = DatabaseManager()
    try:
        # Delete all from database
        count = db_manager.delete_all_analysis_runs()

        return {
            "message": f"Successfully deleted {count} analysis run(s)",
            "deleted_count": count
        }
    except Exception as e:
        print(f"[ERROR] Failed to delete all analyses: {e}")
        raise HTTPException(status_code=500, detail=f"Failed to delete all analyses: {str(e)}")
    finally:
        db_manager.close()


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

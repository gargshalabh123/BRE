from fastapi import FastAPI, File, UploadFile, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import os
import shutil
from pathlib import Path
from dotenv import load_dotenv
import tempfile
import zipfile
from typing import List

from api.analysis import router as analysis_router
from api.ai import router as ai_router
from api.language_analysis import router as language_router
from api.zip_analysis import router as zip_router

load_dotenv()

app = FastAPI(
    title="Business Rules Extraction Framework",
    description="Extract business rules and analyze legacy codebases",
    version="1.0.0"
)

# CORS Configuration
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000", "http://localhost:5173"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Configuration
UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))
UPLOAD_DIR.mkdir(exist_ok=True)
MAX_UPLOAD_SIZE_MB = int(os.getenv("MAX_UPLOAD_SIZE_MB", "100"))

# Include routers
app.include_router(analysis_router, prefix="/api/analysis", tags=["Analysis"])
app.include_router(ai_router, prefix="/api/ai", tags=["AI"])
app.include_router(language_router, prefix="/api/languages", tags=["Language-Specific Analysis"])
app.include_router(zip_router, prefix="/api/zip", tags=["ZIP Analysis"])


@app.get("/")
async def root():
    return {
        "message": "Business Rules Extraction Framework API",
        "version": "1.0.0",
        "endpoints": {
            "upload": "/api/upload",
            "analysis": "/api/analysis",
            "ai": "/api/ai",
            "zip": "/api/zip"
        }
    }


@app.post("/api/upload")
async def upload_file(file: UploadFile = File(...)):
    """
    Upload a file or ZIP archive for analysis
    """
    try:
        # Check file size
        file.file.seek(0, 2)
        file_size = file.file.tell()
        file.file.seek(0)

        if file_size > MAX_UPLOAD_SIZE_MB * 1024 * 1024:
            raise HTTPException(
                status_code=413,
                detail=f"File size exceeds maximum allowed size of {MAX_UPLOAD_SIZE_MB}MB"
            )

        # Create unique directory for this upload
        upload_id = tempfile.mkdtemp(dir=UPLOAD_DIR)
        upload_path = Path(upload_id)

        # Save uploaded file
        file_path = upload_path / file.filename
        with open(file_path, "wb") as buffer:
            shutil.copyfileobj(file.file, buffer)

        # Extract if ZIP
        extracted_files = []
        if file.filename.endswith('.zip'):
            extract_dir = upload_path / "extracted"
            extract_dir.mkdir(exist_ok=True)

            try:
                with zipfile.ZipFile(file_path, 'r') as zip_ref:
                    zip_ref.extractall(extract_dir)
                print(f"[INFO] Successfully extracted {file.filename} to {extract_dir}")
            except Exception as e:
                print(f"[ERROR] Failed to extract ZIP: {e}")
                raise

            # Get all extracted files
            for root, dirs, files in os.walk(extract_dir):
                for f in files:
                    rel_path = os.path.relpath(os.path.join(root, f), extract_dir)
                    extracted_files.append(rel_path)

            analysis_path = str(extract_dir)
        else:
            extracted_files = [file.filename]
            analysis_path = str(upload_path)

        return JSONResponse({
            "upload_id": os.path.basename(upload_id),
            "filename": file.filename,
            "size": file_size,
            "is_archive": file.filename.endswith('.zip'),
            "files_count": len(extracted_files),
            "files": extracted_files[:50],  # Limit to first 50
            "analysis_path": analysis_path
        })

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.delete("/api/upload/{upload_id}")
async def delete_upload(upload_id: str):
    """
    Delete uploaded files and analysis results
    """
    upload_path = UPLOAD_DIR / upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    try:
        shutil.rmtree(upload_path)
        return {"message": "Upload deleted successfully"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/health")
async def health_check():
    # Check if code has specialized analyzer integration
    try:
        from analyzers.code_analyzer import CodeAnalyzer
        test_analyzer = CodeAnalyzer(".")
        has_language_router = hasattr(test_analyzer, 'language_router')
    except:
        has_language_router = False

    return {
        "status": "healthy",
        "code_version": "2.0_WITH_SPECIALIZED_ANALYZERS" if has_language_router else "1.0_OLD_CODE",
        "has_cobol_analyzer_integration": has_language_router
    }


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host=os.getenv("HOST", "0.0.0.0"),
        port=int(os.getenv("PORT", "8000")),
        reload=True
    )

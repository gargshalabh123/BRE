"""
AI-powered code analysis endpoints with context-aware, hallucination-resistant analysis
Supports OpenAI, Anthropic Claude, and Google Gemini
"""
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from pathlib import Path
import os
from typing import Dict, Any, Optional, List
import json
import hashlib
import sqlite3
from datetime import datetime

# Import our services
import sys
sys.path.append(str(Path(__file__).parent.parent))
from services.llm_service import get_llm_service
from services.context_builder import get_context_builder

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))
DB_PATH = Path("backend/data/bre_analysis.db")


# ==================== Request/Response Models ====================

class FileAnalysisRequest(BaseModel):
    upload_id: str
    file: str
    loc: Optional[int] = 0
    sloc: Optional[int] = 0
    complexity: Optional[int] = 0
    comments: Optional[int] = 0
    blank: Optional[int] = 0
    functions: Optional[int] = 0
    provider: Optional[str] = None


class RuleAnalysisRequest(BaseModel):
    upload_id: str
    file: str
    line: int
    type: str
    code: Optional[str] = ""
    description: Optional[str] = None
    provider: Optional[str] = None


class DependencyAnalysisRequest(BaseModel):
    upload_id: str
    file: str
    type: str
    target: str
    line: Optional[int] = None
    lines: Optional[List[int]] = []
    signature: Optional[str] = None
    parameters: Optional[List[str]] = []
    description: Optional[str] = None
    provider: Optional[str] = None


class QueryAnalysisRequest(BaseModel):
    upload_id: str
    file: str
    line: int
    type: str
    query: str
    category: Optional[str] = "SQL"
    provider: Optional[str] = None


class InsightsRequest(BaseModel):
    upload_id: str
    scope: str  # 'codebase', 'file', 'rule', 'dependency'
    item_id: Optional[str] = None
    provider: Optional[str] = None


class ChatRequest(BaseModel):
    upload_id: str
    message: str
    session_id: Optional[str] = None
    provider: Optional[str] = None


# ==================== Helper Functions ====================

def get_db_connection():
    """Get database connection"""
    conn = sqlite3.connect(str(DB_PATH), check_same_thread=False)
    conn.row_factory = sqlite3.Row
    return conn


def get_analysis_from_cache(upload_id: str, item_type: str, item_id: str) -> Optional[Dict]:
    """Check if analysis exists in cache"""
    conn = get_db_connection()
    cursor = conn.cursor()

    cursor.execute("""
        SELECT analysis_text, provider, model, created_at
        FROM ai_analysis_cache
        WHERE item_type = ? AND item_id = ?
        ORDER BY created_at DESC
        LIMIT 1
    """, (item_type, item_id))

    result = cursor.fetchone()
    conn.close()

    if result:
        return {
            "analysis": result[0],
            "provider": result[1],
            "model": result[2],
            "cached": True,
            "cached_at": result[3]
        }
    return None


def save_analysis_to_cache(upload_id: str, item_type: str, item_id: str,
                           item_hash: str, analysis: str, provider: str, model: str):
    """Save analysis to cache"""
    conn = get_db_connection()
    cursor = conn.cursor()

    # Get analysis_run_id
    cursor.execute("SELECT id FROM analysis_runs WHERE upload_id = ?", (upload_id,))
    run = cursor.fetchone()

    if run:
        cursor.execute("""
            INSERT INTO ai_analysis_cache
            (analysis_run_id, item_type, item_id, item_hash, analysis_text, provider, model)
            VALUES (?, ?, ?, ?, ?, ?, ?)
        """, (run[0], item_type, item_id, item_hash, analysis, provider, model))
        conn.commit()

    conn.close()


async def load_full_analysis(upload_id: str) -> Dict[str, Any]:
    """Load full analysis data from database"""
    from database.persistence_service import PersistenceService
    from database.db_manager import DatabaseManager

    # Load from database directly
    db_manager = DatabaseManager()
    persistence = PersistenceService(db_manager)
    return persistence.load_analysis_results(upload_id)


# ==================== API Endpoints ====================

@router.post("/analyze-file")
async def analyze_file(request: FileAnalysisRequest) -> Dict[str, Any]:
    """
    Analyze a specific file's metrics and quality
    Uses context-aware analysis to prevent hallucination
    """
    try:
        # Check cache first
        item_id = f"{request.upload_id}:{request.file}"
        cached = get_analysis_from_cache(request.upload_id, "file", item_id)
        if cached:
            return cached

        # Build context
        context_builder = get_context_builder(request.upload_id, "file")
        file_data = {"file": request.file, "type": "Unknown"}
        metrics = {
            "loc": request.loc,
            "sloc": request.sloc,
            "complexity": request.complexity,
            "comments": request.comments,
            "blank": request.blank,
            "functions": request.functions
        }

        context = context_builder.build_context(file_data, metrics)

        # Analyze with LLM
        llm_service = get_llm_service(request.provider)
        result = await llm_service.analyze(context, max_tokens=2000)

        # Cache the result
        item_hash = hashlib.md5(f"{request.file}:{request.loc}:{request.complexity}".encode()).hexdigest()
        save_analysis_to_cache(
            request.upload_id, "file", item_id, item_hash,
            result["analysis"], result["provider"], result["model"]
        )

        return result

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"File analysis failed: {str(e)}")


@router.post("/analyze-rule")
async def analyze_rule(request: RuleAnalysisRequest) -> Dict[str, Any]:
    """
    Analyze a business rule with surrounding context
    Finds duplicate rules and provides improvement suggestions
    """
    import logging
    import sys
    logger = logging.getLogger(__name__)

    try:
        print(f"[DEBUG] analyze_rule called with: upload_id={request.upload_id}, file={request.file}, line={request.line}, type={request.type}", file=sys.stderr)
        print(f"[DEBUG] code length: {len(request.code) if request.code else 0}, description: {request.description}", file=sys.stderr)

        # Check cache
        item_id = f"{request.upload_id}:{request.file}:{request.line}"
        cached = get_analysis_from_cache(request.upload_id, "rule", item_id)
        if cached:
            return cached

        print(f"[DEBUG] Loading full analysis...", file=sys.stderr)
        # Load full analysis to find similar rules
        full_analysis = await load_full_analysis(request.upload_id)
        all_rules = full_analysis.get("business_rules", [])
        print(f"[DEBUG] Found {len(all_rules)} rules", file=sys.stderr)

        # Build context
        context_builder = get_context_builder(request.upload_id, "rule")
        rule_data = {
            "file": request.file,
            "line": request.line,
            "type": request.type,
            "code": request.code,
            "description": request.description
        }

        print(f"[DEBUG] Building context...", file=sys.stderr)
        context = context_builder.build_context(rule_data, all_rules)
        print(f"[DEBUG] Context built, length: {len(context)}", file=sys.stderr)

        # Analyze with LLM
        print(f"[DEBUG] Getting LLM service...", file=sys.stderr)
        llm_service = get_llm_service(request.provider)
        print(f"[DEBUG] Calling LLM analyze...", file=sys.stderr)
        result = await llm_service.analyze(context, max_tokens=2500)

        # Cache the result
        item_hash = hashlib.md5(request.code.encode()).hexdigest()
        save_analysis_to_cache(
            request.upload_id, "rule", item_id, item_hash,
            result["analysis"], result["provider"], result["model"]
        )

        return result

    except Exception as e:
        import traceback
        error_detail = f"Rule analysis failed: {str(e)}\n{traceback.format_exc()}"
        print(f"[ERROR] {error_detail}", file=sys.stderr)
        logger.error(error_detail)
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/analyze-dependency")
async def analyze_dependency(request: DependencyAnalysisRequest) -> Dict[str, Any]:
    """
    Analyze a dependency with impact assessment
    Shows which files depend on this and refactoring suggestions
    """
    try:
        # Check cache
        item_id = f"{request.upload_id}:{request.file}:{request.target}"
        cached = get_analysis_from_cache(request.upload_id, "dependency", item_id)
        if cached:
            return cached

        # Load full analysis to find related dependencies
        full_analysis = await load_full_analysis(request.upload_id)
        all_dependencies = full_analysis.get("detailed_dependencies", {})

        # Build context
        context_builder = get_context_builder(request.upload_id, "dependency")
        dep_data = {
            "file": request.file,
            "type": request.type,
            "target": request.target,
            "line": request.line,
            "lines": request.lines or ([request.line] if request.line else []),
            "signature": request.signature,
            "parameters": request.parameters,
            "description": request.description
        }

        context = context_builder.build_context(dep_data, all_dependencies)

        # Analyze with LLM
        llm_service = get_llm_service(request.provider)
        result = await llm_service.analyze(context, max_tokens=2000)

        # Cache the result
        item_hash = hashlib.md5(f"{request.target}:{request.type}".encode()).hexdigest()
        save_analysis_to_cache(
            request.upload_id, "dependency", item_id, item_hash,
            result["analysis"], result["provider"], result["model"]
        )

        return result

    except Exception as e:
        import traceback
        error_detail = f"Dependency analysis failed: {str(e)}\n{traceback.format_exc()}"
        print(f"[ERROR] {error_detail}")
        raise HTTPException(status_code=500, detail=error_detail)


@router.post("/analyze-query")
async def analyze_query(request: QueryAnalysisRequest) -> Dict[str, Any]:
    """
    Analyze a database query for optimization and security
    Provides index suggestions and identifies SQL injection risks
    """
    try:
        # Check cache
        item_id = f"{request.upload_id}:{request.file}:{request.line}"
        cached = get_analysis_from_cache(request.upload_id, "query", item_id)
        if cached:
            return cached

        # Load full analysis to find related queries
        full_analysis = await load_full_analysis(request.upload_id)
        all_queries = full_analysis.get("database_operations", {}).get("queries", [])

        # Build context
        context_builder = get_context_builder(request.upload_id, "query")
        query_data = {
            "file": request.file,
            "line": request.line,
            "type": request.type,
            "query": request.query,
            "category": request.category
        }

        context = context_builder.build_context(query_data, all_queries)

        # Analyze with LLM
        llm_service = get_llm_service(request.provider)
        result = await llm_service.analyze(context, max_tokens=2000)

        # Cache the result
        item_hash = hashlib.md5(request.query.encode()).hexdigest()
        save_analysis_to_cache(
            request.upload_id, "query", item_id, item_hash,
            result["analysis"], result["provider"], result["model"]
        )

        return result

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Query analysis failed: {str(e)}")


@router.post("/insights")
async def generate_insights(request: InsightsRequest) -> Dict[str, Any]:
    """
    Generate AI insights with flexible scope (codebase/file/rule/dependency)
    """
    try:
        # Load full analysis
        full_analysis = await load_full_analysis(request.upload_id)

        # Build prompt based on scope
        if request.scope == "codebase":
            prompt = f"""Analyze this entire codebase and provide high-level insights:

**Codebase Statistics:**
- Total Files: {full_analysis.get('summary', {}).get('total_files', 0)}
- Total Lines of Code: {full_analysis.get('metrics', {}).get('total_loc', 0)}
- Business Rules Detected: {len(full_analysis.get('business_rules', []))}
- Database Operations: {full_analysis.get('database_operations', {}).get('total_count', 0)}
- File Types: {json.dumps(full_analysis.get('summary', {}).get('file_types', {}), indent=2)}

Provide:
1. Overall architecture assessment
2. Main business domains identified
3. Technical debt indicators
4. Modernization priorities (top 5)
5. Risk areas requiring attention
"""
        elif request.scope == "file" and request.item_id:
            # Find specific file metrics
            files = full_analysis.get("metrics", {}).get("by_file", [])
            file_data = next((f for f in files if f.get("file") == request.item_id), None)
            if not file_data:
                raise HTTPException(status_code=404, detail="File not found")

            # Use file analysis
            file_request = FileAnalysisRequest(
                upload_id=request.upload_id,
                file=request.item_id,
                **file_data
            )
            return await analyze_file(file_request)

        else:
            prompt = "Invalid scope or missing item_id"

        # Analyze with LLM
        llm_service = get_llm_service(request.provider)
        result = await llm_service.analyze(prompt, max_tokens=3000)

        return result

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Insights generation failed: {str(e)}")


@router.post("/chat")
async def chat(request: ChatRequest) -> Dict[str, Any]:
    """
    Conversational interface - chat with your codebase
    Maintains conversation history for context
    """
    try:
        # For now, simple implementation without RAG
        # In production, you'd use vector search here

        # Load analysis summary
        full_analysis = await load_full_analysis(request.upload_id)

        # Build context from codebase
        context = f"""You are analyzing a codebase with:
- {full_analysis.get('summary', {}).get('total_files', 0)} files
- {full_analysis.get('metrics', {}).get('total_loc', 0)} lines of code
- {len(full_analysis.get('business_rules', []))} business rules
- {full_analysis.get('database_operations', {}).get('total_count', 0)} database operations

User Question: {request.message}

Provide a helpful, accurate answer based on the codebase data. If you don't have enough information, say so."""

        # Analyze with LLM
        llm_service = get_llm_service(request.provider)
        result = await llm_service.analyze(context, max_tokens=1500, temperature=0.8)

        # TODO: Save to conversation history in database

        return {
            "message": result["analysis"],
            "provider": result["provider"],
            "model": result["model"],
            "context_used": []  # TODO: Track which files were referenced
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Chat failed: {str(e)}")


@router.get("/providers")
async def get_available_providers() -> Dict[str, Any]:
    """
    Get list of available AI providers and their configuration
    """
    providers = {}

    # Check OpenAI
    if os.getenv("OPENAI_API_KEY"):
        providers["openai"] = {
            "available": True,
            "model": os.getenv("OPENAI_MODEL", "gpt-4"),
            "name": "OpenAI GPT"
        }

    # Check Anthropic
    if os.getenv("ANTHROPIC_API_KEY"):
        providers["anthropic"] = {
            "available": True,
            "model": os.getenv("ANTHROPIC_MODEL", "claude-3-5-sonnet-20241022"),
            "name": "Anthropic Claude"
        }

    # Check Gemini
    if os.getenv("GEMINI_API_KEY"):
        providers["gemini"] = {
            "available": True,
            "model": os.getenv("GEMINI_MODEL", "gemini-1.5-pro"),
            "name": "Google Gemini"
        }

    default_provider = os.getenv("AI_PROVIDER", "gemini")

    return {
        "providers": providers,
        "default": default_provider,
        "total_available": len(providers)
    }


@router.get("/history/{upload_id}")
async def get_ai_history(upload_id: str) -> Dict[str, Any]:
    """
    Get AI analysis history for an upload
    """
    try:
        conn = get_db_connection()
        cursor = conn.cursor()

        cursor.execute("""
            SELECT item_type, item_id, provider, model, created_at, access_count
            FROM ai_analysis_cache
            WHERE analysis_run_id = (
                SELECT id FROM analysis_runs WHERE upload_id = ?
            )
            ORDER BY created_at DESC
            LIMIT 50
        """, (upload_id,))

        results = cursor.fetchall()
        conn.close()

        history = []
        for row in results:
            history.append({
                "item_type": row[0],
                "item_id": row[1],
                "provider": row[2],
                "model": row[3],
                "created_at": row[4],
                "access_count": row[5]
            })

        return {
            "upload_id": upload_id,
            "history": history,
            "total_analyses": len(history)
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to fetch history: {str(e)}")

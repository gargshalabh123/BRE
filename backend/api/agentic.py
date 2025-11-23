"""
Agentic AI Code Analysis API Endpoints

Multi-agent system with ReAct pattern for deep code analysis
"""
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Dict, Any, Optional, List
from pathlib import Path
import os
import sys

# Import agentic services
sys.path.append(str(Path(__file__).parent.parent))
from services.agentic.orchestrator import OrchestratorAgent
from services.llm_service import get_llm_service
from database.persistence_service import PersistenceService
from database.db_manager import DatabaseManager

router = APIRouter()


# ==================== Request/Response Models ====================

class AgenticAnalysisRequest(BaseModel):
    upload_id: str
    query: str
    agent_type: Optional[str] = None  # "security", "architecture", "general", or None (auto-route)
    provider: Optional[str] = None


class AgenticChatRequest(BaseModel):
    upload_id: str
    message: str
    conversation_history: Optional[List[Dict]] = None
    provider: Optional[str] = None


class MultiAgentRequest(BaseModel):
    upload_id: str
    agent_types: List[str]
    provider: Optional[str] = None


# ==================== Helper Functions ====================

def load_analysis_results(upload_id: str) -> Dict[str, Any]:
    """Load analysis results from database (synchronous)"""
    db_manager = DatabaseManager()
    persistence = PersistenceService(db_manager)
    return persistence.load_analysis_results(upload_id)


def get_orchestrator(upload_id: str, provider: Optional[str]) -> OrchestratorAgent:
    """Get orchestrator instance for upload"""
    # Load analysis results (synchronous)
    analysis_results = load_analysis_results(upload_id)

    if not analysis_results:
        raise HTTPException(status_code=404, detail="Analysis results not found for upload")

    # Get LLM service
    llm_service = get_llm_service(provider)

    # Create orchestrator
    orchestrator = OrchestratorAgent(llm_service, upload_id, analysis_results)

    return orchestrator


# ==================== API Endpoints ====================

@router.post("/agentic/analyze")
async def agentic_analysis(request: AgenticAnalysisRequest) -> Dict[str, Any]:
    """
    Run agentic analysis with ReAct pattern

    The agent will use tools to explore the codebase and answer the query systematically.

    Args:
        upload_id: ID of uploaded codebase
        query: Natural language query/task
        agent_type: Optional specific agent ("security", "architecture", "general")
        provider: AI provider ("openai", "anthropic", "gemini")

    Returns:
        Analysis result with answer and reasoning trace
    """
    try:
        print(f"[API] Agentic analysis request: {request.query[:100]}")

        # Get orchestrator
        orchestrator = get_orchestrator(request.upload_id, request.provider)

        # Run analysis
        result = await orchestrator.analyze(
            query=request.query,
            agent_type=request.agent_type
        )

        return {
            "upload_id": request.upload_id,
            "query": request.query,
            "agent_type": request.agent_type,
            "answer": result.get("answer", ""),
            "reasoning_trace": result.get("reasoning_trace", []),
            "iterations_used": result.get("iterations_used", 0),
            "status": result.get("status", "unknown"),
            "provider": orchestrator.llm.provider_name,
            "model": orchestrator.llm.model_name
        }

    except Exception as e:
        import traceback
        error_detail = f"Agentic analysis failed: {str(e)}\n{traceback.format_exc()}"
        print(f"[ERROR] {error_detail}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/agentic/chat")
async def agentic_chat(request: AgenticChatRequest) -> Dict[str, Any]:
    """
    Interactive chat with agentic analysis

    Maintains conversation context across multiple turns.

    Args:
        upload_id: ID of uploaded codebase
        message: User message
        conversation_history: Previous messages
        provider: AI provider

    Returns:
        Response with context awareness
    """
    try:
        print(f"[API] Agentic chat request: {request.message[:100]}")

        # Get orchestrator
        orchestrator = get_orchestrator(request.upload_id, request.provider)

        # Run interactive chat
        result = await orchestrator.interactive_chat(
            message=request.message,
            conversation_history=request.conversation_history
        )

        return {
            "upload_id": request.upload_id,
            "message": request.message,
            "response": result.get("answer", ""),
            "reasoning_trace": result.get("reasoning_trace", []),
            "conversation_aware": result.get("conversation_aware", False),
            "provider": orchestrator.llm.provider_name,
            "model": orchestrator.llm.model_name
        }

    except Exception as e:
        import traceback
        error_detail = f"Agentic chat failed: {str(e)}\n{traceback.format_exc()}"
        print(f"[ERROR] {error_detail}")
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/agentic/multi-agent")
async def multi_agent_analysis(request: MultiAgentRequest) -> Dict[str, Any]:
    """
    Run multiple specialized agents in parallel

    Each agent analyzes the codebase from their specialty perspective.

    Args:
        upload_id: ID of uploaded codebase
        agent_types: List of agents to run (["security", "architecture", etc.])
        provider: AI provider

    Returns:
        Combined results from all agents with synthesis
    """
    try:
        print(f"[API] Multi-agent analysis request: {request.agent_types}")

        # Get orchestrator
        orchestrator = get_orchestrator(request.upload_id, request.provider)

        # Run multi-agent analysis
        result = await orchestrator.multi_agent_analysis(agent_types=request.agent_types)

        return {
            "upload_id": request.upload_id,
            "agents_used": result.get("agents_used", []),
            "results": result.get("results", {}),
            "synthesis": result.get("synthesis", ""),
            "provider": orchestrator.llm.provider_name,
            "model": orchestrator.llm.model_name
        }

    except Exception as e:
        import traceback
        error_detail = f"Multi-agent analysis failed: {str(e)}\n{traceback.format_exc()}"
        print(f"[ERROR] {error_detail}")
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/agentic/agents")
async def get_available_agents() -> Dict[str, Any]:
    """
    Get list of available specialized agents

    Returns:
        Dict mapping agent types to descriptions
    """
    # This is static, no need to create orchestrator
    return {
        "agents": {
            "security": {
                "name": "Security Agent",
                "description": "Finds security vulnerabilities (SQL injection, XSS, secrets, etc.)",
                "specialty": "Security vulnerability analysis"
            },
            "architecture": {
                "name": "Architecture Agent",
                "description": "Analyzes system architecture, patterns, and structure",
                "specialty": "System architecture analysis"
            },
            "general": {
                "name": "General Agent",
                "description": "General-purpose code analysis",
                "specialty": "General code analysis"
            }
        },
        "default": "general"
    }


@router.get("/agentic/tools")
async def get_available_tools() -> Dict[str, Any]:
    """
    Get list of tools available to agents

    Returns:
        Tool categories and descriptions
    """
    return {
        "categories": {
            "File Operations": [
                {"name": "read_file", "description": "Read source code from files"},
                {"name": "get_file_list", "description": "List files matching criteria"},
                {"name": "get_file_context", "description": "Get code context around a line"},
                {"name": "get_file_info", "description": "Get file metadata"}
            ],
            "Search & Discovery": [
                {"name": "search_code", "description": "Search for code patterns"},
                {"name": "find_similar_code", "description": "Find similar code snippets"},
                {"name": "find_definition", "description": "Find function/class definitions"}
            ],
            "Analysis & Metrics": [
                {"name": "get_file_metrics", "description": "Get code metrics"},
                {"name": "query_database_operations", "description": "Query DB operations"},
                {"name": "get_business_rules", "description": "Get business rules"},
                {"name": "get_complexity_hotspots", "description": "Find complex code"}
            ],
            "Graph & Relationships": [
                {"name": "get_dependencies", "description": "Get file dependencies"},
                {"name": "trace_function_calls", "description": "Trace call graphs"},
                {"name": "build_dependency_graph", "description": "Build dependency graph"}
            ]
        },
        "total_tools": 17
    }


@router.get("/agentic/stats/{upload_id}")
async def get_agentic_stats(upload_id: str, provider: Optional[str] = None) -> Dict[str, Any]:
    """
    Get statistics about agentic system for an upload

    Args:
        upload_id: ID of uploaded codebase
        provider: AI provider (optional)

    Returns:
        Statistics about accessible files, tools, etc.
    """
    try:
        orchestrator = get_orchestrator(upload_id, provider)
        stats = orchestrator.get_statistics()

        return {
            "upload_id": upload_id,
            "statistics": stats,
            "agentic_ready": True
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Failed to get stats: {str(e)}")

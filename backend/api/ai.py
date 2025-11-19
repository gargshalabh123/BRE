"""
AI-powered code analysis endpoints
"""
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from pathlib import Path
import os
from typing import Dict, Any, Optional
import json

try:
    from openai import OpenAI
except ImportError:
    OpenAI = None

try:
    from anthropic import Anthropic
except ImportError:
    Anthropic = None

router = APIRouter()

UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "../uploads"))


class CodeExplanationRequest(BaseModel):
    upload_id: str
    file_path: str
    code_snippet: Optional[str] = None
    line_start: Optional[int] = None
    line_end: Optional[int] = None
    provider: str = "anthropic"  # "openai" or "anthropic"


class AIService:
    """AI service for code explanation and analysis"""

    def __init__(self):
        self.openai_client = None
        self.anthropic_client = None

        # Initialize OpenAI
        if OpenAI and os.getenv("OPENAI_API_KEY"):
            try:
                self.openai_client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))
            except Exception as e:
                print(f"Failed to initialize OpenAI: {e}")

        # Initialize Anthropic
        if Anthropic and os.getenv("ANTHROPIC_API_KEY"):
            try:
                self.anthropic_client = Anthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))
            except Exception as e:
                print(f"Failed to initialize Anthropic: {e}")

    def explain_code(self, code: str, file_path: str, provider: str = "anthropic") -> str:
        """Generate natural language explanation of code"""

        prompt = f"""Analyze the following code from file: {file_path}

Please provide:
1. A high-level summary of what this code does
2. Key business logic and rules implemented
3. Important dependencies and integrations
4. Potential issues or technical debt
5. Suggested improvements

Code:
```
{code}
```

Provide a clear, structured analysis suitable for business stakeholders and developers."""

        if provider == "anthropic" and self.anthropic_client:
            return self._explain_with_anthropic(prompt)
        elif provider == "openai" and self.openai_client:
            return self._explain_with_openai(prompt)
        else:
            raise ValueError(f"AI provider '{provider}' not available or not configured")

    def _explain_with_anthropic(self, prompt: str) -> str:
        """Use Anthropic Claude for explanation"""
        try:
            response = self.anthropic_client.messages.create(
                model=os.getenv("ANTHROPIC_MODEL", "claude-3-5-sonnet-20241022"),
                max_tokens=2000,
                messages=[
                    {"role": "user", "content": prompt}
                ]
            )
            return response.content[0].text
        except Exception as e:
            raise Exception(f"Anthropic API error: {str(e)}")

    def _explain_with_openai(self, prompt: str) -> str:
        """Use OpenAI GPT for explanation"""
        try:
            response = self.openai_client.chat.completions.create(
                model=os.getenv("OPENAI_MODEL", "gpt-4-turbo-preview"),
                messages=[
                    {"role": "system", "content": "You are an expert code analyst specializing in legacy systems and business rules extraction."},
                    {"role": "user", "content": prompt}
                ],
                max_tokens=2000
            )
            return response.choices[0].message.content
        except Exception as e:
            raise Exception(f"OpenAI API error: {str(e)}")

    def summarize_codebase(self, analysis_results: Dict[str, Any], provider: str = "anthropic") -> str:
        """Generate high-level summary of entire codebase"""

        summary_data = {
            "total_files": analysis_results.get("summary", {}).get("total_files", 0),
            "file_types": analysis_results.get("summary", {}).get("file_types", {}),
            "total_loc": analysis_results.get("metrics", {}).get("total_loc", 0),
            "database_operations": analysis_results.get("database_operations", {}).get("total_count", 0),
            "business_rules_count": len(analysis_results.get("business_rules", []))
        }

        prompt = f"""Analyze this legacy codebase summary and provide insights:

Codebase Statistics:
- Total Files: {summary_data['total_files']}
- File Types: {json.dumps(summary_data['file_types'], indent=2)}
- Total Lines of Code: {summary_data['total_loc']}
- Database Operations: {summary_data['database_operations']}
- Business Rules Detected: {summary_data['business_rules_count']}

Please provide:
1. Overall assessment of the codebase
2. Technology stack and architecture patterns
3. Main business domains and functionality
4. Modernization recommendations
5. Risk areas and technical debt indicators

Provide a concise executive summary."""

        if provider == "anthropic" and self.anthropic_client:
            return self._explain_with_anthropic(prompt)
        elif provider == "openai" and self.openai_client:
            return self._explain_with_openai(prompt)
        else:
            raise ValueError(f"AI provider '{provider}' not available")


# Initialize AI service
ai_service = AIService()


@router.post("/explain-code")
async def explain_code(request: CodeExplanationRequest) -> Dict[str, Any]:
    """
    Get AI-powered explanation of code
    """
    upload_path = UPLOAD_DIR / request.upload_id

    if not upload_path.exists():
        raise HTTPException(status_code=404, detail="Upload not found")

    extract_dir = upload_path / "extracted"
    analysis_path = extract_dir if extract_dir.exists() else upload_path

    try:
        # Get code content
        if request.code_snippet:
            code = request.code_snippet
        else:
            file_full_path = analysis_path / request.file_path
            if not file_full_path.exists():
                raise HTTPException(status_code=404, detail="File not found")

            with open(file_full_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()

            if request.line_start and request.line_end:
                code = ''.join(lines[request.line_start - 1:request.line_end])
            else:
                code = ''.join(lines)

        # Get explanation
        explanation = ai_service.explain_code(
            code=code,
            file_path=request.file_path,
            provider=request.provider
        )

        return {
            "file_path": request.file_path,
            "explanation": explanation,
            "provider": request.provider,
            "code_length": len(code)
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"AI analysis failed: {str(e)}")


@router.post("/summarize/{upload_id}")
async def summarize_codebase(upload_id: str, provider: str = "anthropic") -> Dict[str, Any]:
    """
    Get AI-powered summary of entire codebase
    """
    from api.analysis import analyze_full

    try:
        # Get full analysis
        analysis_results = await analyze_full(upload_id)

        # Generate AI summary
        summary = ai_service.summarize_codebase(
            analysis_results=analysis_results,
            provider=provider
        )

        return {
            "upload_id": upload_id,
            "summary": summary,
            "provider": provider,
            "statistics": analysis_results.get("summary", {})
        }

    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Summarization failed: {str(e)}")


@router.get("/providers")
async def get_available_providers() -> Dict[str, Any]:
    """
    Get list of available AI providers
    """
    return {
        "providers": {
            "openai": {
                "available": ai_service.openai_client is not None,
                "model": os.getenv("OPENAI_MODEL", "gpt-4-turbo-preview")
            },
            "anthropic": {
                "available": ai_service.anthropic_client is not None,
                "model": os.getenv("ANTHROPIC_MODEL", "claude-3-5-sonnet-20241022")
            }
        }
    }

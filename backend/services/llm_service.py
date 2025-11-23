"""
Multi-provider LLM Service with context-aware analysis
Supports OpenAI, Anthropic, and Google Gemini
"""
import os
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List
import asyncio

try:
    from openai import OpenAI
except ImportError:
    OpenAI = None

try:
    from anthropic import Anthropic
except ImportError:
    Anthropic = None

try:
    import google.generativeai as genai
except ImportError:
    genai = None


class LLMProvider(ABC):
    """Abstract base class for LLM providers"""

    @abstractmethod
    async def generate(self, prompt: str, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        """Generate response from LLM"""
        pass

    @property
    @abstractmethod
    def provider_name(self) -> str:
        """Return provider name"""
        pass

    @property
    @abstractmethod
    def model_name(self) -> str:
        """Return model name"""
        pass


class OpenAIProvider(LLMProvider):
    """OpenAI provider implementation"""

    def __init__(self, api_key: str, model: str = "gpt-4"):
        if not OpenAI:
            raise ImportError("OpenAI package not installed")
        self.client = OpenAI(api_key=api_key)
        self.model = model

    async def generate(self, prompt: str, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        try:
            response = await asyncio.to_thread(
                self.client.chat.completions.create,
                model=self.model,
                messages=[
                    {
                        "role": "system",
                        "content": "You are an expert code analyst. Your role is to explain and document code clearly and concisely. Focus on WHAT the code does and HOW it works - do NOT provide suggestions, recommendations, or improvements unless specifically asked."
                    },
                    {"role": "user", "content": prompt}
                ],
                max_tokens=max_tokens,
                temperature=temperature
            )
            return response.choices[0].message.content
        except Exception as e:
            raise Exception(f"OpenAI API error: {str(e)}")

    @property
    def provider_name(self) -> str:
        return "openai"

    @property
    def model_name(self) -> str:
        return self.model


class AnthropicProvider(LLMProvider):
    """Anthropic Claude provider implementation"""

    def __init__(self, api_key: str, model: str = "claude-3-5-sonnet-20241022"):
        if not Anthropic:
            raise ImportError("Anthropic package not installed")
        self.client = Anthropic(api_key=api_key)
        self.model = model

    async def generate(self, prompt: str, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        try:
            response = await asyncio.to_thread(
                self.client.messages.create,
                model=self.model,
                max_tokens=max_tokens,
                temperature=temperature,
                system="You are an expert code analyst. Your role is to explain and document code clearly and concisely. Focus on WHAT the code does and HOW it works - do NOT provide suggestions, recommendations, or improvements unless specifically asked.",
                messages=[{"role": "user", "content": prompt}]
            )
            return response.content[0].text
        except Exception as e:
            raise Exception(f"Anthropic API error: {str(e)}")

    @property
    def provider_name(self) -> str:
        return "anthropic"

    @property
    def model_name(self) -> str:
        return self.model


class GeminiProvider(LLMProvider):
    """Google Gemini provider implementation"""

    def __init__(self, api_key: str, model: str = "gemini-1.5-pro"):
        if not genai:
            raise ImportError("Google Generative AI package not installed")
        genai.configure(api_key=api_key)
        self.model_name_str = model

        # Configure safety settings for code analysis
        self.safety_settings = {
            "HARM_CATEGORY_HARASSMENT": "BLOCK_NONE",
            "HARM_CATEGORY_HATE_SPEECH": "BLOCK_NONE",
            "HARM_CATEGORY_SEXUALLY_EXPLICIT": "BLOCK_NONE",
            "HARM_CATEGORY_DANGEROUS_CONTENT": "BLOCK_MEDIUM_AND_ABOVE"
        }

        self.generation_config = {
            "temperature": 0.7,
            "top_p": 0.95,
            "top_k": 40,
        }

        self.model = genai.GenerativeModel(
            model_name=model,
            safety_settings=self.safety_settings,
            generation_config=self.generation_config
        )

    async def generate(self, prompt: str, max_tokens: int = 2000, temperature: float = 0.7) -> str:
        try:
            # Update temperature for this request
            self.generation_config["temperature"] = temperature
            self.generation_config["max_output_tokens"] = max_tokens

            # Add system instruction as part of the prompt
            full_prompt = f"""You are an expert code analyst. Your role is to explain and document code clearly and concisely. Focus on WHAT the code does and HOW it works - do NOT provide suggestions, recommendations, or improvements unless specifically asked.

{prompt}"""

            response = await asyncio.to_thread(
                self.model.generate_content,
                full_prompt
            )

            return response.text
        except Exception as e:
            raise Exception(f"Gemini API error: {str(e)}")

    @property
    def provider_name(self) -> str:
        return "gemini"

    @property
    def model_name(self) -> str:
        return self.model_name_str


class LLMService:
    """Unified LLM service supporting multiple providers"""

    def __init__(self, provider: Optional[str] = None):
        """
        Initialize LLM service with specified provider or auto-detect from env

        Args:
            provider: "openai", "anthropic", or "gemini". If None, auto-detect from env
        """
        self.provider = self._initialize_provider(provider)

    def _initialize_provider(self, provider_name: Optional[str] = None) -> LLMProvider:
        """Initialize the appropriate provider based on configuration"""

        # Determine provider from env or parameter
        if provider_name is None:
            provider_name = os.getenv("AI_PROVIDER", "gemini").lower()

        if provider_name == "openai":
            api_key = os.getenv("OPENAI_API_KEY")
            if not api_key:
                raise ValueError("OPENAI_API_KEY not set in environment")
            model = os.getenv("OPENAI_MODEL", "gpt-4")
            return OpenAIProvider(api_key, model)

        elif provider_name == "anthropic":
            api_key = os.getenv("ANTHROPIC_API_KEY")
            if not api_key:
                raise ValueError("ANTHROPIC_API_KEY not set in environment")
            model = os.getenv("ANTHROPIC_MODEL", "claude-3-5-sonnet-20241022")
            return AnthropicProvider(api_key, model)

        elif provider_name == "gemini":
            api_key = os.getenv("GEMINI_API_KEY")
            if not api_key:
                raise ValueError("GEMINI_API_KEY not set in environment")
            model = os.getenv("GEMINI_MODEL", "gemini-1.5-pro")
            return GeminiProvider(api_key, model)

        else:
            raise ValueError(f"Unsupported AI provider: {provider_name}")

    async def analyze(
        self,
        prompt: str,
        max_tokens: int = 2000,
        temperature: float = 0.7
    ) -> Dict[str, Any]:
        """
        Analyze with LLM and return response with metadata

        Returns:
            Dict with 'analysis', 'provider', 'model', 'cached', 'prompt' keys
        """
        analysis = await self.provider.generate(prompt, max_tokens, temperature)

        return {
            "analysis": analysis,
            "provider": self.provider.provider_name,
            "model": self.provider.model_name,
            "cached": False,
            "prompt": prompt  # Include prompt for debugging
        }

    @property
    def provider_name(self) -> str:
        """Get current provider name"""
        return self.provider.provider_name

    @property
    def model_name(self) -> str:
        """Get current model name"""
        return self.provider.model_name


def get_llm_service(provider: Optional[str] = None) -> LLMService:
    """Factory function to get LLM service instance"""
    return LLMService(provider)

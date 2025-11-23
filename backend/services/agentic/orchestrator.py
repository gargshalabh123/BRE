"""
Orchestrator Agent - Coordinates specialized agents and delegates tasks

Responsible for:
- Understanding user intent
- Routing to appropriate specialized agents
- Synthesizing results from multiple agents
- Managing complex multi-agent workflows
"""
from typing import Dict, List, Any, Optional
import asyncio

from .tools.tool_registry import ToolRegistry
from .agents.base_agent import BaseAgent
from .agents.security_agent import SecurityAgent
from .agents.architecture_agent import ArchitectureAgent


class OrchestratorAgent:
    """
    Master orchestrator that coordinates specialized agents

    Can handle both simple queries (direct tool use) and complex queries
    (delegate to specialized agents)
    """

    def __init__(self, llm_service, upload_id: str, analysis_results: Dict[str, Any]):
        """
        Initialize orchestrator

        Args:
            llm_service: LLM service for generation
            upload_id: Upload ID for codebase
            analysis_results: Complete analysis results
        """
        self.llm = llm_service
        self.upload_id = upload_id
        self.analysis_results = analysis_results

        # Initialize tool registry
        self.tool_registry = ToolRegistry(upload_id, analysis_results)

        # Initialize specialized agents
        self.agents = {
            "security": SecurityAgent(llm_service, self.tool_registry),
            "architecture": ArchitectureAgent(llm_service, self.tool_registry),
            "general": BaseAgent(llm_service, self.tool_registry, agent_name="GeneralAgent")
        }

        print(f"[Orchestrator] Initialized for upload {upload_id}")
        print(f"[Orchestrator] Available agents: {list(self.agents.keys())}")

    async def analyze(
        self,
        query: str,
        agent_type: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Main entry point for analysis

        Args:
            query: User query/task
            agent_type: Optional specific agent to use ("security", "architecture", "general")
                       If None, orchestrator will route automatically

        Returns:
            Analysis results with answer and reasoning trace
        """
        print(f"\n[Orchestrator] Received query: {query[:100]}...")

        # If specific agent requested, use it directly
        if agent_type and agent_type in self.agents:
            agent = self.agents[agent_type]
            print(f"[Orchestrator] Using requested agent: {agent_type}")
            return await agent.run(query)

        # Otherwise, classify intent and route
        intent = await self._classify_intent(query)
        print(f"[Orchestrator] Detected intent: {intent}")

        # Route to appropriate agent
        if intent in self.agents:
            agent = self.agents[intent]
            print(f"[Orchestrator] Routing to {intent} agent")
            return await agent.run(query)
        else:
            # Use general agent
            agent = self.agents["general"]
            print(f"[Orchestrator] Using general agent")
            return await agent.run(query)

    async def _classify_intent(self, query: str) -> str:
        """
        Classify user intent to route to appropriate agent

        Returns:
            Agent type: "security", "architecture", or "general"
        """
        query_lower = query.lower()

        # Security keywords
        security_keywords = [
            'security', 'vulnerability', 'vulnerabilities', 'sql injection', 'xss',
            'secret', 'password', 'authentication', 'authorization', 'exploit',
            'attack', 'malicious', 'secure', 'insecure', 'breach'
        ]

        # Architecture keywords
        architecture_keywords = [
            'architecture', 'structure', 'pattern', 'design', 'component',
            'module', 'layer', 'tier', 'dependency', 'dependencies', 'coupling',
            'organize', 'organization'
        ]

        # Check for security intent
        if any(keyword in query_lower for keyword in security_keywords):
            return "security"

        # Check for architecture intent
        if any(keyword in query_lower for keyword in architecture_keywords):
            return "architecture"

        # Default to general
        return "general"

    async def multi_agent_analysis(
        self,
        agent_types: List[str],
        task_per_agent: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """
        Run multiple agents in parallel on different tasks

        Args:
            agent_types: List of agent types to run
            task_per_agent: Optional dict mapping agent type to specific task
                           If None, each runs a default analysis

        Returns:
            Combined results from all agents
        """
        print(f"[Orchestrator] Running multi-agent analysis: {agent_types}")

        # Prepare tasks
        tasks = []
        for agent_type in agent_types:
            if agent_type not in self.agents:
                print(f"[Orchestrator] Warning: Unknown agent type {agent_type}, skipping")
                continue

            agent = self.agents[agent_type]

            # Get task for this agent
            if task_per_agent and agent_type in task_per_agent:
                task = task_per_agent[agent_type]
            else:
                # Use default analysis
                task = self._get_default_task(agent_type)

            tasks.append(agent.run(task))

        # Run all agents in parallel
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # Combine results
        combined = {
            "agents_used": agent_types,
            "results": {}
        }

        for agent_type, result in zip(agent_types, results):
            if isinstance(result, Exception):
                combined["results"][agent_type] = {
                    "error": str(result),
                    "status": "failed"
                }
            else:
                combined["results"][agent_type] = result

        # Synthesize if multiple agents
        if len(agent_types) > 1:
            combined["synthesis"] = await self._synthesize_results(combined["results"])

        return combined

    def _get_default_task(self, agent_type: str) -> str:
        """Get default analysis task for agent type"""
        default_tasks = {
            "security": "Perform comprehensive security analysis. Find all vulnerabilities.",
            "architecture": "Analyze and explain the system architecture.",
            "general": "Provide a comprehensive analysis of this codebase."
        }
        return default_tasks.get(agent_type, "Analyze this codebase.")

    async def _synthesize_results(self, results: Dict[str, Dict]) -> str:
        """
        Synthesize results from multiple agents into coherent summary

        Args:
            results: Dict mapping agent type to their results

        Returns:
            Synthesized summary
        """
        # Build synthesis prompt
        synthesis_prompt = """Synthesize the following analysis results into a coherent summary:

"""
        for agent_type, result in results.items():
            if result.get('status') != 'failed':
                answer = result.get('answer', '')
                synthesis_prompt += f"\n**{agent_type.capitalize()} Agent:**\n{answer}\n"

        synthesis_prompt += """

Provide a concise executive summary that:
1. Highlights the most important findings
2. Identifies critical issues that need attention
3. Summarizes key insights about the codebase
4. Provides actionable recommendations

Keep it clear and concise (max 300 words).
"""

        try:
            result = await self.llm.analyze(
                synthesis_prompt,
                max_tokens=1000,
                temperature=0.5
            )
            return result.get('analysis', '')
        except Exception as e:
            return f"Synthesis failed: {str(e)}"

    async def interactive_chat(
        self,
        message: str,
        conversation_history: Optional[List[Dict]] = None
    ) -> Dict[str, Any]:
        """
        Interactive chat interface for conversational analysis

        Args:
            message: User message
            conversation_history: Previous conversation messages

        Returns:
            Response with context
        """
        # Build context from history
        context = ""
        if conversation_history:
            context = "\n\n**Previous Conversation:**\n"
            for msg in conversation_history[-5:]:  # Last 5 messages
                role = msg.get('role', 'user')
                content = msg.get('content', '')
                context += f"{role}: {content}\n"

        # Run analysis with context
        result = await self.analyze(message)

        # Add context awareness
        result['conversation_aware'] = bool(conversation_history)

        return result

    def get_available_agents(self) -> Dict[str, str]:
        """Get list of available specialized agents"""
        return {
            "security": "Security vulnerability analysis",
            "architecture": "System architecture analysis",
            "general": "General code analysis"
        }

    def get_statistics(self) -> Dict[str, Any]:
        """Get orchestrator statistics"""
        return {
            "upload_id": self.upload_id,
            "available_agents": list(self.agents.keys()),
            "accessible_files": len(self.tool_registry.list_accessible_files()),
            "available_tools": len(self.tool_registry.tools),
            "codebase_summary": self.analysis_results.get('summary', {})
        }

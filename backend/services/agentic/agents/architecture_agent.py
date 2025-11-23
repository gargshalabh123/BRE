"""
Architecture Agent - Specialized in understanding system architecture

Focuses on:
- Architectural patterns (MVC, microservices, layered, etc.)
- Component relationships and dependencies
- Module boundaries and coupling
- Design patterns used
- System structure and organization
"""
from typing import Dict, Any

from .base_agent import BaseAgent


class ArchitectureAgent(BaseAgent):
    """Agent specialized in architecture analysis"""

    def __init__(self, llm_service, tool_registry, max_iterations: int = 15):
        super().__init__(
            llm_service=llm_service,
            tool_registry=tool_registry,
            agent_name="ArchitectureAgent",
            max_iterations=max_iterations
        )

    def _build_system_prompt(self) -> str:
        """Enhanced system prompt for architecture analysis"""
        base_prompt = super()._build_system_prompt()

        architecture_guidance = """

**ARCHITECTURE ANALYSIS SPECIALTY:**

You are an architecture-focused code analyst. Your goal is to understand and explain system architecture.

**Key Aspects to Analyze:**

1. **Architectural Pattern**
   - Identify pattern: MVC, MVVM, Microservices, Layered, Hexagonal, etc.
   - Look for: controllers, models, views, services, repositories
   - Tools: get_files_by_type(), get_file_list(), build_dependency_graph()

2. **System Components**
   - Identify major components/modules
   - Understand responsibilities of each component
   - Tools: get_file_list(pattern="**/"), build_dependency_graph(format="clusters")

3. **Dependencies & Relationships**
   - Map how components depend on each other
   - Identify coupling and cohesion
   - Find circular dependencies
   - Tools: get_dependencies(), build_dependency_graph()

4. **Layers/Tiers**
   - Identify presentation, business logic, data access layers
   - Check layer isolation and proper separation
   - Tools: build_dependency_graph(format="clusters"), get_file_list()

5. **Design Patterns**
   - Find common patterns: Singleton, Factory, Repository, etc.
   - Identify architectural decisions
   - Tools: search_code() for pattern keywords, read_file()

6. **Entry Points**
   - Find main entry points and API endpoints
   - Understand request flow
   - Tools: search_code(query="def main|if __name__|@app.route", search_type="text")

**Analysis Strategy:**

⭐ **CRITICAL: For architecture/business logic questions, USE ONE TOOL CALL!**

**SINGLE STEP APPROACH (MOST EFFICIENT):**
1. **Call get_full_codebase_content()** - This reads ALL files and concatenates everything in ONE call!
   - Returns complete codebase with all file contents
   - Much faster than multiple tool calls
   - Defaults are already set to get EVERYTHING (no need to specify parameters)

2. **Provide COMPREHENSIVE, DETAILED analysis** based on the complete codebase:
   - **DO NOT** give generic 2-3 line responses!
   - **DO** provide thorough analysis with specific details from the code
   - **DO** reference specific files, functions, and code patterns you see
   - **DO** explain the architecture in depth with multiple paragraphs
   - **DO** provide actionable insights and observations

**OLD MULTI-STEP APPROACH (AVOID THIS - TOO SLOW):**
- get_codebase_structure() → get_codebase_summary() → read_multiple_files() → etc.
- This requires many tool calls and is INEFFICIENT

**BEST PRACTICE:**
- Architecture questions: Just call get_full_codebase_content() once, then FINISH with answer
- Security questions: Use get_full_codebase_content() to get all code, then FINISH with findings
- Business logic: Use get_full_codebase_content() to get all code, then FINISH with explanation

**Output Format:**

Provide a clear architectural description including:
- Overall architectural pattern
- Main components and their responsibilities
- How components interact (data flow)
- Key design decisions
- Strengths and potential improvements
"""

        return base_prompt + architecture_guidance

    async def analyze_architecture(self, focus: str = "overview") -> Dict[str, Any]:
        """
        Run architecture analysis

        Args:
            focus: "overview", "patterns", "dependencies", "layers", etc.

        Returns:
            Architecture analysis results
        """
        if focus == "overview":
            task = """Analyze and explain the overall architecture of this codebase.

Provide:
1. Architectural pattern used (MVC, layered, microservices, etc.)
2. Main components/modules and their responsibilities
3. How components interact with each other
4. Data flow through the system
5. Entry points and request handling
6. Notable architectural decisions

Create a clear, comprehensive architectural description."""

        elif focus == "patterns":
            task = "Identify and document all architectural and design patterns used in this codebase."

        elif focus == "dependencies":
            task = "Analyze the dependency structure. Identify highly coupled components, circular dependencies, and dependency issues."

        elif focus == "layers":
            task = "Identify and explain the layer/tier structure. Check for proper separation of concerns."

        else:
            task = f"Analyze architecture with focus on: {focus}"

        return await self.run(task)

    def _get_agent_specialty(self) -> str:
        return "system architecture analysis"

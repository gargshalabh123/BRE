"""
Base Agent with ReAct (Reasoning + Acting) pattern

Enables multi-step reasoning with tool use for code analysis
"""
from typing import Dict, List, Any, Optional
import re
import json

from ..tools.tool_registry import ToolRegistry


class BaseAgent:
    """
    Base agent implementing ReAct pattern

    ReAct Loop:
    1. Thought: Agent reasons about what to do next
    2. Action: Agent decides to use a tool or finish
    3. Observation: Result from tool execution
    4. Repeat until task is complete
    """

    def __init__(
        self,
        llm_service,
        tool_registry: ToolRegistry,
        agent_name: str = "BaseAgent",
        max_iterations: int = 10
    ):
        """
        Initialize base agent

        Args:
            llm_service: LLM service for generation
            tool_registry: Registry of available tools
            agent_name: Name of this agent
            max_iterations: Maximum reasoning iterations
        """
        self.llm = llm_service
        self.tools = tool_registry
        self.agent_name = agent_name
        self.max_iterations = max_iterations

        print(f"[{agent_name}] Initialized with {len(tool_registry.tools)} tools")

    async def run(self, task: str, context: Optional[str] = None) -> Dict[str, Any]:
        """
        Run agent on a task using ReAct loop

        Args:
            task: Task description
            context: Optional additional context

        Returns:
            Dict with answer and reasoning trace
        """
        print(f"\n[{self.agent_name}] Starting task: {task[:100]}...")

        # Build system prompt with tool descriptions
        system_prompt = self._build_system_prompt()

        # Initialize conversation history
        conversation = []

        # Add context if provided
        if context:
            conversation.append({
                "type": "context",
                "content": context
            })

        # ReAct loop
        for iteration in range(1, self.max_iterations + 1):
            print(f"[{self.agent_name}] Iteration {iteration}/{self.max_iterations}")

            # Build prompt for this iteration
            prompt = self._build_iteration_prompt(
                system_prompt,
                task,
                conversation,
                iteration
            )

            # LLM generates thought + action
            try:
                # Use higher max_tokens for comprehensive responses (especially for FINISH action)
                result = await self.llm.analyze(
                    prompt,
                    max_tokens=4000,  # Increased from 1500 to allow detailed analysis
                    temperature=0.3  # Lower temperature for more focused responses
                )
                response = result.get('analysis', '')
            except Exception as e:
                error_msg = f"LLM generation failed: {str(e)}"
                print(f"[{self.agent_name}] ERROR: {error_msg}")
                conversation.append({
                    "iteration": iteration,
                    "error": error_msg
                })
                break

            # Parse response into thought and action
            thought, action = self._parse_response(response)

            conversation.append({
                "iteration": iteration,
                "thought": thought,
                "action": action
            })

            print(f"[{self.agent_name}] Thought: {thought[:100]}...")
            print(f"[{self.agent_name}] Action: {action.get('type', 'unknown')}")

            # Check if agent wants to finish
            if action['type'] == 'FINISH':
                print(f"[{self.agent_name}] Task completed")
                return {
                    "answer": action.get('value', ''),
                    "reasoning_trace": conversation,
                    "iterations_used": iteration,
                    "status": "completed"
                }

            # Execute tool
            observation = await self._execute_action(action)

            conversation.append({
                "observation": observation
            })

            print(f"[{self.agent_name}] Observation: {str(observation)[:100]}...")

        # Max iterations reached
        print(f"[{self.agent_name}] Max iterations reached")
        return {
            "answer": "Maximum iterations reached. Task may be incomplete.",
            "reasoning_trace": conversation,
            "iterations_used": self.max_iterations,
            "status": "max_iterations"
        }

    def _build_system_prompt(self) -> str:
        """Build system prompt with tool descriptions"""
        tool_descriptions = self.tools.list_tools()
        tool_categories = self.tools.get_tool_categories()

        tools_by_category = []
        for category, tool_names in tool_categories.items():
            tools_text = "\n".join([
                f"  - {name}: {tool_descriptions.get(name, '')}"
                for name in tool_names
            ])
            tools_by_category.append(f"**{category}:**\n{tools_text}")

        tools_formatted = "\n\n".join(tools_by_category)

        return f"""You are an expert code analysis agent named "{self.agent_name}".
Your role is to analyze code systematically using available tools.

You have access to {len(self.tools.tools)} tools organized in {len(tool_categories)} categories:

{tools_formatted}

**Important Guidelines:**
1. ⭐ For architecture/business logic questions: Use get_full_codebase_content() - ONE call to get ALL files!
2. This is THE MOST EFFICIENT approach - get everything at once instead of multiple tool calls
3. For specific code searches: Use search_code() or read_file()
4. Break down complex tasks into steps ONLY when necessary
5. Analyze information before drawing conclusions
6. Be thorough but EFFICIENT - minimize tool calls
7. When you have enough information, provide a complete answer

**Response Format:**
You must respond in this exact format:

Thought: <your reasoning about what to do next>
Action: <tool_name(param1="value1", param2="value2")> OR FINISH: <your final answer>

**Examples:**

Example 1 - Architecture/Business Logic (BEST PRACTICE - ONE CALL):
Thought: To explain the architecture, I need to see the complete codebase
Action: get_full_codebase_content()

Example 2 - Read a specific file:
Thought: Let me read this file to check for vulnerabilities
Action: read_file(file_path="api/users.py", start_line=40, end_line=60)

Example 3 - Search for patterns:
Thought: I need to find all SQL queries in the codebase
Action: search_code(query="SELECT.*FROM", search_type="text")

Example 4 - Finish with COMPREHENSIVE answer:
Thought: I have the complete codebase and have analyzed it thoroughly
Action: FINISH: Based on my comprehensive analysis of the entire codebase (112 files, 40,989 lines), here is the detailed architecture:

## Overall Architecture
[Detailed paragraph explaining the pattern]

## Key Components
1. Component A: [Detailed explanation with file references]
2. Component B: [Detailed explanation]

## Data Flow
[Detailed explanation of how data flows]

## Key Observations
- [Specific finding 1]
- [Specific finding 2]

IMPORTANT: Provide DETAILED, MULTI-PARAGRAPH analysis, NOT just 2-3 generic lines!

Always provide both Thought and Action in your response.
"""

    def _build_iteration_prompt(
        self,
        system_prompt: str,
        task: str,
        conversation: List[Dict],
        iteration: int
    ) -> str:
        """Build prompt for current iteration"""

        # Format conversation history
        history = self._format_conversation(conversation)

        return f"""{system_prompt}

**Task:** {task}

**Previous Steps:**
{history if history else "(This is the first iteration)"}

**Iteration {iteration}:**
What should you do next? Remember to respond with:
Thought: <reasoning>
Action: <tool_call or FINISH>
"""

    def _format_conversation(self, conversation: List[Dict]) -> str:
        """Format conversation history for prompt"""
        if not conversation:
            return ""

        formatted = []

        for entry in conversation:
            if entry.get('type') == 'context':
                formatted.append(f"Context: {entry['content']}")

            elif 'iteration' in entry and 'thought' in entry:
                iter_num = entry['iteration']
                thought = entry['thought']
                action = entry.get('action', {})

                formatted.append(f"\nIteration {iter_num}:")
                formatted.append(f"Thought: {thought}")

                if action.get('type') == 'tool':
                    formatted.append(f"Action: {action.get('name')}({action.get('params_str', '')})")
                elif action.get('type') == 'FINISH':
                    formatted.append(f"Action: FINISH")

            elif 'observation' in entry:
                obs = entry['observation']
                # Truncate long observations
                obs_str = str(obs)
                if len(obs_str) > 1000:
                    obs_str = obs_str[:1000] + "\n... (truncated)"
                formatted.append(f"Observation: {obs_str}")

            elif 'error' in entry:
                formatted.append(f"Error: {entry['error']}")

        return '\n'.join(formatted)

    def _parse_response(self, response: str) -> tuple:
        """
        Parse LLM response into thought and action

        Returns:
            (thought: str, action: dict)
        """
        lines = response.strip().split('\n')

        thought = ""
        action = {"type": "unknown"}
        in_action_section = False
        action_lines = []

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Extract thought
            if line_stripped.startswith("Thought:"):
                thought = line_stripped.replace("Thought:", "").strip()
                in_action_section = False

            # Extract action
            elif line_stripped.startswith("Action:"):
                in_action_section = True
                action_text = line_stripped.replace("Action:", "").strip()

                # Check if FINISH
                if action_text.startswith("FINISH"):
                    # Get everything after FINISH: (could be on same line or next lines)
                    finish_first_line = action_text.replace("FINISH:", "").replace("FINISH", "").strip()
                    action_lines = [finish_first_line] if finish_first_line else []

                    # Collect all remaining lines as part of FINISH content
                    for remaining_line in lines[i+1:]:
                        action_lines.append(remaining_line)

                    finish_text = '\n'.join(action_lines).strip()
                    action = {
                        "type": "FINISH",
                        "value": finish_text
                    }
                    break  # Stop parsing after FINISH
                else:
                    # Parse tool call
                    action = self._parse_tool_call(action_text)
                    break  # Stop after finding action

        return thought, action

    def _parse_tool_call(self, action_text: str) -> Dict:
        """
        Parse tool call from text

        Examples:
        - read_file(file_path="api/users.py")
        - search_code(query="SELECT.*", search_type="text")
        """
        # Extract tool name and parameters
        match = re.match(r'(\w+)\s*\((.*?)\)\s*$', action_text, re.DOTALL)

        if not match:
            return {
                "type": "error",
                "message": f"Could not parse action: {action_text}"
            }

        tool_name = match.group(1)
        params_str = match.group(2)

        # Parse parameters
        params = self._parse_parameters(params_str)

        return {
            "type": "tool",
            "name": tool_name,
            "params": params,
            "params_str": params_str
        }

    def _parse_parameters(self, params_str: str) -> Dict:
        """Parse parameters from string"""
        params = {}

        if not params_str.strip():
            return params

        # Simple parameter parsing (handles strings, numbers, booleans)
        # Example: file_path="api/users.py", start_line=40, end_line=60

        # Split by comma (not inside quotes)
        parts = re.split(r',(?=(?:[^"]*"[^"]*")*[^"]*$)', params_str)

        for part in parts:
            part = part.strip()
            if '=' not in part:
                continue

            key, value = part.split('=', 1)
            key = key.strip()
            value = value.strip()

            # Remove quotes from strings
            if value.startswith('"') and value.endswith('"'):
                value = value[1:-1]
            elif value.startswith("'") and value.endswith("'"):
                value = value[1:-1]
            # Convert numbers
            elif value.isdigit():
                value = int(value)
            # Convert booleans
            elif value.lower() in ('true', 'false'):
                value = value.lower() == 'true'

            params[key] = value

        return params

    async def _execute_action(self, action: Dict) -> Any:
        """Execute an action (tool call)"""
        if action['type'] != 'tool':
            return f"Cannot execute action type: {action['type']}"

        tool_name = action.get('name')
        params = action.get('params', {})

        print(f"[{self.agent_name}] Executing: {tool_name}({params})")

        try:
            result = await self.tools.execute_tool(tool_name, **params)
            return result
        except Exception as e:
            return f"Tool execution error: {str(e)}"

    def _get_agent_specialty(self) -> str:
        """Override in subclasses to define specialty"""
        return "general code analysis"

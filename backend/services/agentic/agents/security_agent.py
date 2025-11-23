"""
Security Agent - Specialized in finding security vulnerabilities

Focuses on:
- SQL injection
- XSS (Cross-Site Scripting)
- Hardcoded secrets
- Authentication/authorization issues
- Input validation problems
"""
from typing import Dict, Any, Optional

from .base_agent import BaseAgent


class SecurityAgent(BaseAgent):
    """Agent specialized in security vulnerability analysis"""

    def __init__(self, llm_service, tool_registry, max_iterations: int = 15):
        super().__init__(
            llm_service=llm_service,
            tool_registry=tool_registry,
            agent_name="SecurityAgent",
            max_iterations=max_iterations
        )

    def _build_system_prompt(self) -> str:
        """Enhanced system prompt for security analysis"""
        base_prompt = super()._build_system_prompt()

        security_guidance = """

**SECURITY ANALYSIS SPECIALTY:**

You are a security-focused code analyst. Your primary goal is to identify security vulnerabilities.

**Priority Vulnerabilities to Check:**

1. **SQL Injection**
   - Look for string concatenation in SQL queries
   - Check for unsanitized user input in database operations
   - Search pattern: SELECT/INSERT/UPDATE/DELETE with string concatenation
   - Tool: query_database_operations() then read_file() to verify

2. **Cross-Site Scripting (XSS)**
   - Unescaped user input in HTML/templates
   - Unsafe use of innerHTML, document.write
   - Search pattern: innerHTML|document.write|dangerouslySetInnerHTML
   - Tool: search_code() with appropriate patterns

3. **Hardcoded Secrets**
   - API keys, passwords, tokens in source code
   - Search pattern: password|api_key|secret|token|credentials
   - Look for: = "..." patterns with sensitive keywords
   - Tool: search_code(query="(password|api_key|secret).*=.*['\\\"]", search_type="text")

4. **Authentication/Authorization Issues**
   - Missing authentication checks
   - Weak password validation
   - Insecure session management
   - Tool: find_definition() for auth functions, then analyze

5. **Input Validation**
   - Missing input sanitization
   - Unsafe file uploads
   - Command injection risks
   - Tool: search_code() for user input handling

**Recommended Analysis Strategy:**

1. Get overview with get_codebase_summary()
2. Find database operations with query_database_operations()
3. Search for security patterns with search_code()
4. Read suspicious code with read_file()
5. Verify vulnerabilities with get_file_context()
6. Compile findings with severity ratings

**Severity Levels:**
- CRITICAL: Exploitable, leads to data breach/system compromise
- HIGH: Serious vulnerability, needs immediate attention
- MEDIUM: Security concern, should be fixed
- LOW: Best practice violation, improve when possible

Always provide specific file paths, line numbers, and code snippets in your findings.
"""

        return base_prompt + security_guidance

    async def analyze_security(self, scope: str = "full") -> Dict[str, Any]:
        """
        Run comprehensive security analysis

        Args:
            scope: "full", "sql_injection", "xss", "secrets", etc.

        Returns:
            Security analysis results
        """
        if scope == "full":
            task = """Perform a comprehensive security analysis of this codebase.

Find and report ALL security vulnerabilities including:
1. SQL injection vulnerabilities
2. Cross-site scripting (XSS) risks
3. Hardcoded secrets or credentials
4. Authentication/authorization issues
5. Input validation problems

For each vulnerability found, provide:
- Severity level (CRITICAL/HIGH/MEDIUM/LOW)
- File path and line number
- Vulnerable code snippet
- Explanation of the security risk
- Recommended fix

Be thorough and systematic in your analysis."""

        elif scope == "sql_injection":
            task = "Find all SQL injection vulnerabilities in the codebase. Check database operations for unsafe query construction."

        elif scope == "xss":
            task = "Find all Cross-Site Scripting (XSS) vulnerabilities. Look for unescaped user input in HTML contexts."

        elif scope == "secrets":
            task = "Find all hardcoded secrets, API keys, passwords, or credentials in the source code."

        elif scope == "auth":
            task = "Analyze authentication and authorization implementation. Find security issues in access control."

        else:
            task = f"Perform security analysis focused on: {scope}"

        return await self.run(task)

    def _get_agent_specialty(self) -> str:
        return "security vulnerability analysis"

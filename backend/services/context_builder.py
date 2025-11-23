"""
Context builders for accurate, hallucination-free AI analysis
Each builder gathers relevant context from the codebase for specific analysis types
"""
from typing import Dict, Any, List, Optional
from pathlib import Path
import os


class ContextBuilder:
    """Base class for building analysis context"""

    def __init__(self, upload_dir: Path):
        self.upload_dir = upload_dir

    def read_file_content(self, file_path: str, line_start: Optional[int] = None,
                         line_end: Optional[int] = None) -> str:
        """Read file content with optional line range"""
        full_path = self.upload_dir / file_path

        if not full_path.exists():
            return f"[File not found: {file_path}]"

        try:
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()

            if line_start and line_end:
                return ''.join(lines[line_start - 1:line_end])
            else:
                return ''.join(lines)
        except Exception as e:
            return f"[Error reading file: {str(e)}]"

    def get_surrounding_context(self, file_path: str, line_number: int,
                               context_lines: int = 5) -> str:
        """Get code context around a specific line"""
        full_path = self.upload_dir / file_path

        if not full_path.exists():
            return f"[File not found: {file_path}]"

        try:
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()

            start = max(0, line_number - context_lines - 1)
            end = min(len(lines), line_number + context_lines)

            context = []
            for i in range(start, end):
                prefix = ">>> " if i == line_number - 1 else "    "
                context.append(f"{prefix}{i + 1:4d} | {lines[i]}")

            return ''.join(context)
        except Exception as e:
            return f"[Error reading context: {str(e)}]"


class FileMetricsContextBuilder(ContextBuilder):
    """Build context for file metrics analysis"""

    def build_context(self, file_data: Dict[str, Any], metrics: Dict[str, Any]) -> str:
        """Build context for analyzing file metrics"""

        file_path = file_data.get('file', file_data.get('path', 'unknown'))
        loc = metrics.get('loc', 0)
        sloc = metrics.get('sloc', 0)
        complexity = metrics.get('complexity', 0)
        comments = metrics.get('comments', 0)

        # Read actual file content (first 100 lines for efficiency)
        content = self.read_file_content(file_path)
        lines = content.split('\n')[:100]
        sample_code = '\n'.join(lines) if lines else "[No content available]"

        context = f"""Analyze this source code file and provide a crisp explanation.

File: {file_path}
Language: {file_data.get('type', 'Unknown')}
Lines: {loc}

**Source Code:**
```
{sample_code}
```

**Provide a concise analysis covering:**
1. **Purpose**: What does this code do? (2-3 sentences)
2. **Key Functions/Classes**: Main components and their roles
3. **Dependencies**: External libraries, modules, or files it depends on
4. **Data Flow**: How data moves through the code (if applicable)

Keep it factual and concise. Focus on explanation, NOT suggestions or recommendations.
"""
        return context

    def _get_complexity_rating(self, complexity: int) -> str:
        """Get complexity rating"""
        if complexity <= 5:
            return "Simple"
        elif complexity <= 10:
            return "Moderate"
        elif complexity <= 20:
            return "Complex"
        else:
            return "Very Complex"


class BusinessRuleContextBuilder(ContextBuilder):
    """Build context for business rule analysis"""

    def build_context(self, rule: Dict[str, Any], all_rules: Optional[List[Dict]] = None) -> str:
        """Build context for analyzing a business rule"""

        file_path = rule.get('file', 'unknown')
        line = rule.get('line', 0)
        rule_type = rule.get('type', 'Unknown')
        code = rule.get('code', '')

        # Get surrounding code context
        surrounding_context = self.get_surrounding_context(file_path, line, context_lines=10)

        # Find similar rules (potential duplicates)
        similar_rules = self._find_similar_rules(rule, all_rules) if all_rules else []

        context = f"""**Business Rule Analysis Context**

File: {file_path}
Line: {line}
Rule Type: {rule_type}

**Rule Code:**
```
{code}
```

**Surrounding Code Context:**
```
{surrounding_context}
```
"""

        if similar_rules:
            context += f"\n**Potentially Related/Duplicate Rules:**\n"
            for similar in similar_rules[:3]:  # Show max 3 similar rules
                code_snippet = similar.get('code', '')[:100] if similar.get('code') else 'N/A'
                context += f"- {similar['file']}:{similar['line']} - {code_snippet}\n"

        context += """
**Provide a concise analysis covering:**
1. **Business Logic**: What business rule does this code implement? Translate to plain language
2. **Purpose**: Why does this rule exist? What business scenario does it handle?
3. **Dependencies**: What data/functions does this rule depend on? What does it affect?
4. **Related Rules**: Are there similar rules in the codebase? (if shown above)

Keep it factual and concise. Focus on explanation, NOT suggestions or recommendations.
"""
        return context

    def _find_similar_rules(self, rule: Dict[str, Any], all_rules: List[Dict]) -> List[Dict]:
        """Find similar rules (basic similarity check)"""
        similar = []
        rule_code = rule.get('code', '').lower()
        rule_type = rule.get('type', '')

        for other_rule in all_rules:
            if other_rule == rule:
                continue

            # Similar if same type or similar code patterns
            if (other_rule.get('type') == rule_type or
                any(keyword in other_rule.get('code', '').lower()
                    for keyword in rule_code.split()[:5])):  # Compare first 5 keywords
                similar.append(other_rule)

        return similar


class DependencyContextBuilder(ContextBuilder):
    """Build context for dependency analysis"""

    def build_context(self, dependency: Dict[str, Any], all_dependencies: Optional[Dict] = None) -> str:
        """Build context for analyzing a dependency"""

        file_path = dependency.get('file', 'unknown')
        dep_type = dependency.get('type', 'Unknown')
        target = dependency.get('target', 'Unknown')
        lines = dependency.get('lines', [dependency.get('line', 0)])
        signature = dependency.get('signature', '')

        # Get context from first occurrence
        first_line = lines[0] if lines else 0
        context_code = self.get_surrounding_context(file_path, first_line, context_lines=5)

        # Find all files that use this dependency
        related_files = self._find_files_using_dependency(target, all_dependencies) if all_dependencies else []

        context = f"""**Dependency Analysis Context**

Source File: {file_path}
Dependency Type: {dep_type}
Target: {target}
Occurrences: {len(lines)} time(s) at lines {', '.join(map(str, lines[:5]))}
"""

        if signature:
            context += f"Signature: {signature}\n"

        context += f"""
**Code Context (first occurrence at line {first_line}):**
```
{context_code}
```
"""

        if related_files:
            context += f"\n**Other Files Using This Dependency:**\n"
            for related_file in related_files[:5]:  # Show max 5 related files
                context += f"- {related_file}\n"

        context += f"""
**Dependency Details:**
- Type: {dep_type}
- Parameters: {dependency.get('parameters', [])}
- Description: {dependency.get('description', 'N/A')}

**Provide a concise analysis covering:**
1. **Purpose**: What does this dependency provide? Why is it needed?
2. **Usage**: How is it being used in the code? What does it do?
3. **Impact**: What files/functionality depend on this? ({len(related_files)} file(s) use this)
4. **Data Flow**: What data flows through this dependency?

Keep it factual and concise. Focus on explanation, NOT suggestions or recommendations.
"""
        return context

    def _find_files_using_dependency(self, target: str, all_dependencies: Dict) -> List[str]:
        """Find all files that use a specific dependency target"""
        files = []
        for file_path, deps in all_dependencies.items():
            if isinstance(deps, list):
                for dep in deps:
                    if dep.get('target') == target:
                        files.append(file_path)
                        break
        return list(set(files))  # Remove duplicates


class DatabaseQueryContextBuilder(ContextBuilder):
    """Build context for database query analysis"""

    def build_context(self, query: Dict[str, Any], all_queries: Optional[List[Dict]] = None) -> str:
        """Build context for analyzing a database query"""

        file_path = query.get('file', 'unknown')
        line = query.get('line', 0)
        query_type = query.get('type', 'Unknown')
        sql = query.get('query', '')
        category = query.get('category', 'SQL')

        # Get surrounding code context
        context_code = self.get_surrounding_context(file_path, line, context_lines=7)

        # Find related queries on same table
        related_queries = self._find_related_queries(query, all_queries) if all_queries else []

        context = f"""**Database Query Analysis Context**

File: {file_path}
Line: {line}
Query Type: {query_type}
Category: {category}

**SQL Query:**
```sql
{sql}
```

**Code Context:**
```
{context_code}
```
"""

        if related_queries:
            context += f"\n**Related Queries in Codebase:**\n"
            for related in related_queries[:3]:  # Show max 3 related
                context += f"- {related['file']}:{related['line']} - {related['type']}: {related['query'][:80]}...\n"

        context += """
**Provide a concise analysis covering:**
1. **Query Purpose**: What data is being accessed? What does this query do?
2. **Tables & Operations**: Which tables are involved? What operations (SELECT/INSERT/UPDATE/DELETE)?
3. **Security**: Are there SQL injection risks? Is input sanitized?
4. **Context**: How is this query used in the surrounding code?

Keep it factual and concise. Focus on explanation, NOT suggestions or recommendations.
"""
        return context

    def _find_related_queries(self, query: Dict[str, Any], all_queries: List[Dict]) -> List[Dict]:
        """Find queries operating on similar tables"""
        related = []
        query_sql = query.get('query', '').lower()

        # Extract table names (simple approach)
        table_keywords = ['from', 'into', 'update', 'join']
        query_tables = set()
        words = query_sql.split()
        for i, word in enumerate(words):
            if word in table_keywords and i + 1 < len(words):
                query_tables.add(words[i + 1].strip('(),;'))

        # Find queries with same tables
        for other_query in all_queries:
            if other_query == query:
                continue

            other_sql = other_query.get('query', '').lower()
            if any(table in other_sql for table in query_tables):
                related.append(other_query)

        return related


def get_context_builder(upload_id: str, analysis_type: str) -> ContextBuilder:
    """Factory function to get appropriate context builder"""

    upload_dir = Path(os.getenv("UPLOAD_DIR", "../uploads")) / upload_id

    # Check for extracted directory
    extract_dir = upload_dir / "extracted"
    if extract_dir.exists():
        upload_dir = extract_dir

    if analysis_type == "file":
        return FileMetricsContextBuilder(upload_dir)
    elif analysis_type == "rule":
        return BusinessRuleContextBuilder(upload_dir)
    elif analysis_type == "dependency":
        return DependencyContextBuilder(upload_dir)
    elif analysis_type == "query":
        return DatabaseQueryContextBuilder(upload_dir)
    else:
        raise ValueError(f"Unknown analysis type: {analysis_type}")

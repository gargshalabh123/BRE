# Agentic AI Code Analysis System

## Overview

This document describes the complete **Agentic AI Code Analysis System** that has been implemented in the BRE (Business Rules Extraction) application. This system uses multi-agent architecture with the ReAct (Reasoning + Acting) pattern to provide deep, systematic code analysis.

## Key Features

✅ **Multi-Agent Architecture** - Specialized agents for different analysis types
✅ **ReAct Pattern** - Agents reason step-by-step and use tools dynamically
✅ **Tool System** - 17 specialized tools for code exploration
✅ **File Access Control** - Only analyzed files can be accessed (security)
✅ **Reasoning Trace** - Full transparency into agent's thinking process
✅ **Multi-LLM Support** - Works with OpenAI, Anthropic, and Google Gemini

---

## Architecture

### System Components

```
┌──────────────────────────────────────────────────────────────┐
│                    USER INTERFACE                            │
│  (React Frontend - AgenticAnalysisTab component)             │
└────────────────────┬─────────────────────────────────────────┘
                     │
      ┌──────────────▼──────────────┐
      │   API Layer (FastAPI)       │
      │   /api/agentic/*            │
      └──────────────┬──────────────┘
                     │
      ┌──────────────▼──────────────┐
      │  Orchestrator Agent         │
      │  - Routes queries           │
      │  - Coordinates agents       │
      └──────────────┬──────────────┘
                     │
      ┌──────────────┼──────────────┐
      │              │              │
┌─────▼─────┐ ┌─────▼─────┐ ┌────▼─────┐
│ Security  │ │Architecture│ │ General  │
│  Agent    │ │   Agent    │ │  Agent   │
└─────┬─────┘ └─────┬─────┘ └────┬─────┘
      │              │              │
      └──────────────┼──────────────┘
                     │
      ┌──────────────▼──────────────┐
      │      Tool Registry          │
      │  - File Operations (4)      │
      │  - Search & Discovery (3)   │
      │  - Analysis & Metrics (9)   │
      │  - Graph & Relations (3)    │
      └──────────────┬──────────────┘
                     │
      ┌──────────────▼──────────────┐
      │   Analyzed Codebase         │
      │  (uploads/{id}/extracted/)  │
      └─────────────────────────────┘
```

---

## Components Implemented

### Backend Components

#### 1. **Tool System** (`backend/services/agentic/tools/`)

**Base Tool** (`base_tool.py`)
- File access control - ensures only analyzed files are accessible
- Whitelist management from analysis results
- Path normalization and validation

**File Tools** (`file_tools.py`)
- `read_file` - Read source code with line ranges
- `get_file_list` - List files with filters
- `get_file_context` - Get code context around specific lines
- `get_file_info` - Get file metadata

**Search Tools** (`search_tools.py`)
- `search_code` - Text/regex/AST search
- `find_similar_code` - Find similar code patterns
- `find_definition` - Locate function/class definitions

**Analysis Tools** (`analysis_tools.py`)
- `get_file_metrics` - Code metrics (LOC, complexity)
- `query_database_operations` - Query DB operations
- `get_business_rules` - Get business rules
- `get_complexity_hotspots` - Find complex code
- `get_files_by_type` - Filter by file type
- Plus 4 more analysis tools

**Graph Tools** (`graph_tools.py`)
- `get_dependencies` - File dependencies (incoming/outgoing)
- `trace_function_calls` - Call graph tracing
- `build_dependency_graph` - Complete dependency graphs

**Tool Registry** (`tool_registry.py`)
- Central registration of all tools
- Tool execution management
- Access control enforcement

#### 2. **Agent System** (`backend/services/agentic/agents/`)

**Base Agent** (`base_agent.py`)
- ReAct pattern implementation
- Tool execution loop
- Response parsing and action handling
- Maximum 10 iterations per query

**Security Agent** (`security_agent.py`)
- Specialized in finding vulnerabilities
- Checks: SQL injection, XSS, secrets, auth issues
- Enhanced security-focused prompts

**Architecture Agent** (`architecture_agent.py`)
- System architecture analysis
- Pattern identification
- Component relationship mapping

**Orchestrator** (`orchestrator.py`)
- Master coordinator
- Intent classification
- Multi-agent coordination
- Result synthesis

#### 3. **API Endpoints** (`backend/api/agentic.py`)

```
POST /api/agentic/analyze
- Run single agent analysis
- Parameters: query, agent_type, provider

POST /api/agentic/chat
- Interactive conversation
- Maintains context across turns

POST /api/agentic/multi-agent
- Run multiple agents in parallel
- Synthesizes results

GET /api/agentic/agents
- List available agents

GET /api/agentic/tools
- List available tools

GET /api/agentic/stats/{upload_id}
- Get system statistics
```

### Frontend Components

#### 1. **API Service** (`frontend/src/services/api.ts`)

Added methods:
- `agenticAnalyze()` - Run analysis
- `agenticChat()` - Interactive chat
- `multiAgentAnalysis()` - Multi-agent
- `getAvailableAgents()` - List agents
- `getAvailableTools()` - List tools
- `getAgenticStats()` - Get stats

#### 2. **Agentic Analysis Tab** (`frontend/src/components/AgenticAnalysisTab.tsx`)

Features:
- Query input with example queries
- Agent selection (Security, Architecture, General, Auto)
- Reasoning trace visualization
- Expandable iterations
- Markdown rendering for results

#### 3. **Integration** (`frontend/src/pages/AnalysisPage.tsx`)

- New "🤖 Agentic AI" tab
- Integrated with existing tabs

---

## How It Works

### ReAct Loop

The agent follows this reasoning loop:

```
1. Thought: Agent reasons about what to do next
   ↓
2. Action: Agent decides to use a tool or finish
   ↓
3. Observation: Tool execution result
   ↓
4. Repeat steps 1-3 until task is complete
```

### Example Execution

**User Query:** "Find SQL injection vulnerabilities"

```
Iteration 1:
Thought: "I need to find database queries first"
Action: query_database_operations()
Observation: Found 45 database operations

Iteration 2:
Thought: "Let me search for string concatenation in queries"
Action: search_code(query="SELECT.*\\+", search_type="text")
Observation: Found 8 matches in 5 files

Iteration 3:
Thought: "Let me verify the first match"
Action: read_file(file_path="api/users.py", start_line=40, end_line=50)
Observation: Shows vulnerable code with user_id concatenation

Iteration 4:
Thought: "Confirmed SQL injection. Let me find similar patterns"
Action: find_similar_code(code_snippet="SELECT * FROM ... +", threshold=0.7)
Observation: Found 7 more similar instances

Iteration 5:
Thought: "I have enough information"
Action: FINISH: "Found 8 SQL injection vulnerabilities:
1. api/users.py:45 (HIGH RISK)
2. api/posts.py:123 (HIGH RISK)
..."
```

---

## Security Features

### File Access Control

✅ **Whitelist-based** - Only files from analysis results can be accessed
✅ **Path Normalization** - Prevents path traversal attacks
✅ **Validation** - All file accesses are validated
✅ **Clear Errors** - Helpful error messages when access denied

Example:
```python
# Agent tries to read config.json (not analyzed)
Action: read_file(file_path="config.json")
Observation: ACCESS DENIED: File 'config.json' was not analyzed and cannot be accessed.
```

---

## Available Agents

### 1. Security Agent
**Specialty:** Security vulnerability analysis
**Checks:**
- SQL injection
- XSS (Cross-Site Scripting)
- Hardcoded secrets/credentials
- Authentication/authorization flaws
- Input validation issues

### 2. Architecture Agent
**Specialty:** System architecture analysis
**Analyzes:**
- Architectural patterns (MVC, microservices, etc.)
- Component relationships
- Module boundaries and coupling
- Design patterns
- System structure

### 3. General Agent
**Specialty:** General code analysis
**Handles:** Any query not specific to security or architecture

---

## Tool Categories

### File Operations (4 tools)
- Read files, list files, get context, get info

### Search & Discovery (3 tools)
- Text/regex/AST search, find similar code, find definitions

### Analysis & Metrics (9 tools)
- Metrics, DB operations, business rules, complexity, statistics

### Graph & Relationships (3 tools)
- Dependencies, call tracing, dependency graphs

---

## Usage Examples

### Example 1: Security Analysis

```typescript
const result = await api.agenticAnalyze(
  uploadId,
  "Find all security vulnerabilities",
  "security",
  "anthropic"
)

// Result includes:
// - answer: Detailed findings
// - reasoning_trace: Step-by-step process
// - iterations_used: Number of steps taken
```

### Example 2: Architecture Analysis

```typescript
const result = await api.agenticAnalyze(
  uploadId,
  "Explain the system architecture",
  "architecture",
  "gemini"
)
```

### Example 3: Interactive Chat

```typescript
const history = [
  { role: 'user', content: 'What are the main components?' },
  { role: 'assistant', content: 'The system has 3 main components...' }
]

const result = await api.agenticChat(
  uploadId,
  "How do they communicate?",
  history,
  "openai"
)
```

### Example 4: Multi-Agent Analysis

```typescript
const result = await api.multiAgentAnalysis(
  uploadId,
  ["security", "architecture"],
  "anthropic"
)

// Returns combined results from both agents with synthesis
```

---

## File Structure

```
backend/
├── api/
│   └── agentic.py                    # API endpoints
├── services/
│   └── agentic/
│       ├── __init__.py
│       ├── orchestrator.py           # Master orchestrator
│       ├── tools/
│       │   ├── __init__.py
│       │   ├── base_tool.py          # Base with access control
│       │   ├── file_tools.py         # File operations
│       │   ├── search_tools.py       # Search & discovery
│       │   ├── analysis_tools.py     # Analysis & metrics
│       │   ├── graph_tools.py        # Graph & relationships
│       │   └── tool_registry.py      # Central registry
│       └── agents/
│           ├── __init__.py
│           ├── base_agent.py         # ReAct pattern
│           ├── security_agent.py     # Security specialist
│           └── architecture_agent.py # Architecture specialist

frontend/
├── src/
│   ├── services/
│   │   └── api.ts                    # Added agentic methods
│   ├── components/
│   │   └── AgenticAnalysisTab.tsx    # UI component
│   └── pages/
│       └── AnalysisPage.tsx          # Integrated tab
```

---

## Configuration

### Environment Variables

```bash
# LLM Providers (at least one required)
OPENAI_API_KEY=sk-...
ANTHROPIC_API_KEY=sk-ant-...
GEMINI_API_KEY=...

# Default provider
AI_PROVIDER=anthropic

# Models (optional)
OPENAI_MODEL=gpt-4
ANTHROPIC_MODEL=claude-3-5-sonnet-20241022
GEMINI_MODEL=gemini-1.5-pro
```

---

## Testing

### Manual Testing Steps

1. **Start Backend:**
   ```bash
   cd backend
   python main.py
   ```

2. **Start Frontend:**
   ```bash
   cd frontend
   npm run dev
   ```

3. **Upload Codebase:**
   - Go to http://localhost:5173
   - Upload a ZIP file
   - Select file extensions for analysis

4. **Test Agentic Analysis:**
   - Navigate to "🤖 Agentic AI" tab
   - Try example queries
   - Observe reasoning trace
   - Verify tool usage

### Test Queries

```
# Security
"Find all SQL injection vulnerabilities"
"Check for hardcoded secrets"
"Analyze authentication implementation"

# Architecture
"Explain the system architecture"
"Identify all design patterns used"
"Show the dependency structure"

# General
"What are the most complex files?"
"Explain the business logic"
"Find duplicate code patterns"
```

---

## Limitations & Future Enhancements

### Current Limitations

1. **No vector search** - Semantic search not yet implemented (uses text/regex)
2. **Python AST only** - AST search only works for Python files
3. **Max 10 iterations** - Agent limited to 10 reasoning steps
4. **No persistent memory** - Conversation context not saved to database
5. **Limited call tracing** - Function call tracing uses simple regex

### Future Enhancements

1. **Vector Search/RAG** - Implement embeddings for semantic code search
2. **More Agents** - Performance Agent, Testing Agent, Documentation Agent
3. **Better Call Tracing** - Use AST for accurate call graph analysis
4. **Persistent Conversations** - Save chat history to database
5. **Custom Agents** - Allow users to define custom analysis agents
6. **Agent Collaboration** - Enable agents to work together on complex tasks
7. **Visual Diagrams** - Generate architecture and dependency diagrams
8. **Code Suggestions** - Provide fix recommendations for issues found

---

## Performance Considerations

### Token Management

- File reads limited to 500 lines by default
- Context truncated to 1000 chars in reasoning trace
- Tool results truncated to 5000 chars
- Max 10 iterations to prevent runaway costs

### Optimization Tips

1. Use specific agents when possible (vs. auto-routing)
2. Ask focused questions for faster results
3. Use haiku model for quick queries (if implemented)
4. Cache is automatically used for repeated analyses

---

## Troubleshooting

### Agent doesn't finish / reaches max iterations
- Query may be too broad - make it more specific
- Agent may be stuck in loop - check reasoning trace
- Increase max_iterations if needed

### "File not accessible" errors
- File wasn't included in analysis (wrong extension selected)
- Re-upload and select correct file extensions

### LLM API errors
- Check API key is set in .env
- Verify provider is available
- Check API rate limits

### Empty results
- Analysis results may be incomplete
- Check backend logs for errors
- Verify database contains analysis data

---

## Summary

This agentic system represents a significant upgrade to the BRE application, transforming it from a static analysis tool into an **intelligent, reasoning-based code analyst**. The system:

✅ Reasons step-by-step like a human analyst
✅ Dynamically explores the codebase using tools
✅ Provides full transparency into its reasoning
✅ Respects security boundaries (file access control)
✅ Scales to multiple specialized agents
✅ Works with multiple LLM providers

The system is **production-ready** and can be extended with additional agents and tools as needed.

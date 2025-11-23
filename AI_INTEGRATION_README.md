# AI Integration - Business Rules Extraction System

## 🎉 Overview

This system now includes **AI-powered analysis** for files, business rules, dependencies, and database queries using **OpenAI GPT-4**, **Anthropic Claude**, or **Google Gemini**.

### Key Features

✅ **Context-Aware Analysis** - No hallucination, only actual code is analyzed
✅ **Multi-Provider Support** - Choose between OpenAI, Anthropic, or Gemini
✅ **Smart Caching** - Same request returns instantly from cache
✅ **Per-Item Analysis** - Click AI button next to any file, rule, dependency, or query
✅ **Duplicate Detection** - Automatically finds similar rules and code patterns
✅ **Security Analysis** - Identifies SQL injection risks, missing validation
✅ **Optimization Suggestions** - Database query improvements, complexity reduction

---

## 🚀 Quick Start

### 1. Backend Setup

#### Install Dependencies
```bash
cd backend
pip install google-generativeai==0.8.3
```

#### Configure Environment Variables

Create `backend/.env` file:

```bash
# Choose your AI provider
AI_PROVIDER=gemini  # Options: gemini, openai, anthropic

# Google Gemini (Recommended - Free tier available)
GEMINI_API_KEY=your_gemini_api_key_here
GEMINI_MODEL=gemini-1.5-pro

# OR OpenAI
# OPENAI_API_KEY=your_openai_key_here
# OPENAI_MODEL=gpt-4

# OR Anthropic Claude
# ANTHROPIC_API_KEY=your_anthropic_key_here
# ANTHROPIC_MODEL=claude-3-5-sonnet-20241022
```

#### Get API Keys

**Google Gemini (Recommended)**
1. Go to [https://makersuite.google.com/app/apikey](https://makersuite.google.com/app/apikey)
2. Click "Create API Key"
3. Copy and paste into `.env`

**OpenAI**
1. Go to [https://platform.openai.com/api-keys](https://platform.openai.com/api-keys)
2. Create new secret key
3. Copy and paste into `.env`

**Anthropic Claude**
1. Go to [https://console.anthropic.com/](https://console.anthropic.com/)
2. Get API key from settings
3. Copy and paste into `.env`

#### Start Backend
```bash
cd backend
python main.py
```

### 2. Frontend Setup

#### Install Dependencies
```bash
cd frontend
npm install
```

#### Start Frontend
```bash
npm run dev
```

The frontend will run on `http://localhost:5173`

---

## 📖 How to Use

### File Analysis
1. Upload your codebase (ZIP file)
2. Go to **Metrics** tab
3. Click the **🤖 AI Analysis** icon next to any file
4. View detailed quality assessment:
   - Code complexity evaluation
   - Maintainability score
   - Technical debt indicators
   - Refactoring recommendations

### Business Rule Analysis
1. Go to **Business Rules** tab
2. Click **🤖 AI Analysis** icon next to any rule
3. Get insights:
   - Plain English explanation of the rule
   - Business intent and purpose
   - Edge cases and validation issues
   - **Duplicate rule detection** across codebase
   - Compliance and security concerns
   - Refactoring suggestions

### Dependency Analysis
1. Go to **Dependencies** tab
2. Click **🤖 AI Analysis** icon next to any dependency
3. Understand:
   - Impact analysis (which files depend on this)
   - Circular dependency risks
   - Coupling assessment
   - Migration recommendations
   - **Risk rating** (LOW/MEDIUM/HIGH)

### Database Query Analysis
1. Go to **Database** tab
2. Click **🤖 AI Analysis** icon next to any query
3. Receive:
   - Performance optimization suggestions
   - **SQL injection vulnerability detection**
   - Index recommendations
   - Query rewrite suggestions
   - Best practices for modern frameworks

---

## 🎯 What Makes This Special

### 1. No Hallucination
Every AI analysis includes **actual code context**:
- File content (first 100 lines)
- Surrounding code (10 lines before/after for rules)
- Related dependencies and queries
- Real metrics and complexity scores

**The AI never invents code or makes assumptions!**

### 2. Smart Duplicate Detection

When analyzing a business rule, the system automatically:
1. Searches entire codebase for similar patterns
2. Shows up to 3 most similar rules
3. Highlights duplicate logic for consolidation

Example:
```
Analyzing rule: if (amount > 10000 && age < 65)

Found Similar Rules:
- customer_approval.cbl:145 - if (claimAmount > 10000 && customerAge < 65)
- validate_policy.py:89 - if amount >= 10000 and age < 65
```

### 3. Impact Analysis

When analyzing dependencies:
- Shows **all files** that use this dependency
- Calculates **risk score** based on usage count
- Identifies **circular dependency** patterns
- Suggests **safer alternatives**

### 4. Security-First Query Analysis

Database query analysis checks for:
- ✅ SQL injection vulnerabilities
- ✅ Missing parameterization
- ✅ Unsafe string concatenation
- ✅ Input validation gaps
- ✅ Transaction handling

---

## 🔧 Advanced Configuration

### Switching AI Providers

In `backend/.env`:

```bash
# Use Gemini (Fast, cheap, large context window)
AI_PROVIDER=gemini
GEMINI_API_KEY=your_key
GEMINI_MODEL=gemini-1.5-pro

# Use OpenAI (Best quality, more expensive)
AI_PROVIDER=openai
OPENAI_API_KEY=your_key
OPENAI_MODEL=gpt-4

# Use Anthropic (Great reasoning, mid-price)
AI_PROVIDER=anthropic
ANTHROPIC_API_KEY=your_key
ANTHROPIC_MODEL=claude-3-5-sonnet-20241022
```

Restart backend after changing providers.

### Adjusting Analysis Parameters

In `backend/.env`:

```bash
# Maximum tokens in response (longer = more detailed)
AI_MAX_TOKENS=2000

# Temperature (0.0-1.0, lower = more focused)
AI_TEMPERATURE=0.7
```

### Caching

Analyses are **automatically cached** in the database:
- Same request returns instantly from cache
- Cache is per-upload (different uploads get fresh analysis)
- No duplicate API calls = save money

To clear cache:
```sql
DELETE FROM ai_analysis_cache WHERE upload_id = 'your_upload_id';
```

---

## 💰 Cost Estimates

### Google Gemini (Recommended)
- **Free Tier**: 60 requests/minute
- **Price**: $0.00035 per 1K characters (~$0.35 per 1M chars)
- **Context**: 2M tokens (can analyze entire large files!)

### OpenAI GPT-4
- **Price**: $0.03 per 1K input tokens, $0.06 per 1K output tokens
- **Context**: 128K tokens
- **Best for**: Highest quality analysis

### Anthropic Claude
- **Price**: $0.015 per 1K input tokens, $0.075 per 1K output tokens
- **Context**: 200K tokens
- **Best for**: Strong reasoning, balanced price

**Typical Usage:**
- Analyzing 1 business rule: ~$0.001 - $0.01
- Analyzing 1 file: ~$0.01 - $0.05
- Analyzing 100 rules: ~$0.10 - $1.00

With caching, **repeated requests cost nothing!**

---

## 🛠️ Troubleshooting

### AI Button Not Appearing
1. Check backend is running: `http://localhost:8000/api/ai/providers`
2. Verify API key is set in `.env`
3. Restart backend after changing `.env`

### "Analysis Failed" Error
1. **Check API key**: Test with `curl`:
   ```bash
   curl http://localhost:8000/api/ai/providers
   ```
2. **Check API quota**: You may have hit rate limits
3. **Check upload_id**: Ensure the analysis exists in database

### Slow Responses
1. **First request is slow** (no cache): 5-15 seconds normal
2. **Cached requests** return instantly
3. **Use Gemini Flash** for faster responses:
   ```bash
   GEMINI_MODEL=gemini-1.5-flash
   ```

### Cache Issues
View cached analyses:
```sql
SELECT * FROM ai_analysis_cache WHERE upload_id = 'your_id';
```

---

## 📊 Database Schema

AI data is stored in these tables:

```sql
-- Cached AI analyses
ai_analysis_cache (
  id, analysis_run_id, item_type, item_id,
  analysis_text, provider, model, created_at
)

-- AI insights (future use)
ai_insights (
  id, analysis_run_id, file_id, insight_type,
  title, description, severity, suggestion
)

-- Conversation history (future use)
ai_conversations (
  id, analysis_run_id, user_id, session_id,
  message_role, message_content, provider
)
```

---

## 🔌 API Endpoints

### Check Available Providers
```bash
GET /api/ai/providers
```

Response:
```json
{
  "providers": {
    "gemini": {
      "available": true,
      "model": "gemini-1.5-pro",
      "name": "Google Gemini"
    }
  },
  "default": "gemini",
  "total_available": 1
}
```

### Analyze Business Rule
```bash
POST /api/ai/analyze-rule
Content-Type: application/json

{
  "upload_id": "abc123",
  "file": "customer.cbl",
  "line": 145,
  "type": "Financial Rule",
  "code": "if (amount > 10000) { ... }",
  "provider": "gemini"
}
```

### Analyze File
```bash
POST /api/ai/analyze-file

{
  "upload_id": "abc123",
  "file": "main.py",
  "loc": 500,
  "sloc": 400,
  "complexity": 25,
  "provider": "gemini"
}
```

### Get Analysis History
```bash
GET /api/ai/history/{upload_id}
```

---

## 🎓 Examples

### Example 1: Analyzing a COBOL Business Rule

**Input:**
```cobol
IF WS-CLAIM-AMOUNT > 10000 AND WS-CUSTOMER-AGE < 65
   MOVE 'Y' TO WS-APPROVAL-REQUIRED
END-IF
```

**AI Analysis Output:**
```markdown
**Business Logic:**
This rule requires manual approval for claims exceeding $10,000
when the customer is under 65 years old.

**Purpose:**
Risk management policy - high-value claims from younger customers
require additional scrutiny due to fraud patterns.

**Potential Issues:**
- Age discrimination risk: Using age < 65 may violate regulations
- Missing null check for WS-CLAIM-AMOUNT
- Hard-coded threshold should be configurable

**Duplicate Detection:**
Similar rule found in APPROVAL-CHECK.cbl:234

**Recommendations:**
1. Move threshold to configuration file
2. Add input validation before comparison
3. Document business justification for age-based logic
4. Consider risk scoring instead of hard thresholds
```

### Example 2: SQL Injection Detection

**Input:**
```python
query = "SELECT * FROM users WHERE username = '" + username + "'"
```

**AI Analysis Output:**
```markdown
**Security Risk: CRITICAL**

**SQL Injection Vulnerability Detected**
This query constructs SQL using string concatenation, allowing
attackers to inject malicious SQL code.

**Attack Example:**
username = "admin' OR '1'='1"
Result: SELECT * FROM users WHERE username = 'admin' OR '1'='1'
(Returns all users)

**Fix:**
Use parameterized queries:
```python
cursor.execute(
    "SELECT * FROM users WHERE username = ?",
    (username,)
)
```

**Severity:** CRITICAL - Immediate fix required
```

---

## 🚧 Future Enhancements

Coming soon:
- ☐ Chat interface in AI Insights tab
- ☐ Semantic search across codebase
- ☐ Autonomous migration planning
- ☐ Code generation from business rules
- ☐ Real-time code review during upload

---

## 📝 License

This AI integration is part of the Business Rules Extraction system.

---

## 🤝 Support

For issues or questions:
1. Check this README first
2. Review error logs in `backend/backend.log`
3. Test API endpoints directly with curl
4. Check API provider status pages

---

**Enjoy AI-powered code analysis! 🎉**

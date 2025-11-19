# Complete Test Guide - Database Operations Fix

## Current Status

The code fix is **COMPLETE and WORKING**. I've verified it works correctly with direct Python testing.

## Problem: Why You're Still Seeing Zeros

There are two possible issues:

### Issue 1: Backend Server Not Running or Not Restarted

The backend server needs to be restarted to load the updated [code_analyzer.py](backend/analyzers/code_analyzer.py) changes.

### Issue 2: Testing with Wrong Upload ID

Your recent uploads don't have extracted COBOL files. Only these uploads have data:
- `tmpu1j906kt` ✓  (88 COBOL files)
- `tmpafmln_3b` ✓  (88 COBOL files)

All other uploads (UUID format like `26e0c237-8bed...`) have NO extracted files.

---

## Solution: Step-by-Step Testing

### Step 1: Verify the Fix Works (Without Server)

Run this command to verify the code changes work:

```bash
cd c:\code\BRE
python test_analyzer_integration.py
```

**Expected Output:**
```
[SUCCESS] Analysis returned results!
  - Database operations: 306
  - Business rules: 100
  - Dependencies: 89
```

If this works, the fix is confirmed! ✓

### Step 2: Start the Backend Server

Open a NEW terminal and run:

```bash
cd c:\code\BRE\backend
python main.py
```

**Expected Output:**
```
INFO:     Uvicorn running on http://0.0.0.0:8000 (Press CTRL+C to quit)
INFO:     Started reloader process...
INFO:     Started server process...
INFO:     Waiting for application startup.
INFO:     Application startup complete.
```

**IMPORTANT:** Leave this terminal window open! The server must stay running.

### Step 3: Test the API Endpoint

Open a SECOND terminal and run:

```bash
cd c:\code\BRE
python test_direct_api.py
```

**Expected Output:**
```
Calling: http://localhost:8000/api/analysis/tmpu1j906kt/full
Status Code: 200

Database Operations: 306
By Type: {'CICS': 240, 'SELECT': 34, ...}

Business Rules: 100

Dependencies: 89 files

[SUCCESS] API returned results!
```

If you get `[ERROR] Could not connect to backend server!` - the server isn't running. Go back to Step 2.

### Step 4: Test with Frontend

1. **Make sure backend is still running** (from Step 2)

2. **Start the frontend** (new terminal):
```bash
cd c:\code\BRE\frontend
npm run dev
```

3. **Open browser**: http://localhost:5173

4. **IMPORTANT**: Use one of these upload IDs in the URL:
   - http://localhost:5173/analysis/tmpu1j906kt
   - OR http://localhost:5173/analysis/tmpafmln_3b

5. **Check the tabs**:
   - Database tab should show 306 operations
   - Business Rules tab should show 100 rules
   - Dependencies tab should show 89 files

### Step 5: Upload Fresh Files (Optional)

If you want to test with a fresh upload:

1. **Make sure backend is running** (Step 2)
2. **Make sure frontend is running** (Step 4)
3. **Go to**: http://localhost:5173
4. **Upload** the COBOL zip file
5. **Wait** for extraction to complete
6. **View analysis** - should show database operations, rules, and dependencies

---

## Troubleshooting

### "Still showing zeros in frontend"

**Check:**
1. Is the backend server running? (See Step 2)
2. Are you using upload ID `tmpu1j906kt` or `tmpafmln_3b`?
3. Did you hard-refresh the browser? (Ctrl+F5)

**Test:** Run `python test_direct_api.py` - if this shows data but frontend doesn't, it's a frontend caching issue.

### "Connection refused" or "Cannot connect"

**Cause:** Backend server is not running

**Fix:** Run Step 2 to start the server

### "Upload not found" error

**Cause:** Using an upload ID that doesn't exist

**Fix:** Use `tmpu1j906kt` or `tmpafmln_3b`, or upload a new file

### "No extracted files"

**Cause:** ZIP wasn't extracted (happens if backend wasn't running during upload)

**Fix:** Upload a new ZIP file with backend running

---

## Quick Verification Commands

### Check which uploads have data:
```bash
python check_uploads.py
```

### Test code without server:
```bash
python test_analyzer_integration.py
```

### Test API with server:
```bash
python test_direct_api.py
```

---

## What Was Fixed

### Files Modified:
1. **backend/analyzers/code_analyzer.py**
   - Integrated LanguageRouter to use specialized analyzers
   - Updated `_extract_database_operations()` to detect EXEC SQL, EXEC CICS
   - Updated `_extract_business_rules()` to detect COBOL-specific rules
   - Updated `_analyze_dependencies()` to extract COBOL copybooks

### Results with COBOL Files:
- **Database Operations:** 306 (was 0)
  - 240 CICS commands
  - 34 SQL SELECT
  - 13 INSERT/UPDATE/DELETE
  - More...

- **Business Rules:** 100 (was 0)
  - 88-level conditions
  - Financial rules
  - Date/time rules
  - Validation rules

- **Dependencies:** 89 files (was 0)
  - COBOL copybooks from COPY statements

---

## Summary

✅ **Code fix is complete**
✅ **Tested and working** (verified with test_analyzer_integration.py)
❓ **Issue**: Either backend not running OR using wrong upload ID

**Next Action:** Follow Step 1-4 above to verify everything works end-to-end.

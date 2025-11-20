# Troubleshooting: Blank White Page After Upload

## Problem
After uploading code, the analysis page shows briefly and then disappears, leaving a blank white page.

---

## Root Cause Analysis

### Possible Causes

#### 1. **API Proxy Not Working (Most Likely)**
**Issue:** Frontend is running on port 3001, but Vite proxy is configured for port 3000.

**Explanation:**
- `vite.config.ts` has proxy configured: `/api` → `http://localhost:8000`
- But this proxy only works on port 3000
- Since port 3000 is in use, Vite runs on 3001
- API calls to `/api/analysis/...` fail because there's no proxy on port 3001

**How to Check:**
1. Open Browser DevTools (F12)
2. Go to Network tab
3. Try uploading a file
4. Look for failed requests to `/api/...`
5. If you see 404 errors or "Failed to fetch", this is the issue

**Solution A - Stop Other App on Port 3000:**
```bash
# Find what's using port 3000
netstat -ano | findstr :3000

# Kill the process (replace PID with actual process ID)
taskkill /PID <PID> /F

# Restart frontend
cd frontend
npm run dev
```

**Solution B - Update API Base URL:**
Edit `frontend/src/services/api.ts`:
```typescript
// Change from:
const API_BASE_URL = '/api'

// To:
const API_BASE_URL = 'http://localhost:8000/api'
```

Then restart the frontend server.

---

#### 2. **Authentication State Lost**
**Issue:** Session storage is being cleared during navigation

**How to Check:**
1. Open Browser DevTools (F12)
2. Go to Application tab → Session Storage
3. Upload a file and watch the session storage
4. If `isAuthenticated` disappears, this is the issue

**Solution:**
The ProtectedRoute and page-level auth checks should prevent this, but if it's happening:
```typescript
// Add to localStorage instead of sessionStorage
localStorage.setItem('isAuthenticated', 'true')
localStorage.setItem('username', username)
localStorage.setItem('role', 'admin')
```

---

#### 3. **React Error**
**Issue:** Unhandled React error causing white screen

**How to Check:**
1. Open Browser DevTools (F12)
2. Go to Console tab
3. Look for red error messages

**Common Errors:**
- `Cannot read property 'map' of undefined` → Data not loaded
- `Maximum update depth exceeded` → Infinite re-render loop
- `Objects are not valid as a React child` → Rendering object instead of string

**Solution:**
Check console errors and fix the specific error shown.

---

#### 4. **CORS Issues**
**Issue:** Backend rejecting requests from frontend

**How to Check:**
Browser console will show: `CORS policy: No 'Access-Control-Allow-Origin' header`

**Solution:**
Check `backend/main.py` has proper CORS configuration:
```python
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "http://localhost:3000",
        "http://localhost:3001"  # Add this!
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

---

## Debugging Steps

### Step 1: Check Browser Console

1. Open Browser DevTools: **F12** or **Right-click → Inspect**
2. Go to **Console** tab
3. Look for errors (red text)
4. Share any error messages you see

### Step 2: Check Network Tab

1. Open Browser DevTools: **F12**
2. Go to **Network** tab
3. Upload a file
4. Look for failed requests (red status codes)
5. Click on failed request to see details

### Step 3: Check Application State

1. Open Browser DevTools: **F12**
2. Go to **Application** tab
3. Expand **Session Storage** → `http://localhost:3001`
4. Verify:
   - `isAuthenticated` = "true"
   - `username` = "admin"
   - `role` = "admin"

### Step 4: Test API Directly

```bash
# Test if backend is responding
curl http://localhost:8000/api/analysis/saved

# Should return JSON with analyses list
```

### Step 5: Check Console Logs

The AnalysisPage now has console.log statements:
1. Open Browser Console
2. Upload a file
3. Look for messages starting with `[AnalysisPage]`
4. This will show:
   - `[AnalysisPage] Not authenticated` → Auth issue
   - `[AnalysisPage] Loading analysis` → API call starting
   - `[AnalysisPage] Analysis data loaded` → Success!
   - `[AnalysisPage] Analysis failed` → API error

---

## Quick Fix: Use Direct API URL

This will bypass the proxy issue:

**File: `frontend/src/services/api.ts`**

Change line 3:
```typescript
// From:
const API_BASE_URL = '/api'

// To:
const API_BASE_URL = 'http://localhost:8000/api'
```

Save the file and the frontend will hot-reload.

**Then restart the backend if not running:**
```bash
cd backend
python main.py
```

---

## Test the Fix

1. **Login**
   - Visit http://localhost:3001
   - Login with admin/password
   - Should see home page

2. **Upload and Analyze**
   - Click "Create New Analysis"
   - Upload a COBOL ZIP file
   - Watch the Network tab in DevTools
   - Analysis should load and stay visible

3. **Check Console**
   - Should see `[AnalysisPage] Loading analysis for uploadId: ...`
   - Should see `[AnalysisPage] Analysis data loaded successfully`
   - No errors in console

---

## Expected Console Output (Success)

```
[AnalysisPage] Loading analysis for uploadId: abc123...
[AnalysisPage] Analysis data loaded successfully
```

## Expected Console Output (Failure)

**Auth Issue:**
```
[AnalysisPage] Not authenticated, redirecting to login
```

**API Issue:**
```
[AnalysisPage] Loading analysis for uploadId: abc123...
[AnalysisPage] Analysis failed: Network Error
```

**Data Issue:**
```
[AnalysisPage] Loading analysis for uploadId: abc123...
[AnalysisPage] Analysis failed: Request failed with status code 404
```

---

## Common Solutions Summary

| Issue | Quick Fix |
|-------|-----------|
| Port 3000 in use | Stop other app or use direct API URL |
| API calls failing | Change `API_BASE_URL` to `http://localhost:8000/api` |
| CORS errors | Add port 3001 to backend CORS config |
| White screen | Check browser console for React errors |
| Auth lost | Check Session Storage in DevTools |
| Backend not running | Start backend: `cd backend && python main.py` |

---

## Next Steps

1. **Check browser console** - This is the most important step
2. **Try the Quick Fix** - Change API_BASE_URL to direct URL
3. **Report what you see** - Share console errors if any

The blank page is almost certainly one of these issues. The console logs will tell us exactly which one.

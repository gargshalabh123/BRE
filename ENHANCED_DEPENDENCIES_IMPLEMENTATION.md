# Enhanced Dependency Tracking Implementation

## Overview
Implemented detailed dependency tracking that shows not just what files are referenced, but **how** they are referenced, including call types, method signatures, and parameters.

## What Was Added

### Backend Changes

#### 1. New Analyzer: `backend/analyzers/dependency_analyzer.py`
A comprehensive dependency analyzer that extracts detailed information about program calls, method invocations, and imports across multiple languages.

**Features:**
- **COBOL Support:**
  - `CALL` statements - External program invocations with parameters
  - `PERFORM` statements - Internal paragraph/section calls
  - `COPY` statements - Copybook includes

- **Python Support:**
  - `import` statements - Module imports
  - `from ... import` - Specific imports with member lists
  - Function calls with parameters (filtered to exclude built-ins)

- **Java Support:**
  - `import` statements - Class/package imports
  - Method calls with object.method() signature

- **JavaScript/TypeScript Support:**
  - ES6 `import from` statements
  - CommonJS `require()` calls
  - Function calls with parameters

**Dependency Object Structure:**
```python
{
    'target': str,        # Program/function/module being called
    'type': str,          # Type of dependency (see types below)
    'line': int,          # Line number where dependency occurs
    'signature': str,     # Full call statement/signature
    'parameters': list,   # List of parameter names/values
    'description': str    # Human-readable description
}
```

**Dependency Types:**
- `PROGRAM_CALL` - External COBOL program call (CALL statement)
- `PERFORM_PARAGRAPH` - Internal COBOL paragraph/section call
- `COPYBOOK` - COBOL copybook include
- `IMPORT` - Module/class import (Python, Java, JS)
- `FROM_IMPORT` - Specific member import (Python)
- `REQUIRE` - CommonJS require (JS)
- `METHOD_CALL` - Method invocation (Java)
- `FUNCTION_CALL` - Function invocation (Python, JS)

#### 2. Updated: `backend/analyzers/code_analyzer.py`

**New Method:** `_analyze_detailed_dependencies()`
- Returns `Dict[str, List[Dict[str, Any]]]` mapping file paths to detailed dependency lists
- Uses the new DependencyAnalyzer for enhanced tracking
- Maintains backward compatibility with simple dependencies

**Modified Method:** `analyze_all()`
- Now returns both `dependencies` (simple) and `detailed_dependencies` (enhanced)
- Simple dependencies kept for backward compatibility

### Frontend Changes

#### 3. Updated: `frontend/src/services/api.ts`

**Added Interface:**
```typescript
detailed_dependencies?: Record<string, Array<{
  target: string
  type: string
  line: number
  signature: string
  parameters: string[]
  description: string
}>>
```

#### 4. Updated: `frontend/src/pages/FileDetailPage.tsx`

**Enhanced Dependency Display:**

The Dependencies tab now shows a rich table with:
- **Type Column:** Color-coded badges for each dependency type
  - Blue: PROGRAM_CALL
  - Purple: PERFORM_PARAGRAPH
  - Green: COPYBOOK
  - Orange: IMPORT
  - Pink: METHOD_CALL
  - Teal: FUNCTION_CALL

- **Target Column:** The program/function/module being called (monospace font, bold)

- **Line Column:** Line number where the dependency occurs

- **Signature Column:** Full call statement or method signature
  - Truncated with ellipsis for long signatures
  - Monospace font for code readability

- **Parameters Column:** Individual parameter badges
  - Gray rounded badges for each parameter
  - Flex wrap for multiple parameters
  - Shows "-" if no parameters

- **Description Column:** Human-readable explanation of the dependency

**Backward Compatibility:**
- Falls back to simple dependencies if detailed_dependencies not available
- Converts simple string dependencies to detailed format for consistent display

## Example Output

### COBOL Program with Dependencies

For a COBOL file containing:
```cobol
CALL 'CBACT01P' USING WS-ACCOUNT-ID WS-STATUS-CODE
PERFORM 2000-VALIDATE-INPUT
COPY CUSTOMER-RECORD
```

The Dependencies tab will show:

| Type | Target | Line | Signature | Parameters | Description |
|------|--------|------|-----------|------------|-------------|
| <span style="background:#e3f2fd; color:#1565c0">PROGRAM CALL</span> | CBACT01P | 145 | CALL 'CBACT01P' USING WS-ACCOUNT-ID WS-STATUS-CODE | WS-ACCOUNT-ID, WS-STATUS-CODE | Calls external program CBACT01P |
| <span style="background:#f3e5f5; color:#6a1b9a">PERFORM PARAGRAPH</span> | 2000-VALIDATE-INPUT | 156 | PERFORM 2000-VALIDATE-INPUT | - | Performs paragraph 2000-VALIDATE-INPUT |
| <span style="background:#e8f5e9; color:#2e7d32">COPYBOOK</span> | CUSTOMER-RECORD | 23 | COPY CUSTOMER-RECORD | - | Includes copybook CUSTOMER-RECORD |

### Python File with Dependencies

For a Python file containing:
```python
import os
from datetime import datetime, timedelta
result = calculate_total(amount, tax_rate)
```

The Dependencies tab will show:

| Type | Target | Line | Signature | Parameters | Description |
|------|--------|------|-----------|------------|-------------|
| <span style="background:#fff3e0; color:#e65100">IMPORT</span> | os | 1 | import os | - | Imports module os |
| <span style="background:#fff3e0; color:#e65100">FROM IMPORT</span> | datetime | 2 | from datetime import datetime, timedelta | datetime, timedelta | Imports datetime, timedelta from datetime |
| <span style="background:#e0f2f1; color:#00695c">FUNCTION CALL</span> | calculate_total | 15 | calculate_total(amount, tax_rate) | amount, tax_rate | Calls function calculate_total |

## Benefits

### 1. **Better Code Understanding**
- Immediately see **what kind** of dependency it is (program call vs. import vs. internal call)
- See **where** each dependency occurs (line numbers)
- Understand **how** it's called (full signature)

### 2. **Improved Modernization Planning**
- Identify external program calls that need API conversion
- Find internal paragraphs that could be refactored into functions
- See parameter passing patterns for migration planning

### 3. **Enhanced Documentation**
- Each dependency has a human-readable description
- Color coding makes it easy to scan for specific dependency types
- Parameter lists help understand data flow

### 4. **Better Debugging**
- Line numbers let you jump directly to the dependency
- Signatures show the exact syntax used
- Parameter lists reveal what data is being passed

## Color Coding Legend

- **Blue (PROGRAM_CALL):** External program invocations requiring cross-program analysis
- **Purple (PERFORM_PARAGRAPH):** Internal control flow, good candidates for refactoring
- **Green (COPYBOOK):** Shared data structures and definitions
- **Orange (IMPORT):** Module dependencies for modern languages
- **Pink (METHOD_CALL):** Object-oriented method invocations
- **Teal (FUNCTION_CALL):** Procedural function calls

## Future Enhancements (Optional)

1. **Reverse Dependencies:** Show which files call/depend on the current file
2. **Dependency Graph Visualization:** Interactive graph showing call chains
3. **Cross-Reference Links:** Click on a program call to navigate to that program's detail page
4. **Filter by Dependency Type:** Toggle visibility of different dependency types
5. **Export Dependency Matrix:** Generate reports of all dependencies
6. **Unused Dependency Detection:** Identify imports/copies that aren't actually used
7. **Circular Dependency Detection:** Flag potential circular dependencies
8. **Parameter Type Analysis:** For typed languages, show parameter types in addition to names

## Testing

To test the enhanced dependency tracking:

1. **Upload a COBOL codebase** with various dependency types
2. Navigate to the **Metrics tab** and click on a COBOL file
3. Go to the **Dependencies tab** in the file detail page
4. Verify that:
   - CALL statements show as PROGRAM_CALL with parameters
   - PERFORM statements show as PERFORM_PARAGRAPH
   - COPY statements show as COPYBOOK
   - Each dependency has correct line numbers
   - Signatures are displayed correctly
   - Parameters are shown in badges

5. **Upload a Python/Java codebase** and verify:
   - Import statements are tracked
   - Function/method calls are detected
   - Parameters are extracted

## Files Modified/Created

### Backend:
- ✅ **NEW:** `backend/analyzers/dependency_analyzer.py` - Core dependency extraction logic
- ✅ **MODIFIED:** `backend/analyzers/code_analyzer.py` - Integration of detailed dependencies
- ✅ **AUTO:** `backend/api/analysis.py` - Returns detailed_dependencies in response (no changes needed)

### Frontend:
- ✅ **MODIFIED:** `frontend/src/services/api.ts` - Added detailed_dependencies interface
- ✅ **MODIFIED:** `frontend/src/pages/FileDetailPage.tsx` - Enhanced dependency table display

## Summary

Successfully implemented comprehensive dependency tracking that provides:
- **Type identification** - Know exactly what kind of dependency each entry is
- **Call signatures** - See the full syntax of how dependencies are invoked
- **Parameter tracking** - Understand what data is being passed between components
- **Rich visualization** - Color-coded badges and organized table layout
- **Language support** - Works across COBOL, Python, Java, JavaScript/TypeScript
- **Backward compatibility** - Falls back to simple dependencies if detailed not available

This enhancement transforms the Dependencies tab from a simple list of names into a comprehensive analysis tool that helps developers understand code structure, plan modernization efforts, and debug issues more effectively.

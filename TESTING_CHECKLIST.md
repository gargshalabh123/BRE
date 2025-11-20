# Dependency Analysis Testing Checklist

## Quick Start

1. **Start Backend:** `cd backend && python main.py`
2. **Start Frontend:** `cd frontend && npm run dev`
3. **Upload COBOL codebase** with various dependency types
4. **Navigate through** tabs to verify functionality

---

## Feature 1: Mainframe Dependencies

### CICS Program Control

**Upload a COBOL file containing:**
```cobol
EXEC CICS XCTL PROGRAM('ACCT02') END-EXEC
EXEC CICS LINK PROGRAM('VALPGM') END-EXEC
```

**Expected Results:**
- [ ] `CICS_XCTL` dependency appears with target `ACCT02`
- [ ] Badge color: Pink background (#fce4ec), dark pink text (#c2185b)
- [ ] `CICS_LINK` dependency appears with target `VALPGM`
- [ ] Badge color: Purple background (#f3e5f5), dark purple text (#7b1fa2)
- [ ] Both visible in Dependencies tab
- [ ] Both visible in file detail Dependencies tab

### CICS Transaction Management

**Upload a COBOL file containing:**
```cobol
EXEC CICS RETURN TRANSID('ACT2') END-EXEC
EXEC CICS START TRANSID('BTCH') END-EXEC
```

**Expected Results:**
- [ ] `CICS_RETURN_TRANSID` dependency with target `ACT2`
- [ ] Badge color: Yellow background (#fff9c4), dark orange text (#f57f17)
- [ ] `CICS_START_TRANSID` dependency with target `BTCH`
- [ ] Badge color: Orange background (#fff3e0), dark orange text (#e65100)

### BMS Screen Dependencies

**Upload a COBOL file containing:**
```cobol
EXEC CICS SEND MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC
EXEC CICS RECEIVE MAP('ACCMAP') MAPSET('ACCTSET') END-EXEC
```

**Expected Results:**
- [ ] `BMS_SEND_MAP` dependency with target `ACCTSET/ACCMAP`
- [ ] Parameters column shows: `ACCTSET`, `ACCMAP` badges
- [ ] Badge color: Teal background (#e0f2f1), dark teal text (#00695c)
- [ ] `BMS_RECEIVE_MAP` dependency with target `ACCTSET/ACCMAP`
- [ ] Badge color: Green background (#e8f5e9), dark green text (#2e7d32)

### IMS Dependencies

**Upload a COBOL file containing:**
```cobol
CALL 'CBLTDLI' USING GU-FUNCTION PCB-MASK SEGMENT-IO.
ENTRY 'CUSTPSB'
```

**Expected Results:**
- [ ] `IMS_DLI_CALL` dependency with target `GU-FUNCTION`
- [ ] Badge color: Deep purple background (#ede7f6), deep purple text (#4527a0)
- [ ] `IMS_PSB` dependency with target `CUSTPSB` (from ENTRY statement)
- [ ] Badge color: Purple background (#f3e5f5), dark purple text (#6a1b9a)

**Upload a COBOL file containing 88-level conditions:**
```cobol
01 IMS-RETURN-CODE PIC XX.
   88 STATUS-OK VALUE '  '.
   88 COULD-NOT-SCHEDULE-PSB VALUE 'TE'.
   88 END-OF-DB VALUE 'GB'.
```

**Expected Results:**
- [ ] **NO** `IMS_PSB` dependencies for any 88-level conditions
- [ ] **NO** false positives from VALUE clauses
- [ ] **NO** dependencies from error messages containing "PSB"

### NEW: CONDITION_88 - 88-Level Conditions

**Upload a COBOL file containing:**
```cobol
01 IMS-RETURN-CODE PIC XX.
   88 STATUS-OK VALUE '  '.
   88 COULD-NOT-SCHEDULE-PSB VALUE 'TE'.
   88 END-OF-DB VALUE 'GB'.
   88 RETRY-CONDITION VALUE 'AJ'.
```

**Expected Results:**
- [ ] `CONDITION_88: STATUS-OK` dependency appears (line with 88-level)
- [ ] Badge color: Yellow-amber background (#fff8e1), dark orange text (#f57f00)
- [ ] Parameters column shows: `['  ']` (the VALUE)
- [ ] `CONDITION_88: COULD-NOT-SCHEDULE-PSB` dependency appears
- [ ] Parameters column shows: `['TE']`
- [ ] All 4 conditions captured separately
- [ ] **NO** false positive IMS_PSB entries

### NEW: MESSAGE_USAGE - Error Messages

**Upload a COBOL file containing:**
```cobol
DISPLAY 'System error while scheduling PSB: Code:' IMS-RETURN-CODE.
DISPLAY 'INVALID CUSTOMER ID: ' WS-CUST-ID.
DISPLAY 'FAILED TO UPDATE RECORD' SQLCODE.
```

**Expected Results:**
- [ ] `MESSAGE_USAGE` dependency for "System error..." message
- [ ] Badge color: Light red background (#ffebee), dark red text (#c62828)
- [ ] Parameters column shows: `['IMS-RETURN-CODE']` (variables referenced)
- [ ] `MESSAGE_USAGE` dependency for "INVALID CUSTOMER ID: " message
- [ ] Parameters column shows: `['WS-CUST-ID']`
- [ ] `MESSAGE_USAGE` dependency for "FAILED TO UPDATE RECORD" message
- [ ] Parameters column shows: `['SQLCODE']`
- [ ] Only ERROR/FAIL/INVALID messages captured (not all DISPLAY statements)

### NEW: IMS_DLI_OP - IMS Database Operations

**Upload a COBOL file containing:**
```cobol
EXEC DLI GU PCB(AUTH-PCB) SEGMENT(AUTH-REC)
     INTO(AUTH-RECORD-LAYOUT) WHERE('12345678') END-EXEC.

EXEC DLI GNP PCB(AUTH-PCB) SEGMENT(AUTH-REC)
     INTO(AUTH-RECORD-LAYOUT) END-EXEC.

EXEC DLI REPL PCB(AUTH-PCB) SEGMENT(AUTH-REC)
     FROM(AUTH-RECORD-LAYOUT) END-EXEC.

EXEC DLI SCHD PSB(CUSTPSB) END-EXEC.
```

**Expected Results:**
- [ ] `IMS_DLI_OP: GU` dependency appears
- [ ] Badge color: Light purple background (#f3e5f5), deep purple text (#4a148c)
- [ ] Parameters: `['GU', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT', 'WHERE:12345678']`
- [ ] `IMS_DLI_OP: GNP` dependency appears
- [ ] Parameters: `['GNP', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT']`
- [ ] `IMS_DLI_OP: REPL` dependency appears
- [ ] Parameters include `FROM:AUTH-RECORD-LAYOUT`
- [ ] `IMS_DLI_OP: SCHD` dependency appears
- [ ] Parameters: `['SCHD', 'PSB:CUSTPSB']`

### NEW: CICS_OP - CICS Transaction Operations

**Upload a COBOL file containing:**
```cobol
EXEC CICS SYNCPOINT END-EXEC.

EXEC CICS SYNCPOINT ROLLBACK END-EXEC.

EXEC CICS LINK PROGRAM('WS-PGM-AUTH-FRAUD')
     COMMAREA(WS-FRAUD-CHECK-AREA) END-EXEC.
```

**Expected Results:**
- [ ] `CICS_OP: SYNCPOINT` dependency appears
- [ ] Badge color: Light cyan background (#e1f5fe), dark blue text (#01579b)
- [ ] Parameters: `['SYNCPOINT']`
- [ ] `CICS_OP: SYNCPOINT ROLLBACK` dependency appears
- [ ] Parameters: `['SYNCPOINT ROLLBACK']`
- [ ] `CICS_OP: WS-PGM-AUTH-FRAUD` dependency appears
- [ ] Parameters: `['LINK', 'WS-PGM-AUTH-FRAUD', 'COMMAREA:WS-FRAUD-CHECK-AREA']`
- [ ] COMMAREA information captured in parameters

---

## Feature 2: Comment Filtering

### Test False Positive Prevention

**Upload a COBOL file containing:**
```cobol
      * Licensed under the Apache License, Version 2.0 (the "License");
      * you may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *     http://www.apache.org/licenses/LICENSE-2.0
      *
      * This program performs validation of customer records
```

**Expected Results:**
- [ ] **NO** `COPY` dependency for "copy of the License"
- [ ] **NO** `PERFORM` dependency for "performs validation"
- [ ] Comment lines completely ignored
- [ ] Zero false positives in dependency list

### Test Actual Dependencies Are Still Captured

**Upload a COBOL file containing:**
```cobol
      * This is a comment
       COPY CUSTOMER-RECORD.
      * Another comment
       PERFORM 2000-VALIDATE-INPUT.
```

**Expected Results:**
- [ ] `COPYBOOK` dependency with target `CUSTOMER-RECORD` (line after comment)
- [ ] `PERFORM_PARAGRAPH` dependency with target `2000-VALIDATE-INPUT` (line after comment)
- [ ] Comments are ignored but actual code is captured

---

## Feature 3: Dependency Grouping

### Main Dependencies Tab Grouping

**Upload a COBOL file with repeated PERFORM calls:**
```cobol
       PERFORM 2000-VALIDATE-INPUT.        *> Line 125
       ...
       PERFORM 2000-VALIDATE-INPUT.        *> Line 148
       ...
       PERFORM 2000-VALIDATE-INPUT.        *> Line 201
       ...
       PERFORM 2000-VALIDATE-INPUT.        *> Line 245
       ...
       PERFORM 2000-VALIDATE-INPUT.        *> Line 289
```

**Navigate to Main Dependencies Tab:**

**Expected Results:**
- [ ] **Only 1 row** for `2000-VALIDATE-INPUT` (not 5 rows!)
- [ ] Count badge shows: `5`
- [ ] Count badge is **yellow** (#fff3cd background) because count > 1
- [ ] Lines column shows: `125, 148, 201, 245, 289`
- [ ] All line numbers are comma-separated
- [ ] Signature shows first occurrence's statement
- [ ] Description is correct

### File Detail Page Grouping

**Click on the COBOL file from Metrics tab:**

**Navigate to file's Dependencies tab:**

**Expected Results:**
- [ ] Tab shows unique count: "Dependencies (1)" not "Dependencies (5)"
- [ ] Table header shows: "Dependencies (1 unique, 5 total)"
- [ ] **Only 1 row** for `2000-VALIDATE-INPUT`
- [ ] Count badge shows: `5` (yellow)
- [ ] Lines column shows: `125, 148, 201, 245, 289`

### Smart Line Truncation

**Upload a file with a paragraph performed 10+ times:**

**Expected Results:**
- [ ] If ≤5 occurrences: Show all (e.g., `125, 148, 201, 245, 289`)
- [ ] If >5 occurrences: Show first 3, ..., last 2
  - Example: `125, 148, 201, ... 445, 489` for 10 occurrences
- [ ] Truncation is clean and readable
- [ ] All line numbers are sorted ascending

### Count Badge Color

**Test with different counts:**

**Expected Results:**
- [ ] Count = 1: Gray badge (#e9ecef background, #495057 text)
- [ ] Count > 1: Yellow badge (#fff3cd background, #856404 text)
- [ ] Badge is pill-shaped (borderRadius: 12px)
- [ ] Bold font weight

---

## Feature 4: Centralized Styling

### Color Consistency

**Navigate to:**
1. Main Dependencies tab
2. Click on a file → File detail Dependencies tab
3. Check Dependency Types Breakdown section

**Expected Results:**
- [ ] All three locations use same colors for same type
- [ ] `PROGRAM_CALL` always blue (#e3f2fd / #1565c0)
- [ ] `PERFORM_PARAGRAPH` always purple (#f3e5f5 / #6a1b9a)
- [ ] `COPYBOOK` always green (#e8f5e9 / #2e7d32)
- [ ] New types (CICS, BMS, IMS) have consistent colors everywhere

### Label Formatting

**Check all dependency type labels:**

**Expected Results:**
- [ ] All underscores replaced with spaces
- [ ] `PROGRAM_CALL` displays as "PROGRAM CALL"
- [ ] `CICS_XCTL` displays as "CICS XCTL"
- [ ] `BMS_SEND_MAP` displays as "BMS SEND MAP"
- [ ] Consistent formatting across all views

---

## Feature 5: UI/UX Testing

### Main Dependencies Tab

**Navigate to main Dependencies tab:**

**Expected Results:**
- [ ] Summary stats show correct counts
- [ ] "Files with Dependencies" count is accurate
- [ ] "Total Dependencies" count is accurate
- [ ] "Unique Targets" count is accurate
- [ ] Type breakdown pills show correct counts
- [ ] Type breakdown pills have correct colors
- [ ] "Most Referenced Targets" table shows top 20
- [ ] Filter dropdown includes all types + "ALL"
- [ ] Filtering works correctly
- [ ] File links navigate to correct file detail page
- [ ] Table is scrollable horizontally if needed

### File Detail Page

**Click on a file from Metrics tab:**

**Expected Results:**
- [ ] File name displayed correctly in header
- [ ] "Back to Analysis" button works
- [ ] Overview tab shows file metrics
- [ ] Source Code tab shows syntax-highlighted code
- [ ] Dependencies tab shows grouped dependencies
- [ ] Tab count reflects unique dependencies
- [ ] Table header shows "X unique, Y total"
- [ ] DB Operations tab shows database queries (if any)
- [ ] Business Rules tab shows extracted rules (if any)

### Table Responsiveness

**Test on different window sizes:**

**Expected Results:**
- [ ] Tables scroll horizontally when needed
- [ ] No layout breaking
- [ ] All columns visible with scroll
- [ ] Count badges don't wrap
- [ ] Line numbers wrap properly
- [ ] Signature truncates with ellipsis if too long

---

## Performance Testing

### Large Codebase

**Upload a large COBOL codebase (100+ files, 10,000+ LOC):**

**Expected Results:**
- [ ] Analysis completes in reasonable time (< 2 minutes)
- [ ] Dependencies tab loads without lag
- [ ] Grouping happens instantly
- [ ] No browser freezing
- [ ] Table scrolling is smooth
- [ ] Filtering is responsive

### Many Duplicate Dependencies

**Upload a file with 50+ calls to same paragraph:**

**Expected Results:**
- [ ] Lines show: `125, 148, 201, ... 945, 989`
- [ ] Smart truncation works correctly
- [ ] No performance issues
- [ ] Count badge shows correct number
- [ ] Table renders quickly

---

## Edge Cases

### Empty Dependencies

**Upload a file with no dependencies:**

**Expected Results:**
- [ ] Dependencies tab shows: "No dependencies found"
- [ ] No errors in console
- [ ] Tab count shows: "Dependencies (0)"
- [ ] No table rendering issues

### Single Occurrence

**Upload a file with each dependency called only once:**

**Expected Results:**
- [ ] Count badges are gray (not yellow)
- [ ] Count shows: `1`
- [ ] Lines show single line number
- [ ] No "..." truncation
- [ ] Grouping still works (just 1 item per group)

### Mixed Dependencies

**Upload a file with:**
- Some dependencies called once
- Some dependencies called multiple times
- Different types of dependencies

**Expected Results:**
- [ ] Mix of gray and yellow badges
- [ ] Correct line numbers for each
- [ ] Grouped by type+target correctly
- [ ] All types displayed with correct colors

---

## Browser Compatibility

**Test in:**
- [ ] Chrome (latest)
- [ ] Firefox (latest)
- [ ] Edge (latest)
- [ ] Safari (if available)

**Expected Results:**
- [ ] All features work in all browsers
- [ ] Colors render correctly
- [ ] Badges look consistent
- [ ] No console errors
- [ ] Smooth scrolling

---

## Console Checks

**Open browser developer console:**

**Expected Results:**
- [ ] No errors (red messages)
- [ ] No warnings about React keys
- [ ] No "Failed to fetch" errors
- [ ] API calls return 200 OK
- [ ] detailed_dependencies field present in response

---

## Regression Testing

### Verify Existing Features Still Work

**After implementing new features:**

**Expected Results:**
- [ ] Metrics tab still works
- [ ] Complexity tab still works
- [ ] Database Operations tab still works
- [ ] Business Rules tab still works
- [ ] AI Insights tab still works
- [ ] ZIP upload still works
- [ ] Export JSON still works
- [ ] File navigation still works

---

## Summary Checklist

### Backend
- [ ] All 8 new dependency types captured
- [ ] Comment lines filtered correctly
- [ ] No false positives
- [ ] detailed_dependencies in API response
- [ ] Backward compatible (dependencies still present)

### Frontend - Main Dependencies Tab
- [ ] Grouping works correctly
- [ ] Count badges show correct numbers
- [ ] Yellow for >1, gray for =1
- [ ] Line numbers display correctly
- [ ] Smart truncation for >5 lines
- [ ] All new types displayed
- [ ] Colors from utility functions
- [ ] Filter works with grouped data
- [ ] File links navigate correctly

### Frontend - File Detail Page
- [ ] Grouping works within file
- [ ] Tab count shows unique
- [ ] Header shows "X unique, Y total"
- [ ] Count badges correct
- [ ] Line numbers correct
- [ ] All new types displayed
- [ ] Colors from utility functions
- [ ] Navigation works

### Code Quality
- [ ] No console errors
- [ ] No React warnings
- [ ] DRY principle (no duplicated color code)
- [ ] Consistent styling everywhere
- [ ] TypeScript types correct
- [ ] Code is readable and maintainable

---

## Sign-Off

**Tested by:** _________________

**Date:** _________________

**Browser:** _________________

**Result:** ☐ PASS  ☐ FAIL

**Notes:**
_______________________________________________
_______________________________________________
_______________________________________________

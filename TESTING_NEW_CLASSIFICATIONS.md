# Testing Guide - New Dependency Classifications

## Quick Start

1. **Restart Backend:**
   ```bash
   cd backend
   python main.py
   ```

2. **Start Frontend:**
   ```bash
   cd frontend
   npm run dev
   ```

3. **Upload Test COBOL File**

4. **Verify New Dependency Types**

---

## Test Case 1: CONDITION_88 (88-Level Conditions)

### Create Test File: `TEST_CONDITIONS.cbl`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CONDITIONS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 IMS-RETURN-CODE PIC XX.
          88 STATUS-OK VALUE '  '.
          88 COULD-NOT-SCHEDULE-PSB VALUE 'TE'.
          88 END-OF-DB VALUE 'GB'.
          88 RETRY-CONDITION VALUE 'AJ'.

       01 TRANSACTION-STATUS PIC X.
          88 TRANS-APPROVED VALUE 'A'.
          88 TRANS-DECLINED VALUE 'D'.
          88 TRANS-PENDING VALUE 'P'.
```

### Expected Results

**Dependencies Tab:**
- ✅ `CONDITION_88: STATUS-OK` (Line 9)
  - Parameters: `['  ']`
  - Badge: Yellow-amber background

- ✅ `CONDITION_88: COULD-NOT-SCHEDULE-PSB` (Line 10)
  - Parameters: `['TE']`
  - Badge: Yellow-amber background

- ✅ `CONDITION_88: END-OF-DB` (Line 11)
  - Parameters: `['GB']`

- ✅ `CONDITION_88: RETRY-CONDITION` (Line 12)
  - Parameters: `['AJ']`

- ✅ `CONDITION_88: TRANS-APPROVED` (Line 15)
  - Parameters: `['A']`

- ✅ `CONDITION_88: TRANS-DECLINED` (Line 16)
  - Parameters: `['D']`

- ✅ `CONDITION_88: TRANS-PENDING` (Line 17)
  - Parameters: `['P']`

**Verify:**
- [ ] All 7 conditions appear
- [ ] No false positive IMS_PSB entries
- [ ] Badge color is yellow-amber (#fff8e1 bg, #f57f00 text)
- [ ] VALUE clauses shown in Parameters column

---

## Test Case 2: MESSAGE_USAGE (Error Messages)

### Create Test File: `TEST_MESSAGES.cbl`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-MESSAGES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'System error while scheduling PSB: Code:'
               IMS-RETURN-CODE.

           DISPLAY 'INVALID CUSTOMER ID: ' WS-CUST-ID.

           DISPLAY 'FAILED TO UPDATE RECORD' SQLCODE.

           DISPLAY 'ERROR: Database connection timeout'
               WS-ERROR-CODE WS-TIMESTAMP.

           STOP RUN.
```

### Expected Results

**Dependencies Tab:**
- ✅ `MESSAGE_USAGE: "System error while scheduling PSB: Code:"` (Line 6)
  - Parameters: `['IMS-RETURN-CODE']`
  - Badge: Light red background

- ✅ `MESSAGE_USAGE: "INVALID CUSTOMER ID: "` (Line 9)
  - Parameters: `['WS-CUST-ID']`

- ✅ `MESSAGE_USAGE: "FAILED TO UPDATE RECORD"` (Line 11)
  - Parameters: `['SQLCODE']`

- ✅ `MESSAGE_USAGE: "ERROR: Database connection timeout"` (Line 13)
  - Parameters: `['WS-ERROR-CODE', 'WS-TIMESTAMP']`

**Verify:**
- [ ] All 4 error messages captured
- [ ] Variables extracted correctly
- [ ] Badge color is light red (#ffebee bg, #c62828 text)
- [ ] Only ERROR/FAIL/INVALID messages captured

---

## Test Case 3: IMS_DLI_OP (IMS Database Operations)

### Create Test File: `TEST_IMS_OPS.cbl`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-IMS-OPS.

       PROCEDURE DIVISION.
       READ-AUTH-RECORD.
           EXEC DLI GU PCB(AUTH-PCB) SEGMENT(AUTH-REC)
                INTO(AUTH-RECORD-LAYOUT)
                WHERE('12345678') END-EXEC.

       READ-NEXT-AUTH-RECORD.
           EXEC DLI GNP PCB(AUTH-PCB) SEGMENT(AUTH-REC)
                INTO(AUTH-RECORD-LAYOUT) END-EXEC.

       UPDATE-AUTH-DETAILS.
           EXEC DLI REPL PCB(AUTH-PCB) SEGMENT(AUTH-REC)
                FROM(AUTH-RECORD-LAYOUT) END-EXEC.

       SCHEDULE-PSB.
           EXEC DLI SCHD PSB(CUSTPSB) END-EXEC.

       TERMINATE-PSB.
           EXEC DLI TERM END-EXEC.
```

### Expected Results

**Dependencies Tab:**
- ✅ `IMS_DLI_OP: GU` (Line 6)
  - Parameters: `['GU', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT', 'WHERE:12345678']`
  - Badge: Light purple background

- ✅ `IMS_DLI_OP: GNP` (Line 11)
  - Parameters: `['GNP', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT']`

- ✅ `IMS_DLI_OP: REPL` (Line 15)
  - Parameters: `['REPL', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT']`

- ✅ `IMS_DLI_OP: SCHD` (Line 19)
  - Parameters: `['SCHD', 'PSB:CUSTPSB']`

- ✅ `IMS_DLI_OP: TERM` (Line 22)
  - Parameters: `['TERM']`

**Verify:**
- [ ] All 5 operations captured
- [ ] PCB names extracted
- [ ] Segment names extracted
- [ ] Layouts (INTO/FROM) extracted
- [ ] WHERE clause extracted
- [ ] PSB extracted for SCHD
- [ ] Badge color is light purple (#f3e5f5 bg, #4a148c text)

---

## Test Case 4: CICS_OP (CICS Operations)

### Create Test File: `TEST_CICS_OPS.cbl`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-CICS-OPS.

       PROCEDURE DIVISION.
       UPDATE-SUCCESS.
           EXEC CICS SYNCPOINT END-EXEC.

       UPDATE-FAILURE.
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.

       CALL-FRAUD-CHECK.
           EXEC CICS LINK PROGRAM('WS-PGM-AUTH-FRAUD')
                COMMAREA(WS-FRAUD-CHECK-AREA) END-EXEC.

       CALL-VALIDATION.
           EXEC CICS LINK PROGRAM('VALPGM')
                COMMAREA(WS-VALIDATION-AREA)
                LENGTH(100) END-EXEC.
```

### Expected Results

**Dependencies Tab:**
- ✅ `CICS_OP: SYNCPOINT` (Line 6)
  - Parameters: `['SYNCPOINT']`
  - Badge: Light cyan background

- ✅ `CICS_OP: SYNCPOINT ROLLBACK` (Line 9)
  - Parameters: `['SYNCPOINT ROLLBACK']`

- ✅ `CICS_OP: WS-PGM-AUTH-FRAUD` (Line 12)
  - Parameters: `['LINK', 'WS-PGM-AUTH-FRAUD', 'COMMAREA:WS-FRAUD-CHECK-AREA']`

- ✅ `CICS_OP: VALPGM` (Line 16)
  - Parameters: `['LINK', 'VALPGM', 'COMMAREA:WS-VALIDATION-AREA']`

**Verify:**
- [ ] All 4 operations captured
- [ ] SYNCPOINT vs SYNCPOINT ROLLBACK distinguished
- [ ] Program names extracted from LINK
- [ ] COMMAREA extracted
- [ ] Badge color is light cyan (#e1f5fe bg, #01579b text)

---

## Test Case 5: Comprehensive Integration Test

### Use: `COPAUS1C.cbl` (Your actual file)

**Before Classification:**
```
❌ IMS PSB: VALUE (False positive from 88-level)
❌ IMS PSB: COULD-NOT-SCHEDULE-PSB (False positive)
❌ IMS PSB: Code (False positive from error message)
```

**After Classification:**
```
✅ CONDITION_88: COULD-NOT-SCHEDULE-PSB (Proper category)
✅ MESSAGE_USAGE: "System error..." (Proper category)
✅ IMS_DLI_OP: GU (With full context)
✅ CICS_OP: LINK (With COMMAREA)
✅ IMS_PSB: CUSTPSB (Only legitimate ENTRY)
```

**Verify:**
- [ ] No more false positive IMS_PSB entries
- [ ] 88-level conditions in CONDITION_88 category
- [ ] Error messages in MESSAGE_USAGE category
- [ ] IMS operations with full parameters
- [ ] CICS operations with COMMAREA info

---

## Visual Verification Guide

### Badge Colors Quick Reference

| Type | Background | Text | Category |
|------|------------|------|----------|
| CONDITION_88 | #fff8e1 (yellow-amber) | #f57f00 (dark orange) | Data Definitions |
| MESSAGE_USAGE | #ffebee (light red) | #c62828 (dark red) | Error Handling |
| IMS_DLI_OP | #f3e5f5 (light purple) | #4a148c (deep purple) | IMS DL/I Operations |
| CICS_OP | #e1f5fe (light cyan) | #01579b (dark blue) | CICS Operations |

### Parameters Column Verification

**CONDITION_88:**
```
VALUE | Description
------|-----------
['  '] | Blank/space value
['TE'] | Two-character code
['A', 'P', 'D'] | Multiple values (if any)
```

**MESSAGE_USAGE:**
```
Variables Referenced
-------------------
['IMS-RETURN-CODE']
['WS-CUST-ID', 'SQLCODE']
['ERROR-MSG', 'TIMESTAMP']
```

**IMS_DLI_OP:**
```
Context Information
------------------
['GU', 'PCB:AUTH-PCB', 'SEG:AUTH-REC', 'LAYOUT:AUTH-RECORD-LAYOUT', 'WHERE:12345678']
['GNP', 'PCB:AUTH-PCB', 'SEG:AUTH-REC']
['SCHD', 'PSB:CUSTPSB']
```

**CICS_OP:**
```
Operation Details
----------------
['SYNCPOINT']
['SYNCPOINT ROLLBACK']
['LINK', 'PROGNAME', 'COMMAREA:WS-AREA']
```

---

## Common Issues & Solutions

### Issue 1: Still seeing false positive IMS_PSB entries
**Solution:** Ensure backend was restarted after code changes

### Issue 2: CONDITION_88 not appearing
**Check:** Is the 88-level properly formatted? Must start with 88 in column 8-11

### Issue 3: MESSAGE_USAGE capturing too many DISPLAY statements
**By Design:** Only captures ERROR/FAIL/INVALID messages

### Issue 4: IMS_DLI_OP parameters missing
**Check:** Ensure EXEC DLI format is correct with proper parentheses

### Issue 5: CICS_OP not showing COMMAREA
**Check:** COMMAREA must be in format `COMMAREA(WS-VAR-NAME)`

---

## Success Criteria

### All Tests Pass If:
- ✅ Zero false positive IMS_PSB entries
- ✅ All 88-level conditions in CONDITION_88
- ✅ Error messages tracked in MESSAGE_USAGE
- ✅ IMS operations with full context
- ✅ CICS operations with COMMAREA
- ✅ Correct badge colors for all types
- ✅ Parameters column populated correctly
- ✅ Grouping still works (count badges, line numbers)

---

## After Testing

1. **Document Results** in this file
2. **Report Issues** to development team
3. **Update TESTING_CHECKLIST.md** with new test cases
4. **Celebrate** successful implementation! 🎉

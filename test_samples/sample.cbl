      *****************************************************************
      * SAMPLE COBOL PROGRAM - CUSTOMER DISCOUNT CALCULATOR
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTDISC.
       AUTHOR. LEGACY SYSTEMS TEAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTFILE'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD.
       01  CUSTOMER-RECORD.
           05 CUST-ID              PIC 9(6).
           05 CUST-NAME            PIC X(30).
           05 CUST-BALANCE         PIC 9(7)V99.
           05 CUST-AGE             PIC 999.
           05 CUST-STATUS          PIC X.
              88 ACTIVE-CUSTOMER   VALUE 'A'.
              88 INACTIVE-CUSTOMER VALUE 'I'.

       WORKING-STORAGE SECTION.
       01  WS-DISCOUNT-RATE        PIC 99V99.
       01  WS-DISCOUNT-AMOUNT      PIC 9(7)V99.
       01  WS-FINAL-AMOUNT         PIC 9(7)V99.

       77  WS-MIN-BALANCE          PIC 9(7)V99 VALUE 1000.00.
       77  WS-SENIOR-AGE           PIC 999 VALUE 65.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           OPEN INPUT CUSTOMER-FILE.
           PERFORM READ-CUSTOMER-RECORD
           PERFORM PROCESS-CUSTOMER
           CLOSE CUSTOMER-FILE.
           STOP RUN.

       READ-CUSTOMER-RECORD.
           READ CUSTOMER-FILE
               AT END DISPLAY 'END OF FILE'
           END-READ.

       PROCESS-CUSTOMER.
           PERFORM CALCULATE-DISCOUNT.
           PERFORM APPLY-DISCOUNT.
           PERFORM VALIDATE-BALANCE.

       CALCULATE-DISCOUNT.
      * Business Rule: Senior citizens get 20% discount
           IF CUST-AGE >= WS-SENIOR-AGE
               MOVE 20 TO WS-DISCOUNT-RATE
           ELSE
      * Business Rule: Regular customers get 10% discount
               IF ACTIVE-CUSTOMER
                   MOVE 10 TO WS-DISCOUNT-RATE
               ELSE
                   MOVE 0 TO WS-DISCOUNT-RATE
               END-IF
           END-IF.

       APPLY-DISCOUNT.
           COMPUTE WS-DISCOUNT-AMOUNT =
               CUST-BALANCE * WS-DISCOUNT-RATE / 100.
           COMPUTE WS-FINAL-AMOUNT =
               CUST-BALANCE - WS-DISCOUNT-AMOUNT.

       VALIDATE-BALANCE.
      * Business Rule: Minimum balance must be maintained
           IF WS-FINAL-AMOUNT < WS-MIN-BALANCE
               DISPLAY 'WARNING: BALANCE BELOW MINIMUM'
               DISPLAY 'CUSTOMER: ' CUST-NAME
           END-IF.

      * Embedded SQL Example
           EXEC SQL
               UPDATE CUSTOMERS
               SET BALANCE = :WS-FINAL-AMOUNT
               WHERE CUST_ID = :CUST-ID
           END-EXEC.

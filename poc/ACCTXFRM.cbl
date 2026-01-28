       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTXFRM.
       AUTHOR. BANKING-SYSTEM.
      *****************************************************************
      * PROGRAM NAME: ACCTXFRM                                        *
      * DESCRIPTION:  DATA TRANSFORMATION FROM CUSTOMERACCOUNTS       *
      *               TO ACCOUNTTRANSACTIONS TABLE                    *
      * INPUT:        CUSTOMERACCOUNTS TABLE (VIA SQL CURSOR)         *
      * OUTPUT:       ACCOUNTTRANSACTIONS TABLE (VIA SQL INSERT)      *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * SQL COMMUNICATION AREA
       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

      * SOURCE RECORD VARIABLES (FROM CUSTOMERACCOUNTS)
       01  WS-SOURCE-RECORD.
           05 WS-ACCT-ID           PIC 9(10).
           05 WS-CUST-ID           PIC 9(10).
           05 WS-CUST-NAME         PIC X(100).
           05 WS-ACCT-NUM          PIC X(20).
           05 WS-ACCT-TYPE         PIC X(20).
           05 WS-BALANCE           PIC S9(13)V99 COMP-3.
           05 WS-BRANCH            PIC X(10).
           05 WS-KYC               PIC X(20).
           05 WS-RISK              PIC 9(3).
           05 WS-ACCT-STATUS       PIC X(20).
           05 WS-CREATED           PIC X(26).
           05 WS-UPDATED           PIC X(26).

      * TARGET RECORD VARIABLES (FOR ACCOUNTTRANSACTIONS)
       01  WS-TARGET-RECORD.
           05 TGT-TRANS-ID         PIC 9(15) VALUE 0.
           05 TGT-ACCT-ID          PIC 9(10).
           05 TGT-CUST-ID          PIC 9(10).
           05 TGT-ACCT-NUM         PIC X(20).
           05 TGT-DESC             PIC X(200).
           05 TGT-SUBTYPE          PIC X(30).
           05 TGT-AMOUNT           PIC S9(13)V99 COMP-3.
           05 TGT-RUNBAL           PIC S9(13)V99 COMP-3.
           05 TGT-BRANCH           PIC X(10).
           05 TGT-REFNUM           PIC X(50).
           05 TGT-TRANS-DATE       PIC X(26).
           05 TGT-PROC-STATUS      PIC X(20) VALUE 'PROCESSED'.

      * WORK VARIABLES
       01  WS-COUNTERS.
           05 WS-RECORDS-READ      PIC 9(9) COMP VALUE 0.
           05 WS-RECORDS-INSERTED  PIC 9(9) COMP VALUE 0.
           05 WS-RECORDS-FAILED    PIC 9(9) COMP VALUE 0.

       01  WS-CURRENT-TIMESTAMP    PIC X(26).
       01  WS-TEMP-FIELD           PIC X(100).
       01  WS-EOF-FLAG             PIC X VALUE 'N'.
           88 END-OF-CURSOR        VALUE 'Y'.

       PROCEDURE DIVISION.

       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-RECORDS
           PERFORM 3000-FINALIZATION
           STOP RUN.

       1000-INITIALIZATION.
           DISPLAY '================================================'
           DISPLAY 'ACCOUNT TRANSFORMATION PROGRAM STARTED'
           DISPLAY '================================================'

      * GET CURRENT TIMESTAMP
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP

      * DECLARE CURSOR FOR ACTIVE ACCOUNTS
           EXEC SQL
               DECLARE ACCTCUR CURSOR FOR
               SELECT AccountID, CustomerID, CustomerName,
                      AccountNumber, AccountType, Balance,
                      BranchCode, KYCStatus, RiskScore,
                      AccountStatus, CreatedDate, LastUpdated
               FROM CustomerAccounts
               WHERE AccountStatus = 'Active'
               ORDER BY AccountID
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR DECLARING CURSOR. SQLCODE: ' SQLCODE
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF

      * OPEN CURSOR
           EXEC SQL
               OPEN ACCTCUR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR OPENING CURSOR. SQLCODE: ' SQLCODE
               MOVE 8 TO RETURN-CODE
               STOP RUN
           END-IF

           DISPLAY 'CURSOR OPENED SUCCESSFULLY'.

       2000-PROCESS-RECORDS.
           PERFORM 2100-FETCH-RECORD
           PERFORM 2200-PROCESS-LOOP
               UNTIL END-OF-CURSOR.

       2100-FETCH-RECORD.
           EXEC SQL
               FETCH ACCTCUR INTO
                   :WS-ACCT-ID,
                   :WS-CUST-ID,
                   :WS-CUST-NAME,
                   :WS-ACCT-NUM,
                   :WS-ACCT-TYPE,
                   :WS-BALANCE,
                   :WS-BRANCH,
                   :WS-KYC,
                   :WS-RISK,
                   :WS-ACCT-STATUS,
                   :WS-CREATED,
                   :WS-UPDATED
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   ADD 1 TO WS-RECORDS-READ
               WHEN 100
                   SET END-OF-CURSOR TO TRUE
               WHEN OTHER
                   DISPLAY 'FETCH ERROR. SQLCODE: ' SQLCODE
                   SET END-OF-CURSOR TO TRUE
           END-EVALUATE.

       2200-PROCESS-LOOP.
           IF NOT END-OF-CURSOR
               PERFORM 2300-TRANSFORM-DATA
               PERFORM 2400-INSERT-RECORD
               PERFORM 2100-FETCH-RECORD
           END-IF.

       2300-TRANSFORM-DATA.
      * MOVE BASIC FIELDS
           MOVE WS-ACCT-ID TO TGT-ACCT-ID
           MOVE WS-CUST-ID TO TGT-CUST-ID
           MOVE WS-ACCT-NUM TO TGT-ACCT-NUM

      * CREATE DESCRIPTION WITH CUSTOMER NAME
           INITIALIZE TGT-DESC
           STRING 'Customer: ' DELIMITED BY SIZE
                  WS-CUST-NAME DELIMITED BY SPACE
                  ' - Account Inquiry' DELIMITED BY SIZE
                  INTO TGT-DESC
           END-STRING

      * DETERMINE SUBTYPE BASED ON ACCOUNT TYPE
           EVALUATE WS-ACCT-TYPE
               WHEN 'Checking'
                   MOVE 'CHK-Inquiry' TO TGT-SUBTYPE
               WHEN 'Savings'
                   MOVE 'SAV-Inquiry' TO TGT-SUBTYPE
               WHEN 'Investment'
                   MOVE 'INV-Inquiry' TO TGT-SUBTYPE
               WHEN 'Credit'
                   MOVE 'CRD-Inquiry' TO TGT-SUBTYPE
               WHEN OTHER
                   MOVE 'GEN-Inquiry' TO TGT-SUBTYPE
           END-EVALUATE

      * MOVE BALANCE TO AMOUNT AND RUNNING BALANCE
           MOVE WS-BALANCE TO TGT-AMOUNT
           MOVE WS-BALANCE TO TGT-RUNBAL

      * MOVE BRANCH CODE
           MOVE WS-BRANCH TO TGT-BRANCH

      * CREATE REFERENCE NUMBER WITH RISK SCORE
           INITIALIZE TGT-REFNUM
           STRING 'RSK-' DELIMITED BY SIZE
                  WS-RISK DELIMITED BY SIZE
                  '-' DELIMITED BY SIZE
                  WS-ACCT-NUM DELIMITED BY SPACE
                  INTO TGT-REFNUM
           END-STRING

      * SET TRANSACTION DATE TO CURRENT TIMESTAMP
           MOVE WS-CURRENT-TIMESTAMP TO TGT-TRANS-DATE

      * DISPLAY PROGRESS EVERY 1000 RECORDS
           IF FUNCTION MOD(WS-RECORDS-READ, 1000) = 0
               DISPLAY 'PROCESSED ' WS-RECORDS-READ ' RECORDS'
           END-IF.

       2400-INSERT-RECORD.
           EXEC SQL
               INSERT INTO AccountTransactions
               (AccountID, CustomerID, AccountNumber, Description,
                TransactionSubType, Amount, RunningBalance,
                BranchCode, ReferenceNumber, TransactionDate,
                ProcessingStatus)
               VALUES
               (:TGT-ACCT-ID, :TGT-CUST-ID, :TGT-ACCT-NUM,
                :TGT-DESC, :TGT-SUBTYPE, :TGT-AMOUNT,
                :TGT-RUNBAL, :TGT-BRANCH, :TGT-REFNUM,
                :TGT-TRANS-DATE, :TGT-PROC-STATUS)
           END-EXEC

           IF SQLCODE = 0
               ADD 1 TO WS-RECORDS-INSERTED
           ELSE
               ADD 1 TO WS-RECORDS-FAILED
               DISPLAY 'INSERT FAILED FOR ACCOUNT: ' WS-ACCT-NUM
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'SQLERRM: ' SQLERRM
           END-IF.

       3000-FINALIZATION.
      * CLOSE CURSOR
           EXEC SQL
               CLOSE ACCTCUR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'WARNING: ERROR CLOSING CURSOR. SQLCODE: '
                       SQLCODE
           END-IF

      * COMMIT CHANGES
           EXEC SQL
               COMMIT WORK
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR COMMITTING TRANSACTION. SQLCODE: '
                       SQLCODE
               MOVE 8 TO RETURN-CODE
           ELSE
               DISPLAY '================================================'
               DISPLAY 'TRANSFORMATION COMPLETED SUCCESSFULLY'
               DISPLAY 'RECORDS READ:     ' WS-RECORDS-READ
               DISPLAY 'RECORDS INSERTED: ' WS-RECORDS-INSERTED
               DISPLAY 'RECORDS FAILED:   ' WS-RECORDS-FAILED
               DISPLAY '================================================'
           END-IF.

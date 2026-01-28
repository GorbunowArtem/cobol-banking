       IDENTIFICATION DIVISION.
       PROGRAM-ID. POST-LEDGER.
       AUTHOR. COBOL Data Lineage Feature Team.
      ******************************************************************
      * POST_LEDGER - Double-Entry Ledger Posting Program
      ******************************************************************
      * Purpose: Create double-entry ledger entries from transactions
      * Input:   dbo.Transactions table (SQL Server)
      * Output:  dbo.LedgerEntries table (SQL Server)
      *          dbo.PostingAudit table (audit trail)
      * Lineage: transform_kind=posting
      ******************************************************************
      * LINEAGE: PROGRAM=POST_LEDGER
      * LINEAGE: SRC=sqlserver.dbo.Transactions(TX_ID,ACC_ID,AMOUNT,
      *               CURRENCY,TX_TS_UTC)
      * LINEAGE: TGT=sqlserver.dbo.LedgerEntries(ENTRY_ID,TX_ID,
      *               ACC_ID,DEBIT,CREDIT,CURRENCY,POSTED_TS_UTC)
      * LINEAGE: MAP=TX_ID -> TX_ID [COPY]
      * LINEAGE: MAP=ACC_ID -> ACC_ID [COPY]
      * LINEAGE: MAP=AMOUNT -> DEBIT|CREDIT [IF AMOUNT<0 THEN 
      *               DEBIT=ABS(AMOUNT), CREDIT=NULL ELSE 
      *               DEBIT=NULL, CREDIT=AMOUNT]
      * LINEAGE: MAP=CURRENCY -> CURRENCY [COPY]
      * LINEAGE: MAP=TX_TS_UTC -> POSTED_TS_UTC [CURRENT_TIMESTAMP]
      * LINEAGE: REF={git_commit_sha}
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Include shared copybooks
       COPY DB-CONFIG.
       COPY RECORD-DEFS.
       COPY LINEAGE-LOGGER.

      * Run identifier (timestamp-based)
       01  WS-CURRENT-RUN-ID.
           05  FILLER                  PIC X(12) VALUE 'POST_LEDGER_'.
           05  WS-RUN-ID-TS            PIC X(15).

      * Cursor processing
       01  WS-CURSOR-OPEN              PIC X VALUE 'N'.
           88  CURSOR-IS-OPEN          VALUE 'Y'.

      * Row counters
       01  WS-ROWS-INPUT               PIC 9(9) COMP-5 VALUE ZERO.
       01  WS-ROWS-OUTPUT              PIC 9(9) COMP-5 VALUE ZERO.
       01  WS-ERRORS                   PIC 9(9) COMP-5 VALUE ZERO.

      * EXEC SQL working variables
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-TX-ID                   PIC 9(18) COMP-5.
       01  SQL-ACC-ID                  PIC X(32).
       01  SQL-AMOUNT                  PIC S9(16)V99 COMP-3.
       01  SQL-CURRENCY                PIC X(3).
       01  SQL-TX-TS-UTC               PIC X(26).
       01  SQL-DEBIT                   PIC S9(16)V99 COMP-3.
       01  SQL-CREDIT                  PIC S9(16)V99 COMP-3.
       01  SQL-AUDIT-ROWS-IN           PIC 9(9) COMP-5.
       01  SQL-AUDIT-ROWS-OUT          PIC 9(9) COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

      * SQL communication area
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * Cursor declaration
       EXEC SQL
           DECLARE TX_CURSOR CURSOR FOR
           SELECT TX_ID, ACC_ID, AMOUNT, CURRENCY, TX_TS_UTC
           FROM dbo.Transactions
           ORDER BY TX_ID
       END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '============================================'.
           DISPLAY 'POST_LEDGER - Double-Entry Posting'.
           DISPLAY '============================================'.
           DISPLAY ' '.

           PERFORM INITIALIZE-PROGRAM.
           PERFORM CONNECT-DATABASE.
           PERFORM PROCESS-TRANSACTIONS.
           PERFORM LOG-LINEAGE-EVENT.
           PERFORM LOG-AUDIT-RECORD.
           PERFORM DISCONNECT-DATABASE.
           PERFORM DISPLAY-SUMMARY.

           DISPLAY ' '.
           DISPLAY '============================================'.
           DISPLAY 'POST_LEDGER completed successfully'.
           DISPLAY '============================================'.

           STOP RUN.

      ******************************************************************
      * INITIALIZE-PROGRAM: Set up working storage and run ID
      ******************************************************************
       INITIALIZE-PROGRAM SECTION.
           MOVE ZERO TO WS-ROWS-INPUT.
           MOVE ZERO TO WS-ROWS-OUTPUT.
           MOVE ZERO TO WS-ERRORS.

      *    Generate unique run ID from current timestamp
           ACCEPT WS-RUN-ID-TS FROM TIME.
           DISPLAY 'Run ID: ' WS-CURRENT-RUN-ID.

      *    Load git commit SHA (simplified - would read .version file)
           MOVE 'UNKNOWN' TO WS-COMMIT-SHA.
       INITIALIZE-PROGRAM-EXIT.
           EXIT.

      ******************************************************************
      * CONNECT-DATABASE: Establish ODBC connection to SQL Server
      ******************************************************************
       CONNECT-DATABASE SECTION.
           DISPLAY 'Connecting to SQL Server (DSN: '
                   WS-DSN-SQLSERVER ')...'.

           EXEC SQL
               CONNECT TO :WS-DSN-SQLSERVER
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Database connection failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'SQLSTATE: ' SQLSTATE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-SQLSERVER-CONNECTED.
           DISPLAY 'Database connection established'.
       CONNECT-DATABASE-EXIT.
           EXIT.

      ******************************************************************
      * PROCESS-TRANSACTIONS: Iterate over transactions and post
      ******************************************************************
       PROCESS-TRANSACTIONS SECTION.
           DISPLAY 'Opening cursor on dbo.Transactions...'.

      *    Open cursor
           EXEC SQL
               OPEN TX_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to open cursor'
               DISPLAY 'SQLCODE: ' SQLCODE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-CURSOR-OPEN.

      *    Fetch and process each row
           PERFORM FETCH-AND-POST-TRANSACTION
               UNTIL SQLCODE = 100 OR SQLCODE < 0.

      *    Close cursor
           EXEC SQL
               CLOSE TX_CURSOR
           END-EXEC.

           MOVE 'N' TO WS-CURSOR-OPEN.
           DISPLAY 'Transaction posting complete'.
       PROCESS-TRANSACTIONS-EXIT.
           EXIT.

      ******************************************************************
      * FETCH-AND-POST-TRANSACTION: Fetch one row and create ledger
      ******************************************************************
       FETCH-AND-POST-TRANSACTION SECTION.
      *    Fetch next transaction
           EXEC SQL
               FETCH TX_CURSOR INTO
                   :SQL-TX-ID,
                   :SQL-ACC-ID,
                   :SQL-AMOUNT,
                   :SQL-CURRENCY,
                   :SQL-TX-TS-UTC
           END-EXEC.

           IF SQLCODE = 100
               GO TO FETCH-AND-POST-TRANSACTION-EXIT
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Fetch failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
               GO TO FETCH-AND-POST-TRANSACTION-EXIT
           END-IF.

           ADD 1 TO WS-ROWS-INPUT.

      *    Apply double-entry logic
           PERFORM APPLY-DOUBLE-ENTRY-LOGIC.

      *    Insert ledger entry
           PERFORM INSERT-LEDGER-ENTRY.

       FETCH-AND-POST-TRANSACTION-EXIT.
           EXIT.

      ******************************************************************
      * APPLY-DOUBLE-ENTRY-LOGIC: Determine DEBIT vs CREDIT
      ******************************************************************
       APPLY-DOUBLE-ENTRY-LOGIC SECTION.
      *    Rule: If AMOUNT >= 0, CREDIT the account (money in)
      *          If AMOUNT < 0, DEBIT the account (money out)

           IF SQL-AMOUNT >= 0
      *        Positive amount = CREDIT entry
               MOVE 0 TO SQL-DEBIT
               MOVE SQL-AMOUNT TO SQL-CREDIT
           ELSE
      *        Negative amount = DEBIT entry (use absolute value)
               COMPUTE SQL-DEBIT = FUNCTION ABS(SQL-AMOUNT)
               MOVE 0 TO SQL-CREDIT
           END-IF.

       APPLY-DOUBLE-ENTRY-LOGIC-EXIT.
           EXIT.

      ******************************************************************
      * INSERT-LEDGER-ENTRY: Insert into dbo.LedgerEntries
      ******************************************************************
       INSERT-LEDGER-ENTRY SECTION.
      *    Insert ledger entry with NULL handling for DEBIT/CREDIT
           IF SQL-DEBIT = 0
               EXEC SQL
                   INSERT INTO dbo.LedgerEntries (
                       TX_ID,
                       ACC_ID,
                       DEBIT,
                       CREDIT,
                       CURRENCY,
                       POSTED_TS_UTC
                   ) VALUES (
                       :SQL-TX-ID,
                       :SQL-ACC-ID,
                       NULL,
                       :SQL-CREDIT,
                       :SQL-CURRENCY,
                       SYSUTCDATETIME()
                   )
               END-EXEC
           ELSE
               EXEC SQL
                   INSERT INTO dbo.LedgerEntries (
                       TX_ID,
                       ACC_ID,
                       DEBIT,
                       CREDIT,
                       CURRENCY,
                       POSTED_TS_UTC
                   ) VALUES (
                       :SQL-TX-ID,
                       :SQL-ACC-ID,
                       :SQL-DEBIT,
                       NULL,
                       :SQL-CURRENCY,
                       SYSUTCDATETIME()
                   )
               END-EXEC
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Ledger insert failed for TX_ID: '
                       SQL-TX-ID
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
           ELSE
               ADD 1 TO WS-ROWS-OUTPUT
           END-IF.

       INSERT-LEDGER-ENTRY-EXIT.
           EXIT.

      ******************************************************************
      * LOG-LINEAGE-EVENT: Record transformation metadata
      ******************************************************************
       LOG-LINEAGE-EVENT SECTION.
      *    Set lineage parameters
           MOVE 'POST_LEDGER' TO WS-LIN-PROGRAM.
           MOVE 'sqlserver' TO WS-LIN-SRC-ENGINE.
           MOVE 'dbo' TO WS-LIN-SRC-SCHEMA.
           MOVE 'Transactions' TO WS-LIN-SRC-TABLE.
           MOVE 'TX_ID,ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC'
               TO WS-LIN-SRC-COLS.
           MOVE 'sqlserver' TO WS-LIN-TGT-ENGINE.
           MOVE 'dbo' TO WS-LIN-TGT-SCHEMA.
           MOVE 'LedgerEntries' TO WS-LIN-TGT-TABLE.
           MOVE 'ENTRY_ID,TX_ID,ACC_ID,DEBIT,CREDIT,CURRENCY'
               TO WS-LIN-TGT-COLS.
           MOVE 'posting' TO WS-LIN-TRANSFORM-KIND.
           MOVE 'IF AMOUNT<0 THEN DEBIT=ABS(AMOUNT) ELSE CREDIT=AMOUNT'
               TO WS-LIN-TRANSFORM-EXPR.
           MOVE WS-CURRENT-RUN-ID TO WS-LIN-RUN-ID.

      *    Call lineage logger copybook section
           PERFORM ADD-LINEAGE-EVENT.

           DISPLAY 'Lineage event logged successfully'.
       LOG-LINEAGE-EVENT-EXIT.
           EXIT.

      ******************************************************************
      * LOG-AUDIT-RECORD: Insert PostingAudit record
      ******************************************************************
       LOG-AUDIT-RECORD SECTION.
           MOVE WS-ROWS-INPUT TO SQL-AUDIT-ROWS-IN.
           MOVE WS-ROWS-OUTPUT TO SQL-AUDIT-ROWS-OUT.

           EXEC SQL
               INSERT INTO dbo.PostingAudit (
                   PROGRAM,
                   RUN_ID,
                   ROWS_IN,
                   ROWS_OUT,
                   TS_UTC
               ) VALUES (
                   'POST_LEDGER',
                   :WS-CURRENT-RUN-ID,
                   :SQL-AUDIT-ROWS-IN,
                   :SQL-AUDIT-ROWS-OUT,
                   SYSUTCDATETIME()
               )
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'WARNING: Failed to insert audit record'
               DISPLAY 'SQLCODE: ' SQLCODE
           ELSE
               DISPLAY 'Audit record created successfully'
           END-IF.

       LOG-AUDIT-RECORD-EXIT.
           EXIT.

      ******************************************************************
      * DISCONNECT-DATABASE: Commit transaction and disconnect
      ******************************************************************
       DISCONNECT-DATABASE SECTION.
           IF NOT SQLSERVER-IS-CONNECTED
               GO TO DISCONNECT-DATABASE-EXIT
           END-IF.

      *    Commit all changes
           EXEC SQL
               COMMIT WORK
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'WARNING: Commit failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               EXEC SQL
                   ROLLBACK WORK
               END-EXEC
           ELSE
               DISPLAY 'Transaction committed successfully'
           END-IF.

      *    Disconnect from database
           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC.

           MOVE 'N' TO WS-SQLSERVER-CONNECTED.
           DISPLAY 'Database connection closed'.
       DISCONNECT-DATABASE-EXIT.
           EXIT.

      ******************************************************************
      * DISPLAY-SUMMARY: Show processing statistics
      ******************************************************************
       DISPLAY-SUMMARY SECTION.
           DISPLAY ' '.
           DISPLAY 'Processing Summary:'.
           DISPLAY '  Transactions processed: ' WS-ROWS-INPUT.
           DISPLAY '  Ledger entries created: ' WS-ROWS-OUTPUT.
           DISPLAY '  Errors encountered:     ' WS-ERRORS.
       DISPLAY-SUMMARY-EXIT.
           EXIT.

       END PROGRAM POST-LEDGER.

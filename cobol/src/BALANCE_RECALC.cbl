       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALANCE-RECALC.
       AUTHOR. COBOL Data Lineage Feature Team.
      ******************************************************************
      * BALANCE_RECALC - Account Balance Calculation Program
      ******************************************************************
      * Purpose: Calculate and maintain account balances from ledger
      * Input:   dbo.LedgerEntries table (SQL Server)
      * Output:  dbo.AccountBalances table (SQL Server)
      *          dbo.PostingAudit table (audit trail)
      * Lineage: transform_kind=aggregate
      ******************************************************************
      * LINEAGE: PROGRAM=BALANCE_RECALC
      * LINEAGE: SRC=sqlserver.dbo.LedgerEntries(ACC_ID,CURRENCY,
      *               DEBIT,CREDIT)
      * LINEAGE: TGT=sqlserver.dbo.AccountBalances(ACC_ID,CURRENCY,
      *               BALANCE,AS_OF_UTC)
      * LINEAGE: MAP=ACC_ID -> ACC_ID [GROUP BY]
      * LINEAGE: MAP=CURRENCY -> CURRENCY [GROUP BY]
      * LINEAGE: MAP=DEBIT,CREDIT -> BALANCE [SUM(CREDIT)-SUM(DEBIT)]
      * LINEAGE: MAP=CURRENT_TIMESTAMP -> AS_OF_UTC [SYSTEM]
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
           05  FILLER                  PIC X(15) VALUE 'BALANCE_RECALC_'.
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
       01  SQL-ACC-ID                  PIC X(32).
       01  SQL-CURRENCY                PIC X(3).
       01  SQL-BALANCE                 PIC S9(16)V99 COMP-3.
       01  SQL-SUM-DEBIT               PIC S9(16)V99 COMP-3.
       01  SQL-SUM-CREDIT              PIC S9(16)V99 COMP-3.
       01  SQL-AUDIT-ROWS-IN           PIC 9(9) COMP-5.
       01  SQL-AUDIT-ROWS-OUT          PIC 9(9) COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

      * Null indicators
       01  SQL-NULL-INDICATORS.
           05  SQL-SUM-DEBIT-NULL      PIC S9(4) COMP-5.
           05  SQL-SUM-CREDIT-NULL     PIC S9(4) COMP-5.

      * SQL communication area
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * Cursor declaration - aggregate ledger entries by account
       EXEC SQL
           DECLARE BALANCE_CURSOR CURSOR FOR
           SELECT ACC_ID, CURRENCY,
                  SUM(DEBIT) AS SUM_DEBIT,
                  SUM(CREDIT) AS SUM_CREDIT
           FROM dbo.LedgerEntries
           GROUP BY ACC_ID, CURRENCY
           ORDER BY ACC_ID, CURRENCY
       END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '============================================'.
           DISPLAY 'BALANCE_RECALC - Balance Calculation'.
           DISPLAY '============================================'.
           DISPLAY ' '.

           PERFORM INITIALIZE-PROGRAM.
           PERFORM CONNECT-DATABASE.
           PERFORM CALCULATE-BALANCES.
           PERFORM LOG-LINEAGE-EVENT.
           PERFORM LOG-AUDIT-RECORD.
           PERFORM DISCONNECT-DATABASE.
           PERFORM DISPLAY-SUMMARY.

           DISPLAY ' '.
           DISPLAY '============================================'.
           DISPLAY 'BALANCE_RECALC completed successfully'.
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

      *    Load git commit SHA (simplified)
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
      * CALCULATE-BALANCES: Aggregate ledger entries and update
      ******************************************************************
       CALCULATE-BALANCES SECTION.
           DISPLAY 'Opening cursor on aggregated ledger entries...'.

      *    Open cursor
           EXEC SQL
               OPEN BALANCE_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to open cursor'
               DISPLAY 'SQLCODE: ' SQLCODE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-CURSOR-OPEN.

      *    Fetch and process each aggregated row
           PERFORM FETCH-AND-UPSERT-BALANCE
               UNTIL SQLCODE = 100 OR SQLCODE < 0.

      *    Close cursor
           EXEC SQL
               CLOSE BALANCE_CURSOR
           END-EXEC.

           MOVE 'N' TO WS-CURSOR-OPEN.
           DISPLAY 'Balance calculation complete'.
       CALCULATE-BALANCES-EXIT.
           EXIT.

      ******************************************************************
      * FETCH-AND-UPSERT-BALANCE: Fetch aggregate and update balance
      ******************************************************************
       FETCH-AND-UPSERT-BALANCE SECTION.
      *    Fetch next aggregated balance
           EXEC SQL
               FETCH BALANCE_CURSOR INTO
                   :SQL-ACC-ID,
                   :SQL-CURRENCY,
                   :SQL-SUM-DEBIT:SQL-SUM-DEBIT-NULL,
                   :SQL-SUM-CREDIT:SQL-SUM-CREDIT-NULL
           END-EXEC.

           IF SQLCODE = 100
               GO TO FETCH-AND-UPSERT-BALANCE-EXIT
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Fetch failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
               GO TO FETCH-AND-UPSERT-BALANCE-EXIT
           END-IF.

           ADD 1 TO WS-ROWS-INPUT.

      *    Calculate balance: SUM(CREDIT) - SUM(DEBIT)
           PERFORM CALCULATE-BALANCE.

      *    Upsert into AccountBalances table
           PERFORM UPSERT-ACCOUNT-BALANCE.

       FETCH-AND-UPSERT-BALANCE-EXIT.
           EXIT.

      ******************************************************************
      * CALCULATE-BALANCE: Compute balance from debit/credit sums
      ******************************************************************
       CALCULATE-BALANCE SECTION.
      *    Handle NULL sums (treat as 0)
           IF SQL-SUM-DEBIT-NULL = -1
               MOVE 0 TO SQL-SUM-DEBIT
           END-IF.

           IF SQL-SUM-CREDIT-NULL = -1
               MOVE 0 TO SQL-SUM-CREDIT
           END-IF.

      *    Calculate: BALANCE = CREDIT - DEBIT
           COMPUTE SQL-BALANCE = SQL-SUM-CREDIT - SQL-SUM-DEBIT.

       CALCULATE-BALANCE-EXIT.
           EXIT.

      ******************************************************************
      * UPSERT-ACCOUNT-BALANCE: Insert or update AccountBalances
      ******************************************************************
       UPSERT-ACCOUNT-BALANCE SECTION.
      *    SQL Server MERGE (upsert) statement
           EXEC SQL
               MERGE dbo.AccountBalances AS target
               USING (SELECT :SQL-ACC-ID AS ACC_ID,
                             :SQL-CURRENCY AS CURRENCY,
                             :SQL-BALANCE AS BALANCE,
                             SYSUTCDATETIME() AS AS_OF_UTC) AS source
               ON (target.ACC_ID = source.ACC_ID
                   AND target.CURRENCY = source.CURRENCY)
               WHEN MATCHED THEN
                   UPDATE SET BALANCE = source.BALANCE,
                              AS_OF_UTC = source.AS_OF_UTC
               WHEN NOT MATCHED THEN
                   INSERT (ACC_ID, CURRENCY, BALANCE, AS_OF_UTC)
                   VALUES (source.ACC_ID, source.CURRENCY,
                           source.BALANCE, source.AS_OF_UTC)
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Balance upsert failed for '
                       SQL-ACC-ID ' ' SQL-CURRENCY
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
           ELSE
               ADD 1 TO WS-ROWS-OUTPUT
           END-IF.

       UPSERT-ACCOUNT-BALANCE-EXIT.
           EXIT.

      ******************************************************************
      * LOG-LINEAGE-EVENT: Record transformation metadata
      ******************************************************************
       LOG-LINEAGE-EVENT SECTION.
      *    Set lineage parameters
           MOVE 'BALANCE_RECALC' TO WS-LIN-PROGRAM.
           MOVE 'sqlserver' TO WS-LIN-SRC-ENGINE.
           MOVE 'dbo' TO WS-LIN-SRC-SCHEMA.
           MOVE 'LedgerEntries' TO WS-LIN-SRC-TABLE.
           MOVE 'ACC_ID,CURRENCY,DEBIT,CREDIT'
               TO WS-LIN-SRC-COLS.
           MOVE 'sqlserver' TO WS-LIN-TGT-ENGINE.
           MOVE 'dbo' TO WS-LIN-TGT-SCHEMA.
           MOVE 'AccountBalances' TO WS-LIN-TGT-TABLE.
           MOVE 'ACC_ID,CURRENCY,BALANCE,AS_OF_UTC'
               TO WS-LIN-TGT-COLS.
           MOVE 'aggregate' TO WS-LIN-TRANSFORM-KIND.
           MOVE 'SUM(CREDIT)-SUM(DEBIT) GROUP BY ACC_ID,CURRENCY'
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
                   'BALANCE_RECALC',
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
           DISPLAY '  Aggregates processed:   ' WS-ROWS-INPUT.
           DISPLAY '  Balances updated:       ' WS-ROWS-OUTPUT.
           DISPLAY '  Errors encountered:     ' WS-ERRORS.
       DISPLAY-SUMMARY-EXIT.
           EXIT.

       END PROGRAM BALANCE-RECALC.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPL-REPORTING.
       AUTHOR. COBOL Data Lineage Feature Team.
      ******************************************************************
      * REPL_REPORTING - Cross-Database Replication Program
      ******************************************************************
      * Purpose: Replicate data from SQL Server to PostgreSQL
      * Input:   dbo.vw_DailyBalances, dbo.vw_CurrencyRollups (SQL Server)
      * Output:  public.daily_snapshots, public.account_rollups (PostgreSQL)
      * Lineage: transform_kind=replication
      ******************************************************************
      * LINEAGE: PROGRAM=REPL_REPORTING
      * LINEAGE: SRC=sqlserver.dbo.vw_DailyBalances(snap_date,ACC_ID,
      *               BALANCE,CURRENCY)
      * LINEAGE: TGT=postgres.public.daily_snapshots(snap_date,acc_id,
      *               end_balance,currency)
      * LINEAGE: MAP=snap_date -> snap_date [COPY]
      * LINEAGE: MAP=ACC_ID -> acc_id [LOWERCASE]
      * LINEAGE: MAP=BALANCE -> end_balance [COPY]
      * LINEAGE: MAP=CURRENCY -> currency [COPY]
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
           05  FILLER                  PIC X(15) VALUE 'REPL_REPORTING_'.
           05  WS-RUN-ID-TS            PIC X(15).

      * Connection flags
       01  WS-PG-CONNECTION-STATUS     PIC X VALUE 'N'.
           88  PG-IS-CONNECTED         VALUE 'Y'.

      * Cursor processing
       01  WS-CURSOR-OPEN              PIC X VALUE 'N'.
           88  CURSOR-IS-OPEN          VALUE 'Y'.

      * Row counters
       01  WS-SNAPSHOTS-REPLICATED     PIC 9(9) COMP-5 VALUE ZERO.
       01  WS-ROLLUPS-REPLICATED       PIC 9(9) COMP-5 VALUE ZERO.
       01  WS-ERRORS                   PIC 9(9) COMP-5 VALUE ZERO.

      * EXEC SQL working variables
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-SNAP-DATE               PIC X(10).
       01  SQL-ACC-ID                  PIC X(32).
       01  SQL-BALANCE                 PIC S9(16)V99 COMP-3.
       01  SQL-CURRENCY                PIC X(3).
       01  SQL-AS-OF-UTC               PIC X(32).
       01  SQL-TOTAL-BALANCE           PIC S9(18)V99 COMP-3.
       EXEC SQL END DECLARE SECTION END-EXEC.

      * SQL communication area
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * Cursor declarations
       EXEC SQL AT SQLSRV_CONN
           DECLARE DAILY_BALANCE_CURSOR CURSOR FOR
           SELECT snap_date, ACC_ID, BALANCE, CURRENCY
           FROM dbo.vw_DailyBalances
       END-EXEC.

       EXEC SQL AT SQLSRV_CONN
           DECLARE CURRENCY_ROLLUP_CURSOR CURSOR FOR
           SELECT as_of_utc, CURRENCY, total_balance
           FROM dbo.vw_CurrencyRollups
       END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '============================================'.
           DISPLAY 'REPL_REPORTING - Cross-DB Replication'.
           DISPLAY '============================================'.
           DISPLAY ' '.

           PERFORM INITIALIZE-PROGRAM.
           PERFORM CONNECT-DATABASES.
           PERFORM REPLICATE-DAILY-SNAPSHOTS.
           PERFORM REPLICATE-CURRENCY-ROLLUPS.
           PERFORM LOG-LINEAGE-EVENTS.
           PERFORM DISCONNECT-DATABASES.
           PERFORM DISPLAY-SUMMARY.

           DISPLAY ' '.
           DISPLAY '============================================'.
           DISPLAY 'REPL_REPORTING completed successfully'.
           DISPLAY '============================================'.

           STOP RUN.

      ******************************************************************
      * INITIALIZE-PROGRAM: Set up working storage and run ID
      ******************************************************************
       INITIALIZE-PROGRAM SECTION.
           MOVE ZERO TO WS-SNAPSHOTS-REPLICATED.
           MOVE ZERO TO WS-ROLLUPS-REPLICATED.
           MOVE ZERO TO WS-ERRORS.

      *    Generate unique run ID from current timestamp
           ACCEPT WS-RUN-ID-TS FROM TIME.
           DISPLAY 'Run ID: ' WS-CURRENT-RUN-ID.

      *    Load git commit SHA (simplified)
           MOVE 'UNKNOWN' TO WS-COMMIT-SHA.
       INITIALIZE-PROGRAM-EXIT.
           EXIT.

      ******************************************************************
      * CONNECT-DATABASES: Connect to both SQL Server and PostgreSQL
      ******************************************************************
       CONNECT-DATABASES SECTION.
      *    Connect to SQL Server (source)
           DISPLAY 'Connecting to SQL Server (DSN: '
                   WS-DSN-SQLSERVER ')...'.

           EXEC SQL AT SQLSRV_CONN
               CONNECT TO :WS-DSN-SQLSERVER
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: SQL Server connection failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'SQLSTATE: ' SQLSTATE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-SQLSERVER-CONNECTED.
           DISPLAY 'SQL Server connection established'.

      *    Connect to PostgreSQL (target)
           DISPLAY 'Connecting to PostgreSQL (DSN: '
                   WS-DSN-POSTGRES ')...'.

           EXEC SQL AT PG_CONN
               CONNECT TO :WS-DSN-POSTGRES
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: PostgreSQL connection failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'SQLSTATE: ' SQLSTATE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-PG-CONNECTION-STATUS.
           DISPLAY 'PostgreSQL connection established'.
       CONNECT-DATABASES-EXIT.
           EXIT.

      ******************************************************************
      * REPLICATE-DAILY-SNAPSHOTS: Replicate daily balance snapshots
      ******************************************************************
       REPLICATE-DAILY-SNAPSHOTS SECTION.
           DISPLAY 'Replicating daily balance snapshots...'.

      *    Open cursor on SQL Server view
           EXEC SQL AT SQLSRV_CONN
               OPEN DAILY_BALANCE_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to open daily balance cursor'
               DISPLAY 'SQLCODE: ' SQLCODE
               GO TO REPLICATE-DAILY-SNAPSHOTS-EXIT
           END-IF.

      *    Fetch and insert/update each snapshot
           PERFORM FETCH-AND-UPSERT-SNAPSHOT
               UNTIL SQLCODE = 100 OR SQLCODE < 0.

      *    Close cursor
           EXEC SQL AT SQLSRV_CONN
               CLOSE DAILY_BALANCE_CURSOR
           END-EXEC.

           DISPLAY 'Daily snapshot replication complete'.
       REPLICATE-DAILY-SNAPSHOTS-EXIT.
           EXIT.

      ******************************************************************
      * FETCH-AND-UPSERT-SNAPSHOT: Fetch from SQL Server, insert to PG
      ******************************************************************
       FETCH-AND-UPSERT-SNAPSHOT SECTION.
      *    Fetch next daily balance row
           EXEC SQL AT SQLSRV_CONN
               FETCH DAILY_BALANCE_CURSOR INTO
                   :SQL-SNAP-DATE,
                   :SQL-ACC-ID,
                   :SQL-BALANCE,
                   :SQL-CURRENCY
           END-EXEC.

           IF SQLCODE = 100
               GO TO FETCH-AND-UPSERT-SNAPSHOT-EXIT
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Fetch daily balance failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
               GO TO FETCH-AND-UPSERT-SNAPSHOT-EXIT
           END-IF.

      *    Upsert into PostgreSQL (ON CONFLICT UPDATE)
           EXEC SQL AT PG_CONN
               INSERT INTO public.daily_snapshots (
                   snap_date, acc_id, end_balance, currency
               ) VALUES (
                   CAST(:SQL-SNAP-DATE AS DATE),
                   :SQL-ACC-ID,
                   :SQL-BALANCE,
                   :SQL-CURRENCY
               )
               ON CONFLICT (snap_date, acc_id)
               DO UPDATE SET
                   end_balance = EXCLUDED.end_balance,
                   currency = EXCLUDED.currency
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Snapshot insert failed for '
                       SQL-ACC-ID
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
           ELSE
               ADD 1 TO WS-SNAPSHOTS-REPLICATED
           END-IF.

       FETCH-AND-UPSERT-SNAPSHOT-EXIT.
           EXIT.

      ******************************************************************
      * REPLICATE-CURRENCY-ROLLUPS: Replicate currency rollups
      ******************************************************************
       REPLICATE-CURRENCY-ROLLUPS SECTION.
           DISPLAY 'Replicating currency rollups...'.

      *    Open cursor on SQL Server view
           EXEC SQL AT SQLSRV_CONN
               OPEN CURRENCY_ROLLUP_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to open rollup cursor'
               DISPLAY 'SQLCODE: ' SQLCODE
               GO TO REPLICATE-CURRENCY-ROLLUPS-EXIT
           END-IF.

      *    Fetch and insert each rollup
           PERFORM FETCH-AND-INSERT-ROLLUP
               UNTIL SQLCODE = 100 OR SQLCODE < 0.

      *    Close cursor
           EXEC SQL AT SQLSRV_CONN
               CLOSE CURRENCY_ROLLUP_CURSOR
           END-EXEC.

           DISPLAY 'Currency rollup replication complete'.
       REPLICATE-CURRENCY-ROLLUPS-EXIT.
           EXIT.

      ******************************************************************
      * FETCH-AND-INSERT-ROLLUP: Fetch from SQL Server, insert to PG
      ******************************************************************
       FETCH-AND-INSERT-ROLLUP SECTION.
      *    Fetch next currency rollup row
           EXEC SQL AT SQLSRV_CONN
               FETCH CURRENCY_ROLLUP_CURSOR INTO
                   :SQL-AS-OF-UTC,
                   :SQL-CURRENCY,
                   :SQL-TOTAL-BALANCE
           END-EXEC.

           IF SQLCODE = 100
               GO TO FETCH-AND-INSERT-ROLLUP-EXIT
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Fetch rollup failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
               GO TO FETCH-AND-INSERT-ROLLUP-EXIT
           END-IF.

      *    Insert into PostgreSQL
           EXEC SQL AT PG_CONN
               INSERT INTO public.account_rollups (
                   as_of_utc, currency, total_balance
               ) VALUES (
                   CAST(:SQL-AS-OF-UTC AS TIMESTAMPTZ),
                   :SQL-CURRENCY,
                   :SQL-TOTAL-BALANCE
               )
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Rollup insert failed for '
                       SQL-CURRENCY
               DISPLAY 'SQLCODE: ' SQLCODE
               ADD 1 TO WS-ERRORS
           ELSE
               ADD 1 TO WS-ROLLUPS-REPLICATED
           END-IF.

       FETCH-AND-INSERT-ROLLUP-EXIT.
           EXIT.

      ******************************************************************
      * LOG-LINEAGE-EVENTS: Record transformation metadata for both
      ******************************************************************
       LOG-LINEAGE-EVENTS SECTION.
      *    Log lineage for daily snapshot replication
           MOVE 'REPL_REPORTING' TO WS-LIN-PROGRAM.
           MOVE 'sqlserver' TO WS-LIN-SRC-ENGINE.
           MOVE 'dbo' TO WS-LIN-SRC-SCHEMA.
           MOVE 'vw_DailyBalances' TO WS-LIN-SRC-TABLE.
           MOVE 'snap_date,ACC_ID,BALANCE,CURRENCY'
               TO WS-LIN-SRC-COLS.
           MOVE 'postgres' TO WS-LIN-TGT-ENGINE.
           MOVE 'public' TO WS-LIN-TGT-SCHEMA.
           MOVE 'daily_snapshots' TO WS-LIN-TGT-TABLE.
           MOVE 'snap_date,acc_id,end_balance,currency'
               TO WS-LIN-TGT-COLS.
           MOVE 'replication' TO WS-LIN-TRANSFORM-KIND.
           MOVE 'Cross-database replication with UPSERT'
               TO WS-LIN-TRANSFORM-EXPR.
           MOVE WS-CURRENT-RUN-ID TO WS-LIN-RUN-ID.

           EXEC SQL AT SQLSRV_CONN END-EXEC.
           PERFORM ADD-LINEAGE-EVENT.

      *    Log lineage for currency rollup replication
           MOVE 'vw_CurrencyRollups' TO WS-LIN-SRC-TABLE.
           MOVE 'as_of_utc,CURRENCY,total_balance'
               TO WS-LIN-SRC-COLS.
           MOVE 'account_rollups' TO WS-LIN-TGT-TABLE.
           MOVE 'as_of_utc,currency,total_balance'
               TO WS-LIN-TGT-COLS.
           MOVE 'Currency aggregation replication'
               TO WS-LIN-TRANSFORM-EXPR.

           PERFORM ADD-LINEAGE-EVENT.

           DISPLAY 'Lineage events logged successfully'.
       LOG-LINEAGE-EVENTS-EXIT.
           EXIT.

      ******************************************************************
      * DISCONNECT-DATABASES: Commit and disconnect both databases
      ******************************************************************
       DISCONNECT-DATABASES SECTION.
      *    Commit and disconnect PostgreSQL
           IF PG-IS-CONNECTED
               EXEC SQL AT PG_CONN
                   COMMIT WORK
               END-EXEC

               IF SQLCODE NOT = 0
                   DISPLAY 'WARNING: PostgreSQL commit failed'
                   DISPLAY 'SQLCODE: ' SQLCODE
                   EXEC SQL AT PG_CONN
                       ROLLBACK WORK
                   END-EXEC
               ELSE
                   DISPLAY 'PostgreSQL transaction committed'
               END-IF

               EXEC SQL AT PG_CONN
                   DISCONNECT CURRENT
               END-EXEC

               MOVE 'N' TO WS-PG-CONNECTION-STATUS
               DISPLAY 'PostgreSQL connection closed'
           END-IF.

      *    Commit and disconnect SQL Server
           IF SQLSERVER-IS-CONNECTED
               EXEC SQL AT SQLSRV_CONN
                   COMMIT WORK
               END-EXEC

               IF SQLCODE NOT = 0
                   DISPLAY 'WARNING: SQL Server commit failed'
                   DISPLAY 'SQLCODE: ' SQLCODE
                   EXEC SQL AT SQLSRV_CONN
                       ROLLBACK WORK
                   END-EXEC
               ELSE
                   DISPLAY 'SQL Server transaction committed'
               END-IF

               EXEC SQL AT SQLSRV_CONN
                   DISCONNECT CURRENT
               END-EXEC

               MOVE 'N' TO WS-SQLSERVER-CONNECTED
               DISPLAY 'SQL Server connection closed'
           END-IF.

       DISCONNECT-DATABASES-EXIT.
           EXIT.

      ******************************************************************
      * DISPLAY-SUMMARY: Show processing statistics
      ******************************************************************
       DISPLAY-SUMMARY SECTION.
           DISPLAY ' '.
           DISPLAY 'Replication Summary:'.
           DISPLAY '  Daily snapshots replicated: '
                   WS-SNAPSHOTS-REPLICATED.
           DISPLAY '  Currency rollups replicated: '
                   WS-ROLLUPS-REPLICATED.
           DISPLAY '  Errors encountered:          '
                   WS-ERRORS.
       DISPLAY-SUMMARY-EXIT.
           EXIT.

       END PROGRAM REPL-REPORTING.

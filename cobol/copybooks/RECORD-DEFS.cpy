      ******************************************************************
      * RECORD-DEFS.cpy - Common Record Layout Definitions
      ******************************************************************
      * Purpose: Shared record structures for database tables
      * Usage: COPY RECORD-DEFS.
      ******************************************************************

      * Transaction record layout (from CSV and dbo.Transactions)
       01  WS-TRANSACTION-REC.
           05  WS-TX-ID                PIC 9(18) COMP-5.
           05  WS-ACC-ID               PIC X(32).
           05  WS-AMOUNT               PIC S9(16)V99 COMP-3.
           05  WS-CURRENCY             PIC X(3).
           05  WS-TX-TS-UTC            PIC X(26).
           05  WS-TX-TYPE              PIC X(32).

      * Ledger entry record layout (dbo.LedgerEntries)
       01  WS-LEDGER-ENTRY-REC.
           05  WS-ENTRY-ID             PIC 9(18) COMP-5.
           05  WS-LE-TX-ID             PIC 9(18) COMP-5.
           05  WS-LE-ACC-ID            PIC X(32).
           05  WS-DEBIT                PIC S9(16)V99 COMP-3.
           05  WS-CREDIT               PIC S9(16)V99 COMP-3.
           05  WS-LE-CURRENCY          PIC X(3).
           05  WS-POSTED-TS-UTC        PIC X(26).

      * Account balance record layout (dbo.AccountBalances)
       01  WS-ACCOUNT-BALANCE-REC.
           05  WS-AB-ACC-ID            PIC X(32).
           05  WS-AB-CURRENCY          PIC X(3).
           05  WS-BALANCE              PIC S9(16)V99 COMP-3.
           05  WS-AS-OF-UTC            PIC X(26).

      * Lineage event record layout (dbo.LineageEvents)
       01  WS-LINEAGE-EVENT-REC.
           05  WS-LE-EVENT-ID          PIC 9(18) COMP-5.
           05  WS-LE-PROGRAM           PIC X(64).
           05  WS-LE-SRC-ENGINE        PIC X(32).
           05  WS-LE-SRC-SCHEMA        PIC X(64).
           05  WS-LE-SRC-TABLE         PIC X(64).
           05  WS-LE-SRC-COLS          PIC X(1024).
           05  WS-LE-TGT-ENGINE        PIC X(32).
           05  WS-LE-TGT-SCHEMA        PIC X(64).
           05  WS-LE-TGT-TABLE         PIC X(64).
           05  WS-LE-TGT-COLS          PIC X(1024).
           05  WS-LE-TRANSFORM-KIND    PIC X(32).
           05  WS-LE-TRANSFORM-EXPR    PIC X(1024).
           05  WS-LE-COMMIT-SHA        PIC X(40).
           05  WS-LE-RUN-ID            PIC X(64).
           05  WS-LE-TS-UTC            PIC X(26).

      * Posting audit record layout (dbo.PostingAudit)
       01  WS-POSTING-AUDIT-REC.
           05  WS-PA-AUDIT-ID          PIC 9(18) COMP-5.
           05  WS-PA-PROGRAM           PIC X(64).
           05  WS-PA-RUN-ID            PIC X(64).
           05  WS-PA-ROWS-IN           PIC 9(9) COMP-5.
           05  WS-PA-ROWS-OUT          PIC 9(9) COMP-5.
           05  WS-PA-TS-UTC            PIC X(26).

      * Daily snapshot record layout (public.daily_snapshots)
       01  WS-DAILY-SNAPSHOT-REC.
           05  WS-DS-SNAP-DATE         PIC X(10).
           05  WS-DS-ACC-ID            PIC X(32).
           05  WS-DS-END-BALANCE       PIC S9(16)V99 COMP-3.
           05  WS-DS-CURRENCY          PIC X(3).

      * Account rollup record layout (public.account_rollups)
       01  WS-ACCOUNT-ROLLUP-REC.
           05  WS-AR-AS-OF-UTC         PIC X(32).
           05  WS-AR-CURRENCY          PIC X(3).
           05  WS-AR-TOTAL-BALANCE     PIC S9(18)V99 COMP-3.

      * CSV parsing working storage
       01  WS-CSV-BUFFER               PIC X(512).
       01  WS-CSV-FIELD-COUNT          PIC 9(2) COMP-5.

      * General purpose counters
       01  WS-COUNTERS.
           05  WS-RECORDS-READ         PIC 9(9) COMP-5 VALUE ZERO.
           05  WS-RECORDS-WRITTEN      PIC 9(9) COMP-5 VALUE ZERO.
           05  WS-RECORDS-SKIPPED      PIC 9(9) COMP-5 VALUE ZERO.
           05  WS-RECORDS-ERROR        PIC 9(9) COMP-5 VALUE ZERO.

      ******************************************************************
      * End of RECORD-DEFS.cpy
      ******************************************************************

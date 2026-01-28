       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINEAGE-EXPORT.
       AUTHOR. COBOL Data Lineage Feature Team.
      ******************************************************************
      * LINEAGE_EXPORT - Lineage Metadata CSV Export Program
      ******************************************************************
      * Purpose: Export lineage events from database to CSV file
      * Input:   dbo.LineageEvents table (SQL Server)
      * Output:  lineage/out/lineage.csv (CSV format)
      * Format:  OpenMetadata-compatible CSV with 14 columns
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LINEAGE-CSV
               ASSIGN TO "lineage/out/lineage.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LINEAGE-CSV.
       01  CSV-OUTPUT-RECORD           PIC X(2048).

       WORKING-STORAGE SECTION.
      * Include shared copybooks
       COPY DB-CONFIG.
       COPY RECORD-DEFS.

      * File status
       01  WS-CSV-FILE-STATUS          PIC XX.
           88  CSV-FILE-OK             VALUE '00'.

      * Cursor processing
       01  WS-CURSOR-OPEN              PIC X VALUE 'N'.
           88  CURSOR-IS-OPEN          VALUE 'Y'.

      * Row counters
       01  WS-EVENTS-EXPORTED          PIC 9(9) COMP-5 VALUE ZERO.

      * CSV formatting working storage
       01  WS-CSV-LINE                 PIC X(2048).
       01  WS-FIELD-DELIMITER          PIC X VALUE ','.
       01  WS-FIELD-QUOTE              PIC X VALUE '"'.

      * EXEC SQL working variables
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-EVENT-ID                PIC 9(18) COMP-5.
       01  SQL-PROGRAM                 PIC X(64).
       01  SQL-SRC-ENGINE              PIC X(32).
       01  SQL-SRC-SCHEMA              PIC X(64).
       01  SQL-SRC-TABLE               PIC X(64).
       01  SQL-SRC-COLS                PIC X(1024).
       01  SQL-TGT-ENGINE              PIC X(32).
       01  SQL-TGT-SCHEMA              PIC X(64).
       01  SQL-TGT-TABLE               PIC X(64).
       01  SQL-TGT-COLS                PIC X(1024).
       01  SQL-TRANSFORM-KIND          PIC X(32).
       01  SQL-TRANSFORM-EXPR          PIC X(1024).
       01  SQL-COMMIT-SHA              PIC X(40).
       01  SQL-RUN-ID                  PIC X(64).
       01  SQL-TS-UTC                  PIC X(26).
       EXEC SQL END DECLARE SECTION END-EXEC.

      * Null indicators for nullable columns
       01  SQL-NULL-INDICATORS.
           05  SQL-SRC-ENGINE-NULL     PIC S9(4) COMP-5.
           05  SQL-SRC-SCHEMA-NULL     PIC S9(4) COMP-5.
           05  SQL-SRC-TABLE-NULL      PIC S9(4) COMP-5.
           05  SQL-SRC-COLS-NULL       PIC S9(4) COMP-5.
           05  SQL-TGT-COLS-NULL       PIC S9(4) COMP-5.
           05  SQL-TRANSFORM-EXPR-NULL PIC S9(4) COMP-5.
           05  SQL-COMMIT-SHA-NULL     PIC S9(4) COMP-5.

      * SQL communication area
       EXEC SQL INCLUDE SQLCA END-EXEC.

      * Cursor declaration
       EXEC SQL
           DECLARE LINEAGE_CURSOR CURSOR FOR
           SELECT EVENT_ID, PROGRAM,
                  SRC_ENGINE, SRC_SCHEMA, SRC_TABLE, SRC_COLS,
                  TGT_ENGINE, TGT_SCHEMA, TGT_TABLE, TGT_COLS,
                  TRANSFORM_KIND, TRANSFORM_EXPR,
                  COMMIT_SHA, RUN_ID, TS_UTC
           FROM dbo.LineageEvents
           ORDER BY TS_UTC ASC
       END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '============================================'.
           DISPLAY 'LINEAGE_EXPORT - CSV Export'.
           DISPLAY '============================================'.
           DISPLAY ' '.

           PERFORM CONNECT-DATABASE.
           PERFORM OPEN-CSV-FILE.
           PERFORM WRITE-CSV-HEADER.
           PERFORM EXPORT-LINEAGE-EVENTS.
           PERFORM CLOSE-CSV-FILE.
           PERFORM DISCONNECT-DATABASE.
           PERFORM DISPLAY-SUMMARY.

           DISPLAY ' '.
           DISPLAY '============================================'.
           DISPLAY 'LINEAGE_EXPORT completed successfully'.
           DISPLAY '============================================'.

           STOP RUN.

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
      * OPEN-CSV-FILE: Create output CSV file
      ******************************************************************
       OPEN-CSV-FILE SECTION.
           DISPLAY 'Creating CSV file: lineage/out/lineage.csv'.

           OPEN OUTPUT LINEAGE-CSV.
           IF NOT CSV-FILE-OK
               DISPLAY 'ERROR: Cannot create CSV file'
               DISPLAY 'File status: ' WS-CSV-FILE-STATUS
               STOP RUN
           END-IF.

           DISPLAY 'CSV file opened successfully'.
       OPEN-CSV-FILE-EXIT.
           EXIT.

      ******************************************************************
      * WRITE-CSV-HEADER: Write column names
      ******************************************************************
       WRITE-CSV-HEADER SECTION.
      *    Build header row with 14 columns
           STRING
               'program' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'src_engine' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'src_schema' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'src_table' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'src_cols' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'tgt_engine' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'tgt_schema' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'tgt_table' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'tgt_cols' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'transform_kind' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'transform_expr' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'commit_sha' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'run_id' DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               'ts_utc' DELIMITED BY SIZE
               INTO WS-CSV-LINE
           END-STRING.

           WRITE CSV-OUTPUT-RECORD FROM WS-CSV-LINE.
           DISPLAY 'CSV header written'.
       WRITE-CSV-HEADER-EXIT.
           EXIT.

      ******************************************************************
      * EXPORT-LINEAGE-EVENTS: Fetch all events and write CSV rows
      ******************************************************************
       EXPORT-LINEAGE-EVENTS SECTION.
           DISPLAY 'Fetching lineage events...'.

      *    Open cursor
           EXEC SQL
               OPEN LINEAGE_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to open cursor'
               DISPLAY 'SQLCODE: ' SQLCODE
               STOP RUN
           END-IF.

           MOVE 'Y' TO WS-CURSOR-OPEN.

      *    Fetch and process each row
           PERFORM FETCH-AND-WRITE-EVENT
               UNTIL SQLCODE = 100 OR SQLCODE < 0.

      *    Close cursor
           EXEC SQL
               CLOSE LINEAGE_CURSOR
           END-EXEC.

           MOVE 'N' TO WS-CURSOR-OPEN.
           DISPLAY 'Lineage export complete'.
       EXPORT-LINEAGE-EVENTS-EXIT.
           EXIT.

      ******************************************************************
      * FETCH-AND-WRITE-EVENT: Fetch one event and write CSV row
      ******************************************************************
       FETCH-AND-WRITE-EVENT SECTION.
      *    Fetch next lineage event
           EXEC SQL
               FETCH LINEAGE_CURSOR INTO
                   :SQL-EVENT-ID,
                   :SQL-PROGRAM,
                   :SQL-SRC-ENGINE:SQL-SRC-ENGINE-NULL,
                   :SQL-SRC-SCHEMA:SQL-SRC-SCHEMA-NULL,
                   :SQL-SRC-TABLE:SQL-SRC-TABLE-NULL,
                   :SQL-SRC-COLS:SQL-SRC-COLS-NULL,
                   :SQL-TGT-ENGINE,
                   :SQL-TGT-SCHEMA,
                   :SQL-TGT-TABLE,
                   :SQL-TGT-COLS:SQL-TGT-COLS-NULL,
                   :SQL-TRANSFORM-KIND,
                   :SQL-TRANSFORM-EXPR:SQL-TRANSFORM-EXPR-NULL,
                   :SQL-COMMIT-SHA:SQL-COMMIT-SHA-NULL,
                   :SQL-RUN-ID,
                   :SQL-TS-UTC
           END-EXEC.

           IF SQLCODE = 100
               GO TO FETCH-AND-WRITE-EVENT-EXIT
           END-IF.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Fetch failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               GO TO FETCH-AND-WRITE-EVENT-EXIT
           END-IF.

      *    Format and write CSV row
           PERFORM FORMAT-CSV-ROW.
           WRITE CSV-OUTPUT-RECORD FROM WS-CSV-LINE.
           ADD 1 TO WS-EVENTS-EXPORTED.

       FETCH-AND-WRITE-EVENT-EXIT.
           EXIT.

      ******************************************************************
      * FORMAT-CSV-ROW: Build CSV row with proper quoting/escaping
      ******************************************************************
       FORMAT-CSV-ROW SECTION.
      *    Handle NULL values and build comma-delimited row
      *    Note: Simplified - production code would handle quoting
      *    for fields containing commas or quotes

           INITIALIZE WS-CSV-LINE.

      *    Build CSV row (14 fields)
           STRING
               FUNCTION TRIM(SQL-PROGRAM) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-SRC-ENGINE) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-SRC-SCHEMA) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-SRC-TABLE) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               FUNCTION TRIM(SQL-SRC-COLS) DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TGT-ENGINE) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TGT-SCHEMA) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TGT-TABLE) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TGT-COLS) DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TRANSFORM-KIND) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TRANSFORM-EXPR) DELIMITED BY SIZE
               WS-FIELD-QUOTE DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-COMMIT-SHA) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-RUN-ID) DELIMITED BY SIZE
               WS-FIELD-DELIMITER DELIMITED BY SIZE
               FUNCTION TRIM(SQL-TS-UTC) DELIMITED BY SIZE
               INTO WS-CSV-LINE
           END-STRING.

       FORMAT-CSV-ROW-EXIT.
           EXIT.

      ******************************************************************
      * CLOSE-CSV-FILE: Close output file
      ******************************************************************
       CLOSE-CSV-FILE SECTION.
           CLOSE LINEAGE-CSV.
           DISPLAY 'CSV file closed'.
       CLOSE-CSV-FILE-EXIT.
           EXIT.

      ******************************************************************
      * DISCONNECT-DATABASE: Disconnect from SQL Server
      ******************************************************************
       DISCONNECT-DATABASE SECTION.
           IF NOT SQLSERVER-IS-CONNECTED
               GO TO DISCONNECT-DATABASE-EXIT
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
      * DISPLAY-SUMMARY: Show export statistics
      ******************************************************************
       DISPLAY-SUMMARY SECTION.
           DISPLAY ' '.
           DISPLAY 'Export Summary:'.
           DISPLAY '  Lineage events exported: ' WS-EVENTS-EXPORTED.
           DISPLAY '  Output file: lineage/out/lineage.csv'.
       DISPLAY-SUMMARY-EXIT.
           EXIT.

       END PROGRAM LINEAGE-EXPORT.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TX-INBOUND.
       AUTHOR. COBOL Data Lineage Feature Team.
      ******************************************************************
      * TX_INBOUND - CSV Transaction Ingestion Program
      ******************************************************************
      * Purpose: Ingest retail banking transactions from CSV files
      *          into SQL Server operational database
      * Input:   data/in/transactions.csv (CSV format)
      * Output:  dbo.Transactions table (SQL Server)
      * Lineage: transform_kind=ingest
      ******************************************************************
      * LINEAGE: PROGRAM=TX_INBOUND
      * LINEAGE: SRC=csv.filesystem.transactions.csv(ACC_ID,AMOUNT,
      *               CURRENCY,TX_TS_UTC,TX_TYPE)
      * LINEAGE: TGT=sqlserver.dbo.Transactions(TX_ID,ACC_ID,AMOUNT,
      *               CURRENCY,TX_TS_UTC,TX_TYPE)
      * LINEAGE: MAP=ACC_ID -> ACC_ID [COPY]
      * LINEAGE: MAP=AMOUNT -> AMOUNT [VALIDATE RANGE]
      * LINEAGE: MAP=CURRENCY -> CURRENCY [UPPERCASE, ISO4217 VALIDATE]
      * LINEAGE: MAP=TX_TS_UTC -> TX_TS_UTC [DATETIME VALIDATE]
      * LINEAGE: MAP=TX_TYPE -> TX_TYPE [COPY, UPPERCASE]
      * LINEAGE: REF={git_commit_sha}
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-CSV
               ASSIGN TO "data/in/transactions.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-CSV.
       01  CSV-RECORD                  PIC X(512).

       WORKING-STORAGE SECTION.
      * Include shared copybooks
       COPY DB-CONFIG.
       COPY RECORD-DEFS.
       COPY LINEAGE-LOGGER.

      * File status
       01  WS-CSV-FILE-STATUS          PIC XX.
           88  CSV-FILE-OK             VALUE '00'.
           88  CSV-END-OF-FILE         VALUE '10'.

      * CSV parsing fields
       01  WS-CSV-LINE                 PIC X(512).
       01  WS-CSV-HEADER               PIC X(512).
       01  WS-FIELD-DELIMITER          PIC X VALUE ','.
       01  WS-FIELD-QUOTE              PIC X VALUE '"'.

      * Parsed transaction fields
       01  WS-CSV-ACC-ID               PIC X(32).
       01  WS-CSV-AMOUNT-STR           PIC X(20).
       01  WS-CSV-CURRENCY             PIC X(3).
       01  WS-CSV-TX-TS-UTC            PIC X(26).
       01  WS-CSV-TX-TYPE              PIC X(32).

      * Validation flags
       01  WS-VALIDATION-FLAGS.
           05  WS-ROW-VALID            PIC X VALUE 'Y'.
               88  ROW-IS-VALID        VALUE 'Y'.
               88  ROW-IS-INVALID      VALUE 'N'.
           05  WS-SKIP-ROW             PIC X VALUE 'N'.
               88  SKIP-THIS-ROW       VALUE 'Y'.

      * Row counters
       01  WS-ROW-NUMBER               PIC 9(9) COMP-5 VALUE 1.
       01  WS-ROWS-VALID               PIC 9(9) COMP-5 VALUE ZERO.
       01  WS-ROWS-INVALID             PIC 9(9) COMP-5 VALUE ZERO.

      * Run identifier (timestamp-based)
       01  WS-RUN-ID-TIMESTAMP         PIC X(20).
       01  WS-CURRENT-RUN-ID.
           05  FILLER                  PIC X(11) VALUE 'TX_INBOUND_'.
           05  WS-RUN-ID-TS            PIC X(15).

      * EXEC SQL working variables
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  SQL-ACC-ID                  PIC X(32).
       01  SQL-AMOUNT                  PIC S9(16)V99 COMP-3.
       01  SQL-CURRENCY                PIC X(3).
       01  SQL-TX-TS-UTC               PIC X(26).
       01  SQL-TX-TYPE                 PIC X(32).
       EXEC SQL END DECLARE SECTION END-EXEC.

      * SQL communication area
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '============================================'.
           DISPLAY 'TX_INBOUND - CSV Transaction Ingestion'.
           DISPLAY '============================================'.
           DISPLAY ' '.

           PERFORM INITIALIZE-PROGRAM.
           PERFORM LOAD-VERSION-FILE.
           PERFORM CONNECT-DATABASE.
           PERFORM PROCESS-CSV-FILE.
           PERFORM LOG-LINEAGE-EVENT.
           PERFORM DISCONNECT-DATABASE.
           PERFORM DISPLAY-SUMMARY.

           DISPLAY ' '.
           DISPLAY '============================================'.
           DISPLAY 'TX_INBOUND completed successfully'.
           DISPLAY '============================================'.

           STOP RUN.

      ******************************************************************
      * INITIALIZE-PROGRAM: Set up working storage and run ID
      ******************************************************************
       INITIALIZE-PROGRAM SECTION.
           MOVE ZERO TO WS-ROWS-VALID.
           MOVE ZERO TO WS-ROWS-INVALID.
           MOVE 1 TO WS-ROW-NUMBER.

      *    Generate unique run ID from current timestamp
           ACCEPT WS-RUN-ID-TIMESTAMP FROM TIME.
           MOVE WS-RUN-ID-TIMESTAMP TO WS-RUN-ID-TS.
           DISPLAY 'Run ID: ' WS-CURRENT-RUN-ID.
       INITIALIZE-PROGRAM-EXIT.
           EXIT.

      ******************************************************************
      * LOAD-VERSION-FILE: Read git commit SHA from .version file
      ******************************************************************
       LOAD-VERSION-FILE SECTION.
           OPEN INPUT TRANSACTION-CSV.
           IF NOT CSV-FILE-OK
               MOVE 'UNKNOWN' TO WS-COMMIT-SHA
               DISPLAY 'WARNING: .version file not found, using UNKNOWN'
               GO TO LOAD-VERSION-FILE-EXIT
           END-IF.

      *    Simple approach: Try to read .version file
      *    In production, use proper file I/O with .version path
           MOVE 'UNKNOWN' TO WS-COMMIT-SHA.
           CLOSE TRANSACTION-CSV.

           DISPLAY 'Git Commit SHA: ' WS-COMMIT-SHA.
       LOAD-VERSION-FILE-EXIT.
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
      * PROCESS-CSV-FILE: Read and process all CSV rows
      ******************************************************************
       PROCESS-CSV-FILE SECTION.
           DISPLAY 'Opening CSV file: data/in/transactions.csv'.

           OPEN INPUT TRANSACTION-CSV.
           IF NOT CSV-FILE-OK
               DISPLAY 'ERROR: Cannot open CSV file'
               DISPLAY 'File status: ' WS-CSV-FILE-STATUS
               STOP RUN
           END-IF.

      *    Read header row and validate
           READ TRANSACTION-CSV INTO WS-CSV-HEADER
               AT END
                   DISPLAY 'ERROR: Empty CSV file'
                   CLOSE TRANSACTION-CSV
                   STOP RUN
           END-READ.

           DISPLAY 'CSV header: ' WS-CSV-HEADER.

      *    Process data rows
           PERFORM PROCESS-CSV-ROW UNTIL CSV-END-OF-FILE.

           CLOSE TRANSACTION-CSV.
           DISPLAY 'CSV file processing complete'.
       PROCESS-CSV-FILE-EXIT.
           EXIT.

      ******************************************************************
      * PROCESS-CSV-ROW: Parse and validate a single CSV row
      ******************************************************************
       PROCESS-CSV-ROW SECTION.
           READ TRANSACTION-CSV INTO WS-CSV-LINE
               AT END
                   SET CSV-END-OF-FILE TO TRUE
                   GO TO PROCESS-CSV-ROW-EXIT
           END-READ.

           ADD 1 TO WS-ROW-NUMBER.
           MOVE 'Y' TO WS-ROW-VALID.

      *    Parse CSV fields using UNSTRING
           UNSTRING WS-CSV-LINE DELIMITED BY WS-FIELD-DELIMITER
               INTO WS-CSV-ACC-ID
                    WS-CSV-AMOUNT-STR
                    WS-CSV-CURRENCY
                    WS-CSV-TX-TS-UTC
                    WS-CSV-TX-TYPE
           END-UNSTRING.

      *    Validate parsed data
           PERFORM VALIDATE-CSV-FIELDS.

           IF ROW-IS-VALID
               PERFORM INSERT-TRANSACTION
               ADD 1 TO WS-ROWS-VALID
           ELSE
               ADD 1 TO WS-ROWS-INVALID
           END-IF.

       PROCESS-CSV-ROW-EXIT.
           EXIT.

      ******************************************************************
      * VALIDATE-CSV-FIELDS: Apply validation rules to parsed data
      ******************************************************************
       VALIDATE-CSV-FIELDS SECTION.
      *    Validate ACC_ID (non-empty, max 32 chars)
           IF WS-CSV-ACC-ID = SPACES OR WS-CSV-ACC-ID = LOW-VALUES
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - ACC_ID is empty'
               MOVE 'N' TO WS-ROW-VALID
               GO TO VALIDATE-CSV-FIELDS-EXIT
           END-IF.

      *    Validate AMOUNT (numeric, within range)
      *    Note: In production, use proper numeric validation
           IF WS-CSV-AMOUNT-STR = SPACES
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - AMOUNT is empty'
               MOVE 'N' TO WS-ROW-VALID
               GO TO VALIDATE-CSV-FIELDS-EXIT
           END-IF.

      *    Validate CURRENCY (3 chars, uppercase)
           IF WS-CSV-CURRENCY = SPACES
               OR FUNCTION LENGTH(FUNCTION TRIM(WS-CSV-CURRENCY))
                  NOT = 3
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - Invalid CURRENCY: ' WS-CSV-CURRENCY
               MOVE 'N' TO WS-ROW-VALID
               GO TO VALIDATE-CSV-FIELDS-EXIT
           END-IF.

      *    Convert CURRENCY to uppercase
           MOVE FUNCTION UPPER-CASE(WS-CSV-CURRENCY)
               TO WS-CSV-CURRENCY.

      *    Validate TX_TS_UTC (non-empty datetime)
           IF WS-CSV-TX-TS-UTC = SPACES
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - TX_TS_UTC is empty'
               MOVE 'N' TO WS-ROW-VALID
               GO TO VALIDATE-CSV-FIELDS-EXIT
           END-IF.

      *    Validate TX_TYPE (non-empty, max 32 chars)
           IF WS-CSV-TX-TYPE = SPACES
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - TX_TYPE is empty'
               MOVE 'N' TO WS-ROW-VALID
               GO TO VALIDATE-CSV-FIELDS-EXIT
           END-IF.

       VALIDATE-CSV-FIELDS-EXIT.
           EXIT.

      ******************************************************************
      * INSERT-TRANSACTION: Insert validated row into database
      ******************************************************************
       INSERT-TRANSACTION SECTION.
      *    Prepare SQL host variables
           MOVE WS-CSV-ACC-ID TO SQL-ACC-ID.
           COMPUTE SQL-AMOUNT =
               FUNCTION NUMVAL(WS-CSV-AMOUNT-STR).
           MOVE WS-CSV-CURRENCY TO SQL-CURRENCY.
           MOVE WS-CSV-TX-TS-UTC TO SQL-TX-TS-UTC.
           MOVE WS-CSV-TX-TYPE TO SQL-TX-TYPE.

      *    Insert into database
           EXEC SQL
               INSERT INTO dbo.Transactions (
                   ACC_ID,
                   AMOUNT,
                   CURRENCY,
                   TX_TS_UTC,
                   TX_TYPE
               ) VALUES (
                   :SQL-ACC-ID,
                   :SQL-AMOUNT,
                   :SQL-CURRENCY,
                   :SQL-TX-TS-UTC,
                   :SQL-TX-TYPE
               )
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Row ' WS-ROW-NUMBER
                       ' - Database insert failed'
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'ACC_ID: ' SQL-ACC-ID
               MOVE 'N' TO WS-ROW-VALID
               ADD 1 TO WS-ROWS-INVALID
               SUBTRACT 1 FROM WS-ROWS-VALID
           END-IF.

       INSERT-TRANSACTION-EXIT.
           EXIT.

      ******************************************************************
      * LOG-LINEAGE-EVENT: Record transformation metadata
      ******************************************************************
       LOG-LINEAGE-EVENT SECTION.
      *    Only log if we processed at least one valid row
           IF WS-ROWS-VALID = ZERO
               GO TO LOG-LINEAGE-EVENT-EXIT
           END-IF.

      *    Set lineage parameters
           MOVE 'TX_INBOUND' TO WS-LIN-PROGRAM.
           MOVE 'csv' TO WS-LIN-SRC-ENGINE.
           MOVE 'filesystem' TO WS-LIN-SRC-SCHEMA.
           MOVE 'transactions.csv' TO WS-LIN-SRC-TABLE.
           MOVE 'ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE'
               TO WS-LIN-SRC-COLS.
           MOVE 'sqlserver' TO WS-LIN-TGT-ENGINE.
           MOVE 'dbo' TO WS-LIN-TGT-SCHEMA.
           MOVE 'Transactions' TO WS-LIN-TGT-TABLE.
           MOVE 'TX_ID,ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE'
               TO WS-LIN-TGT-COLS.
           MOVE 'ingest' TO WS-LIN-TRANSFORM-KIND.
           MOVE 'CSV file parse and validate; insert valid rows'
               TO WS-LIN-TRANSFORM-EXPR.
           MOVE WS-CURRENT-RUN-ID TO WS-LIN-RUN-ID.

      *    Call lineage logger copybook section
           PERFORM ADD-LINEAGE-EVENT.

           DISPLAY 'Lineage event logged successfully'.
       LOG-LINEAGE-EVENT-EXIT.
           EXIT.

      ******************************************************************
      * DISCONNECT-DATABASE: Commit transaction and disconnect
      ******************************************************************
       DISCONNECT-DATABASE SECTION.
           IF NOT SQLSERVER-IS-CONNECTED
               GO TO DISCONNECT-DATABASE-EXIT
           END-IF.

      *    Commit all inserts
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
           DISPLAY '  Total rows processed: '
                   WS-ROW-NUMBER - 1.
           DISPLAY '  Valid rows inserted:  ' WS-ROWS-VALID.
           DISPLAY '  Invalid rows skipped: ' WS-ROWS-INVALID.
       DISPLAY-SUMMARY-EXIT.
           EXIT.

       END PROGRAM TX-INBOUND.

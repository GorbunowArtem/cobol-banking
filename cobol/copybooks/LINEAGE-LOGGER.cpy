      ******************************************************************
      * LINEAGE-LOGGER.cpy - Lineage Event Logging Copybook
      ******************************************************************
      * Purpose: Reusable section for inserting lineage events
      * Usage: COPY LINEAGE-LOGGER.
      *        PERFORM ADD-LINEAGE-EVENT
      ******************************************************************

      * Lineage event parameters (set before calling ADD-LINEAGE-EVENT)
       01  WS-LINEAGE-PARAMS.
           05  WS-LIN-PROGRAM          PIC X(64).
           05  WS-LIN-SRC-ENGINE       PIC X(32).
           05  WS-LIN-SRC-SCHEMA       PIC X(64).
           05  WS-LIN-SRC-TABLE        PIC X(64).
           05  WS-LIN-SRC-COLS         PIC X(1024).
           05  WS-LIN-TGT-ENGINE       PIC X(32).
           05  WS-LIN-TGT-SCHEMA       PIC X(64).
           05  WS-LIN-TGT-TABLE        PIC X(64).
           05  WS-LIN-TGT-COLS         PIC X(1024).
           05  WS-LIN-TRANSFORM-KIND   PIC X(32).
           05  WS-LIN-TRANSFORM-EXPR   PIC X(1024).
           05  WS-LIN-RUN-ID           PIC X(64).

      * Generated timestamp for lineage event
       01  WS-LINEAGE-TIMESTAMP        PIC X(26).

      ******************************************************************
      * Procedure: ADD-LINEAGE-EVENT
      * Description: Inserts a lineage event into dbo.LineageEvents
      * Prerequisites:
      *   - SQL Server connection established
      *   - WS-LINEAGE-PARAMS populated
      *   - WS-COMMIT-SHA loaded from .version file
      *   - WS-LIN-RUN-ID set to unique run identifier
      ******************************************************************
       ADD-LINEAGE-EVENT SECTION.
           
      *    Get current UTC timestamp
           ACCEPT WS-LINEAGE-TIMESTAMP FROM TIME.
           
      *    Insert lineage event into database
           EXEC SQL
               INSERT INTO dbo.LineageEvents (
                   PROGRAM,
                   SRC_ENGINE,
                   SRC_SCHEMA,
                   SRC_TABLE,
                   SRC_COLS,
                   TGT_ENGINE,
                   TGT_SCHEMA,
                   TGT_TABLE,
                   TGT_COLS,
                   TRANSFORM_KIND,
                   TRANSFORM_EXPR,
                   COMMIT_SHA,
                   RUN_ID,
                   TS_UTC
               ) VALUES (
                   :WS-LIN-PROGRAM,
                   :WS-LIN-SRC-ENGINE,
                   :WS-LIN-SRC-SCHEMA,
                   :WS-LIN-SRC-TABLE,
                   :WS-LIN-SRC-COLS,
                   :WS-LIN-TGT-ENGINE,
                   :WS-LIN-TGT-SCHEMA,
                   :WS-LIN-TGT-TABLE,
                   :WS-LIN-TGT-COLS,
                   :WS-LIN-TRANSFORM-KIND,
                   :WS-LIN-TRANSFORM-EXPR,
                   :WS-COMMIT-SHA,
                   :WS-LIN-RUN-ID,
                   SYSUTCDATETIME()
               )
           END-EXEC.
           
      *    Check for SQL errors
           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Failed to insert lineage event'
               DISPLAY 'SQLCODE: ' SQLCODE
               DISPLAY 'PROGRAM: ' WS-LIN-PROGRAM
               DISPLAY 'TRANSFORM_KIND: ' WS-LIN-TRANSFORM-KIND
           END-IF.
           
       ADD-LINEAGE-EVENT-EXIT.
           EXIT.

      ******************************************************************
      * End of LINEAGE-LOGGER.cpy
      ******************************************************************

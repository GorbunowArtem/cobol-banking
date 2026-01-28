      ******************************************************************
      * DB-CONFIG.cpy - Database Configuration Copybook
      ******************************************************************
      * Purpose: DSN constants and environment configuration
      * Usage: COPY DB-CONFIG.
      ******************************************************************
       01  WS-DATABASE-CONFIG.
           05  WS-DSN-SQLSERVER        PIC X(32) VALUE 'SQLSRV_CBLR'.
           05  WS-DSN-POSTGRES         PIC X(32) VALUE 'PG_CBLR'.
           05  WS-COMMIT-SHA           PIC X(40) VALUE SPACES.
           05  WS-VERSION-FILE         PIC X(128) VALUE '.version'.

      * SQLCODE working storage
       01  WS-SQL-STATUS.
           05  WS-SQLCODE              PIC S9(9) COMP-5.
           05  WS-SQL-STATE            PIC X(5).

      * Connection status flags
       01  WS-CONNECTION-FLAGS.
           05  WS-SQLSERVER-CONNECTED  PIC X VALUE 'N'.
               88  SQLSERVER-IS-CONNECTED  VALUE 'Y'.
           05  WS-POSTGRES-CONNECTED   PIC X VALUE 'N'.
               88  POSTGRES-IS-CONNECTED   VALUE 'Y'.

      ******************************************************************
      * End of DB-CONFIG.cpy
      ******************************************************************

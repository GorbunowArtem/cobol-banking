# Research: COBOL Data Lineage Banking Component

**Feature**: COBOL Data Lineage Banking Component  
**Branch**: `001-cobol-data-lineage`  
**Date**: 2026-01-28

## Purpose

This document consolidates research findings for implementing a COBOL-based banking transaction processing system with comprehensive data lineage tracking across heterogeneous databases (SQL Server and PostgreSQL). Research focuses on COBOL-ODBC connectivity, lineage metadata capture strategies, and cross-database replication patterns.

---

## 1. COBOL-ODBC Database Connectivity

### Decision: Use ODBC with Embedded SQL (ESQL) for Database Access

**Rationale**:
- ODBC is the industry-standard database connectivity layer for COBOL on Linux
- Both Micro Focus Visual COBOL and GnuCOBOL support ODBC through SQL preprocessing
- Provides consistent API across SQL Server and PostgreSQL databases
- Enables embedded SQL statements directly in COBOL code (more readable than CALL interfaces)
- unixODBC is well-supported on Amazon Linux 2023

**Implementation Approach**:
- Configure unixODBC with two DSNs: `SQLSRV_CBLR` (SQL Server), `PG_CBLR` (PostgreSQL)
- Use ODBC drivers: `ODBC Driver 17/18 for SQL Server`, `psqlODBC` for PostgreSQL
- Enable TLS/SSL for encrypted connections in DSN configuration
- Store DSN credentials in secured local files (demo-level encryption acceptable)
- Use EXEC SQL blocks in COBOL code for SQL operations
- Preprocess COBOL source files with SQL preprocessor before compilation

**Alternatives Considered**:
- **Native database libraries**: Rejected - not portable across SQL Server and PostgreSQL
- **REST APIs to databases**: Rejected - adds unnecessary complexity, defeats purpose of COBOL demo
- **File-based data exchange only**: Rejected - doesn't showcase modern COBOL database capabilities

**Key References**:
- Micro Focus COBOL documentation: Embedded SQL programming guide
- GnuCOBOL ESQL preprocessor (if using GnuCOBOL)
- unixODBC configuration documentation for multi-DSN setup

---

## 2. Lineage Metadata Capture Strategy

### Decision: Dual-Track Lineage (Static Comments + Runtime Events)

**Rationale**:
- **Static lineage** (COBOL comments) provides code-level documentation that survives refactoring and supports code review
- **Runtime lineage** (database events) captures actual execution flow with timestamps and run IDs for operational audit
- Dual approach enables validation: compare static declarations with runtime behavior
- Static lineage is parseable by external tools without executing code
- Runtime lineage supports historical analysis and troubleshooting

**Static Lineage Grammar**:
```cobol
*> LINEAGE: PROGRAM=<program-name>
*> SRC=db:<engine>:<database>.<schema>.<table>[col1,col2,...]
*> TGT=db:<engine>:<database>.<schema>.<table>[col1,col2,...]
*> MAP=<transformation-expression>
*> REF=RULE:<rule-id>; PURPOSE=<purpose>; TX_BOUNDARY=<batch|record>
```

**Runtime Lineage Schema**:
```
LineageEvents table with columns:
- event_id (auto-increment primary key)
- program (VARCHAR: program name)
- src_engine, src_schema, src_table, src_cols (source metadata)
- tgt_engine, tgt_schema, tgt_table, tgt_cols (target metadata)
- transform_kind (VARCHAR: ingest|posting|aggregate|replication)
- transform_expr (VARCHAR/TEXT: transformation logic description)
- commit_sha (VARCHAR: git commit from .version file)
- run_id (VARCHAR: unique execution identifier)
- ts_utc (DATETIME/TIMESTAMP: event timestamp)
```

**Lineage Capture Points**:
1. **Ingest** (TX_INBOUND): CSV → dbo.Transactions
2. **Posting** (POST_LEDGER): Transactions → LedgerEntries (with debit/credit logic)
3. **Aggregate** (BALANCE_RECALC): LedgerEntries → AccountBalances (GROUP BY)
4. **Replication** (REPL_REPORTING): SQL Server views → PostgreSQL tables
5. **Export** (LINEAGE_EXPORT): LineageEvents → lineage.csv

**Alternatives Considered**:
- **Runtime-only lineage**: Rejected - no code-level documentation for developers
- **Static-only lineage**: Rejected - no operational audit trail or execution history
- **External lineage collection via database triggers**: Rejected - doesn't capture COBOL program context
- **Application log parsing**: Rejected - fragile, requires log format stability

**Key References**:
- OpenMetadata lineage ingestion format (CSV schema for export compatibility)
- Data governance best practices: combining declarative and operational lineage
- COBOL comment conventions for machine-parseable annotations

---

## 3. Cross-Database Replication Pattern

### Decision: Application-Level Replication via COBOL Program

**Rationale**:
- **Control**: COBOL program (REPL_REPORTING) explicitly manages replication, enabling lineage capture
- **Simplicity**: No database-specific replication tools (SSIS, Postgres FDW, etc.) - keeping demo straightforward
- **Transparency**: Replication logic is visible in COBOL code, showcasing COBOL's data movement capabilities
- **Lineage**: Each replicated row generates a lineage event with clear source→target mapping
- **Demo-friendly**: Easy to understand and demonstrate without complex configuration

**Replication Approach**:
1. Create views in SQL Server operational database (e.g., `vw_DailyBalances`, `vw_CurrencyRollups`)
2. COBOL program reads from SQL Server views using SELECT statements
3. Transform data if needed (minimal - mostly direct mapping)
4. Write to PostgreSQL tables using INSERT or UPSERT (ON CONFLICT)
5. Log lineage event with `transform_kind=replication`, engine-qualified identifiers

**Idempotency Strategy**:
- Use UPSERT (INSERT ... ON CONFLICT UPDATE) for daily snapshots keyed by date + account
- Replication is safe to re-run - updates existing rows with latest data
- Alternatively, truncate-and-load for full refresh scenarios

**Error Handling**:
- Wrap replication in database transactions (COMMIT/ROLLBACK)
- If SQL Server read fails, log error and exit gracefully
- If PostgreSQL write fails, rollback and preserve source data integrity
- Lineage events commit only if replication succeeds (transactional consistency)

**Alternatives Considered**:
- **Database-native replication** (SQL Server transactional replication): Rejected - complex setup, doesn't showcase COBOL, no lineage capture
- **ETL tools** (SSIS, Talend): Rejected - defeats purpose of COBOL demo, external dependencies
- **Foreign Data Wrappers** (Postgres FDW to SQL Server): Rejected - PostgreSQL-specific, not representative of typical COBOL workflows
- **Change Data Capture** (CDC): Rejected - overkill for demo, requires SQL Server Enterprise, adds complexity

**Key References**:
- ODBC transaction management: COMMIT and ROLLBACK patterns
- PostgreSQL UPSERT syntax: INSERT ... ON CONFLICT DO UPDATE
- SQL Server indexed views for efficient read-only queries

---

## 4. Double-Entry Ledger Posting Logic

### Decision: Conditional Debit/Credit Assignment Based on Amount Sign

**Rationale**:
- Standard double-entry accounting convention: positive amounts are credits (money in), negative amounts are debits (money out)
- Simple conditional logic in COBOL (IF-THEN-ELSE) is easy to demonstrate
- Each transaction generates exactly one ledger entry (not two) - simplified for demo purposes
- Clear lineage expression: "IF AMOUNT >= 0 THEN CREDIT=AMOUNT ELSE DEBIT=ABS(AMOUNT)"

**Posting Algorithm**:
```cobol
FOR EACH unposted Transaction:
  IF TX_AMOUNT >= 0 THEN
    INSERT LedgerEntry (CREDIT=TX_AMOUNT, DEBIT=NULL)
  ELSE
    INSERT LedgerEntry (DEBIT=ABS(TX_AMOUNT), CREDIT=NULL)
  END-IF
  UPDATE Transaction SET POSTED_FLAG = 'Y'
  LOG lineage event with transformation logic
END-FOR
COMMIT
```

**Audit Trail**:
- PostingAudit table records: program name, run ID, rows processed, timestamp
- Enables reconciliation: count(Transactions) should match count(LedgerEntries)
- If posting run fails mid-stream, ROLLBACK prevents partial posting

**Alternatives Considered**:
- **True double-entry** (two ledger entries per transaction): Rejected - more complex, demo doesn't require contra-accounts
- **Always debit** (standardize sign): Rejected - less realistic banking model
- **Separate tables for debits and credits**: Rejected - unnecessary normalization for demo

**Key References**:
- Double-entry accounting principles (simplified for demo)
- COBOL arithmetic: ABS function for absolute values
- Transaction isolation levels for consistent reads

---

## 5. Git Commit SHA Integration

### Decision: .version File with Git Commit SHA

**Rationale**:
- Lineage events need code version traceability (which code version produced this data?)
- Git commit SHA is immutable and uniquely identifies code state
- Simple file-based approach: `.version` file at repository root contains current commit SHA
- COBOL programs read `.version` file and include SHA in lineage events
- Easy to generate: `git rev-parse HEAD > .version` in deployment/build scripts

**Implementation**:
- Add `.version` file generation to test runner scripts (before program execution)
- COBOL programs read `.version` file into working storage variable
- Include `commit_sha` column in lineage events
- If `.version` missing or unreadable, use default value "UNKNOWN" (graceful degradation per edge case requirement)

**Build/Deployment Integration**:
```bash
# In deployment script or CI/CD pipeline
git rev-parse HEAD > .version
# Then compile and run COBOL programs
```

**Alternatives Considered**:
- **Environment variable**: Rejected - less portable, requires shell environment setup
- **Hardcode SHA at compile time**: Rejected - requires recompilation for every commit
- **Query git directly from COBOL**: Rejected - adds git dependency to runtime, complicates COBOL code
- **Omit version tracking**: Rejected - lineage requirement FR-010 explicitly requires commit SHA

**Key References**:
- Git plumbing commands: `git rev-parse HEAD`
- COBOL file I/O for reading text files
- Graceful degradation patterns for missing config files

---

## 6. CSV File Format and Parsing

### Decision: Standard CSV with Header Row

**Rationale**:
- CSV is simple, human-readable, and universally supported
- Header row documents column names (self-describing format)
- Standard delimiters (comma) and quoting (double-quote) for compatibility
- Easy to generate sample data in Excel or scripts

**Transaction CSV Format**:
```
ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE
ACC001,100.50,USD,2026-01-28T10:30:00Z,DEPOSIT
ACC002,-25.00,USD,2026-01-28T10:35:00Z,WITHDRAWAL
ACC003,500.00,EUR,2026-01-28T10:40:00Z,TRANSFER
```

**COBOL Parsing Approach**:
- Use COBOL sequential file processing (OPEN INPUT, READ, CLOSE)
- Skip first line (header row) or use it for validation
- Parse each line using UNSTRING into working storage variables
- Validate each field before database insertion (FR-002 validation requirement)

**Validation Rules**:
- ACC_ID: non-empty, alphanumeric, max 32 chars
- AMOUNT: numeric, valid decimal format, reasonable range (-1M to +1M)
- CURRENCY: 3-character code (USD, EUR, GBP, etc.)
- TX_TS_UTC: ISO 8601 format, parseable to DATETIME
- TX_TYPE: non-empty, max 32 chars

**Error Handling**:
- Invalid rows: log error to stderr, skip row, continue processing
- Validation failures do not stop entire batch (per edge case requirement)
- Count valid vs. invalid rows, report in PostingAudit

**Alternatives Considered**:
- **Fixed-width format**: Rejected - less flexible, harder to edit manually
- **JSON or XML**: Rejected - overkill for demo, COBOL parsing is more complex
- **Binary format**: Rejected - not human-readable, defeats demo purpose
- **Multiple CSV files**: Rejected - single file is simpler for demo

**Key References**:
- COBOL UNSTRING command for delimited parsing
- ISO 8601 timestamp format
- CSV RFC 4180 (for quoting and escape rules)

---

## 7. ODBC Configuration for Dual Databases

### Decision: Separate DSNs with Explicit Driver Configuration

**Rationale**:
- unixODBC supports multiple DSNs in single `odbc.ini` file
- Each DSN specifies connection string, driver, SSL/TLS settings independently
- COBOL programs use DSN name (not connection string) for database operations
- Configuration is external to COBOL code (environment-specific settings)

**odbc.ini Configuration**:
```ini
[SQLSRV_CBLR]
Driver=ODBC Driver 18 for SQL Server
Server=<rds-endpoint>.us-east-1.rds.amazonaws.com
Database=cblr_ops
UID=<username>
PWD=<password>
Encrypt=yes
TrustServerCertificate=no

[PG_CBLR]
Driver=PostgreSQL Unicode
Server=<aurora-endpoint>.us-east-1.rds.amazonaws.com
Database=cblr_report
Port=5432
UID=<username>
PWD=<password>
SSLMode=require
```

**odbcinst.ini Configuration**:
```ini
[ODBC Driver 18 for SQL Server]
Description=Microsoft ODBC Driver 18 for SQL Server
Driver=/opt/microsoft/msodbcsql18/lib64/libmsodbcsql-18.so

[PostgreSQL Unicode]
Description=PostgreSQL ODBC Driver (Unicode)
Driver=/usr/lib64/psqlodbcw.so
```

**COBOL Usage**:
```cobol
EXEC SQL CONNECT TO 'SQLSRV_CBLR' END-EXEC.
EXEC SQL SELECT ... FROM dbo.Transactions END-EXEC.
EXEC SQL DISCONNECT END-EXEC.

EXEC SQL CONNECT TO 'PG_CBLR' END-EXEC.
EXEC SQL INSERT INTO public.daily_snapshots ... END-EXEC.
EXEC SQL DISCONNECT END-EXEC.
```

**Security Considerations**:
- Store credentials in odbc.ini with restricted file permissions (chmod 600)
- For production: use AWS Secrets Manager or IAM database authentication (out of scope for demo)
- TLS/SSL enabled for all connections (Encrypt=yes, SSLMode=require)

**Alternatives Considered**:
- **Single DSN with dynamic database switching**: Rejected - not supported by ODBC standard
- **Hardcoded connection strings in COBOL**: Rejected - not portable, security risk
- **Connection pooling**: Rejected - unnecessary for batch processing demo

**Key References**:
- unixODBC documentation: DSN configuration
- Microsoft ODBC Driver for SQL Server on Linux
- psqlODBC driver documentation
- AWS RDS connection best practices (SSL/TLS)

---

## 8. Lineage CSV Export Format

### Decision: Flat CSV with OpenMetadata-Compatible Schema

**Rationale**:
- Target audience: data governance tools like OpenMetadata
- CSV is universally ingestible, simple to validate
- Flat structure (no nested objects) for maximum compatibility
- Chronological ordering (by ts_utc) shows data flow sequence

**Lineage CSV Schema**:
```
program,src_engine,src_schema,src_table,src_cols,tgt_engine,tgt_schema,tgt_table,tgt_cols,transform_kind,transform_expr,commit_sha,run_id,ts_utc
TX_INBOUND,csv,filesystem,transactions.csv,"ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",sqlserver,dbo,Transactions,"TX_ID,ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",ingest,"CSV file parse and validate",abc123def456,run-2026-01-28-103045,2026-01-28T10:30:45Z
POST_LEDGER,sqlserver,dbo,Transactions,"TX_ID,ACC_ID,AMOUNT,CURRENCY",sqlserver,dbo,LedgerEntries,"ENTRY_ID,TX_ID,ACC_ID,DEBIT,CREDIT,CURRENCY",posting,"IF AMOUNT>=0 THEN CREDIT=AMOUNT ELSE DEBIT=ABS(AMOUNT)",abc123def456,run-2026-01-28-103050,2026-01-28T10:30:50Z
...
```

**Column Definitions**:
- `program`: COBOL program name
- `src_engine`: Source database engine (csv, sqlserver, postgres)
- `src_schema`: Source schema/namespace (dbo, public, filesystem)
- `src_table`: Source table/file name
- `src_cols`: Comma-separated list of source columns (quoted if contains commas)
- `tgt_engine`, `tgt_schema`, `tgt_table`, `tgt_cols`: Target metadata (same format)
- `transform_kind`: Transformation type (ingest, posting, aggregate, replication)
- `transform_expr`: Human-readable transformation logic
- `commit_sha`: Git commit SHA from .version file
- `run_id`: Unique run identifier (e.g., "run-YYYY-MM-DD-HHMMSS")
- `ts_utc`: Timestamp in ISO 8601 UTC format

**COBOL Export Logic**:
```cobol
OPEN OUTPUT LINEAGE-CSV-FILE.
WRITE HEADER-LINE.
EXEC SQL DECLARE lineage_cursor CURSOR FOR
  SELECT * FROM dbo.LineageEvents ORDER BY ts_utc
END-EXEC.
EXEC SQL OPEN lineage_cursor END-EXEC.
LOOP
  EXEC SQL FETCH lineage_cursor INTO :host-variables END-EXEC.
  IF SQLCODE = 100 THEN EXIT-LOOP.
  FORMAT and WRITE lineage-csv-line.
END-LOOP.
EXEC SQL CLOSE lineage_cursor END-EXEC.
CLOSE LINEAGE-CSV-FILE.
```

**Alternatives Considered**:
- **JSON format**: Rejected - harder to parse in COBOL, less universal than CSV
- **XML format**: Rejected - verbose, complex parsing in COBOL
- **Database view for external query**: Rejected - doesn't meet export requirement (FR-009)
- **Separate files per program**: Rejected - harder to trace complete flow

**Key References**:
- OpenMetadata lineage ingestion documentation
- CSV RFC 4180 (proper escaping and quoting)
- COBOL cursor processing for batch exports

---

## Summary of Research Decisions

| Area | Decision | Key Benefit |
|------|----------|-------------|
| Database Connectivity | ODBC with Embedded SQL | Standard, portable, supports both SQL Server and PostgreSQL |
| Lineage Capture | Dual-track (static comments + runtime events) | Code documentation + operational audit |
| Cross-DB Replication | Application-level COBOL program | Full lineage capture, demo-friendly simplicity |
| Double-Entry Logic | Conditional debit/credit by amount sign | Simple, realistic banking model |
| Code Versioning | .version file with git commit SHA | Immutable version tracking in lineage |
| CSV Format | Standard CSV with header row | Human-readable, easy to generate/parse |
| ODBC Configuration | Separate DSNs per database | Clean separation, SSL/TLS enabled |
| Lineage Export | Flat CSV with OpenMetadata schema | Universal compatibility, chronological flow |

---

## Open Items Resolved

All "NEEDS CLARIFICATION" items from Technical Context have been resolved through this research:

✅ **COBOL-ODBC connectivity**: Embedded SQL with unixODBC and database-specific drivers  
✅ **Lineage metadata capture**: Dual-track approach (static + runtime)  
✅ **Cross-database patterns**: Application-level replication via COBOL program  
✅ **Double-entry accounting**: Conditional logic based on transaction amount sign  
✅ **CSV parsing**: UNSTRING with validation and error handling  
✅ **Git integration**: .version file with commit SHA  
✅ **ODBC configuration**: Dual DSNs with TLS/SSL  
✅ **Lineage export format**: OpenMetadata-compatible flat CSV  

**Research Status**: ✅ **COMPLETE** - Ready for Phase 1 (Design & Contracts)

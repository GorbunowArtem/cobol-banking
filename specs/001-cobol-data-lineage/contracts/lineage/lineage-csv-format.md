# Lineage CSV Export Format Contract

**Feature**: COBOL Data Lineage Banking Component  
**Purpose**: Defines the output CSV format for data lineage metadata export  
**Target Program**: LINEAGE_EXPORT.cbl  
**Target Audience**: Data governance tools (e.g., OpenMetadata)

## Overview

The lineage CSV file is the final output of the data lineage tracking system. It consolidates all runtime lineage events from the operational database into a flat, chronologically-ordered file suitable for ingestion by data governance and catalog tools.

---

## File Specification

**File Name**: `lineage.csv`  
**Location**: `/lineage/out/lineage.csv`  
**Encoding**: UTF-8  
**Line Endings**: LF (Unix)  
**Delimiter**: Comma (`,`)  
**Quoting**: Double quotes (`"`) for fields containing commas, quotes, or special characters  
**Header Row**: Yes (first line contains column names)  
**Row Ordering**: Chronological by ts_utc (oldest to newest)

---

## Column Definitions

| Position | Column Name | Type | Max Length | Nullable | Description |
|----------|-------------|------|------------|----------|-------------|
| 1 | program | String | 64 | No | COBOL program name that performed transformation |
| 2 | src_engine | String | 32 | Yes | Source database engine (csv, sqlserver, postgres) |
| 3 | src_schema | String | 64 | Yes | Source schema or namespace (dbo, public, filesystem) |
| 4 | src_table | String | 64 | Yes | Source table or file name |
| 5 | src_cols | String | - | Yes | Comma-separated list of source columns (quoted) |
| 6 | tgt_engine | String | 32 | No | Target database engine |
| 7 | tgt_schema | String | 64 | No | Target schema or namespace |
| 8 | tgt_table | String | 64 | No | Target table name |
| 9 | tgt_cols | String | - | Yes | Comma-separated list of target columns (quoted) |
| 10 | transform_kind | String | 32 | No | Transformation type (ingest, posting, aggregate, replication) |
| 11 | transform_expr | String | - | Yes | Human-readable transformation logic description |
| 12 | commit_sha | String | 40 | Yes | Git commit SHA (40 hex chars or "UNKNOWN") |
| 13 | run_id | String | 64 | No | Unique run identifier (e.g., "run-2026-01-28-103045") |
| 14 | ts_utc | DateTime | - | No | Event timestamp in ISO 8601 UTC format |

---

## Header Row

```
program,src_engine,src_schema,src_table,src_cols,tgt_engine,tgt_schema,tgt_table,tgt_cols,transform_kind,transform_expr,commit_sha,run_id,ts_utc
```

---

## Data Formats

### program
- **Format**: Uppercase alphanumeric with underscores
- **Examples**: `TX_INBOUND`, `POST_LEDGER`, `BALANCE_RECALC`, `REPL_REPORTING`, `LINEAGE_EXPORT`

### src_engine / tgt_engine
- **Allowed Values**: `csv`, `sqlserver`, `postgres`
- **Case**: Lowercase
- **Note**: Represents database engine or file system type

### src_schema / tgt_schema
- **Examples**: `dbo` (SQL Server), `public` (PostgreSQL), `filesystem` (CSV)
- **Case**: Lowercase
- **Note**: Namespace for table/file

### src_table / tgt_table
- **Examples**: `transactions.csv`, `Transactions`, `LedgerEntries`, `daily_snapshots`
- **Case**: Preserves original case from database
- **Note**: File name or table name

### src_cols / tgt_cols
- **Format**: Comma-separated column names enclosed in outer quotes
- **Example**: `"ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE"`
- **Empty**: Empty string (`""`) if no specific columns tracked
- **Note**: Inner commas are part of the list, not CSV delimiters

### transform_kind
- **Allowed Values**: `ingest`, `posting`, `aggregate`, `replication`
- **Case**: Lowercase
- **Definitions**:
  - `ingest`: CSV → database table
  - `posting`: Source table → transformed table (business logic)
  - `aggregate`: Grouped/summed data transformation
  - `replication`: Cross-database data movement

### transform_expr
- **Format**: Free-text description of transformation logic
- **Quoting**: Enclosed in quotes if contains commas
- **Examples**: 
  - `"CSV file parse and validate; insert valid rows"`
  - `"IF AMOUNT>=0 THEN CREDIT=AMOUNT ELSE DEBIT=ABS(AMOUNT)"`
  - `"SUM(CREDIT)-SUM(DEBIT) GROUP BY ACC_ID,CURRENCY"`
- **Empty**: Empty string (`""`) if no expression available

### commit_sha
- **Format**: 40 hexadecimal characters (git SHA-1)
- **Example**: `abc123def4567890abcdef1234567890abcdef12`
- **Fallback**: `UNKNOWN` if .version file missing or unreadable
- **Case**: Lowercase hex digits

### run_id
- **Format**: Unique identifier per program execution
- **Pattern**: `run-YYYY-MM-DD-HHMMSS` (timestamp-based)
- **Example**: `run-2026-01-28-103045`
- **Note**: Same run_id used across lineage events and audit records for one execution

### ts_utc
- **Format**: ISO 8601 datetime with UTC indicator
- **Pattern**: `YYYY-MM-DDTHH:MM:SSZ`
- **Example**: `2026-01-28T10:30:45Z`
- **Note**: Represents when lineage event was logged (not when data was created)

---

## Example File

```csv
program,src_engine,src_schema,src_table,src_cols,tgt_engine,tgt_schema,tgt_table,tgt_cols,transform_kind,transform_expr,commit_sha,run_id,ts_utc
TX_INBOUND,csv,filesystem,transactions.csv,"ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",sqlserver,dbo,Transactions,"TX_ID,ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",ingest,"CSV file parse and validate; insert valid rows",abc123def4567890abcdef1234567890abcdef12,run-2026-01-28-103045,2026-01-28T10:30:45Z
POST_LEDGER,sqlserver,dbo,Transactions,"TX_ID,ACC_ID,AMOUNT,CURRENCY",sqlserver,dbo,LedgerEntries,"ENTRY_ID,TX_ID,ACC_ID,DEBIT,CREDIT,CURRENCY",posting,"IF AMOUNT>=0 THEN CREDIT=AMOUNT ELSE DEBIT=ABS(AMOUNT)",abc123def4567890abcdef1234567890abcdef12,run-2026-01-28-103050,2026-01-28T10:30:50Z
BALANCE_RECALC,sqlserver,dbo,LedgerEntries,"ACC_ID,CURRENCY,DEBIT,CREDIT",sqlserver,dbo,AccountBalances,"ACC_ID,CURRENCY,BALANCE,AS_OF_UTC",aggregate,"SUM(CREDIT)-SUM(DEBIT) GROUP BY ACC_ID,CURRENCY",abc123def4567890abcdef1234567890abcdef12,run-2026-01-28-103055,2026-01-28T10:30:55Z
REPL_REPORTING,sqlserver,dbo,vw_DailyBalances,"snap_date,ACC_ID,BALANCE,CURRENCY",postgres,public,daily_snapshots,"snap_date,acc_id,end_balance,currency",replication,"Cross-database replication with UPSERT on (snap_date,acc_id)",abc123def4567890abcdef1234567890abcdef12,run-2026-01-28-103100,2026-01-28T10:31:00Z
REPL_REPORTING,sqlserver,dbo,vw_CurrencyRollups,"as_of_utc,CURRENCY,total_balance",postgres,public,account_rollups,"as_of_utc,currency,total_balance",replication,"Currency aggregation replication",abc123def4567890abcdef1234567890abcdef12,run-2026-01-28-103105,2026-01-28T10:31:05Z
```

---

## Data Lineage Flow Example

Reading the example file chronologically:

1. **Ingestion** (10:30:45): CSV file → SQL Server Transactions table
2. **Posting** (10:30:50): Transactions → LedgerEntries (double-entry logic applied)
3. **Aggregation** (10:30:55): LedgerEntries → AccountBalances (summed by account+currency)
4. **Replication** (10:31:00): SQL Server vw_DailyBalances → PostgreSQL daily_snapshots
5. **Replication** (10:31:05): SQL Server vw_CurrencyRollups → PostgreSQL account_rollups

This shows complete end-to-end data flow from source CSV to reporting warehouse.

---

## Export Processing

### LINEAGE_EXPORT Program Workflow

1. **Connect to Database**: Open connection to SQL Server (SQLSRV_CBLR DSN)
2. **Query Lineage Events**: `SELECT * FROM dbo.LineageEvents ORDER BY TS_UTC`
3. **Open Output File**: Create/overwrite `/lineage/out/lineage.csv`
4. **Write Header**: Write column names as first line
5. **Iterate Events**: Fetch each event record and format as CSV line
6. **Handle Quoting**: Quote fields containing commas, newlines, or quotes
7. **Write Line**: Append formatted line to output file
8. **Close Resources**: Close database cursor and output file
9. **Report Status**: Log completion message with row count

---

## Quoting and Escaping Rules

### Fields Requiring Quotes
- Contains comma: `"IF X=1, THEN Y=2"`
- Contains double quote: `"He said ""Hello"""`
- Contains newline: `"Line 1\nLine 2"`
- Starts/ends with whitespace: `" padded value "`

### Escape Sequences
- Double quote within quoted field: `""` (double the quote)
- Example: `"transform_expr": "He said ""stop"" loudly"` → Output: `"He said ""stop"" loudly"`

---

## Validation Rules

### File-Level Validation
- Header row must be present and match expected column names exactly
- All rows must have 14 columns (matching header count)
- File must be UTF-8 encoded
- No empty lines between data rows

### Row-Level Validation
- `program`: Non-empty, max 64 chars
- `tgt_engine`, `tgt_schema`, `tgt_table`: Non-empty (target always required)
- `transform_kind`: Must be one of: ingest, posting, aggregate, replication
- `run_id`: Non-empty, max 64 chars
- `ts_utc`: Valid ISO 8601 datetime format
- `commit_sha`: 40 hex chars or "UNKNOWN"

---

## OpenMetadata Ingestion Compatibility

This CSV format is designed for ingestion into data governance tools like OpenMetadata:

### Mapping to OpenMetadata Lineage Model
- **Upstream (source)**: `src_engine://src_schema.src_table`
- **Downstream (target)**: `tgt_engine://tgt_schema.tgt_table`
- **Column Lineage**: `src_cols` → `tgt_cols`
- **Transformation Logic**: `transform_expr`
- **Metadata**: `program`, `commit_sha`, `run_id`, `ts_utc`

### Ingestion Workflow (External to COBOL System)
1. Transfer `/lineage/out/lineage.csv` to OpenMetadata ingestion location
2. Configure OpenMetadata CSV ingestion connector
3. Map CSV columns to OpenMetadata lineage schema
4. Run ingestion pipeline
5. Verify lineage graph in OpenMetadata UI

**Note**: Actual OpenMetadata ingestion is documented but not automated (out of scope for v1).

---

## Edge Cases

### No Lineage Events
- **Scenario**: LineageEvents table is empty
- **Output**: CSV file with header only (no data rows)
- **Valid**: Yes, this is a valid empty export

### Large Export
- **Scenario**: 10,000+ lineage events
- **Behavior**: All events exported to single CSV file
- **Performance**: Expected <1 minute for 10,000 rows
- **Note**: No pagination; single file output

### Special Characters in Transform Expression
- **Scenario**: `transform_expr` contains commas, quotes, newlines
- **Handling**: Field enclosed in quotes, internal quotes doubled
- **Example**: `"Expression: IF X > 0, THEN ""YES"", ELSE ""NO"""`

### Missing Commit SHA
- **Scenario**: .version file missing during program execution
- **Output**: `commit_sha` column contains `UNKNOWN`
- **Valid**: Yes, this is expected fallback behavior

---

## Testing Recommendations

### Test Case 1: Standard Export
- **Setup**: Run complete pipeline (ingest → post → balance → repl)
- **Expected**: CSV with 4+ rows (one per transform_kind type)
- **Validation**: Parse CSV, verify header, check row count, validate chronological order

### Test Case 2: Empty Export
- **Setup**: Truncate LineageEvents table, run LINEAGE_EXPORT
- **Expected**: CSV with header only, no data rows
- **Validation**: File exists, contains exactly 1 line (header)

### Test Case 3: Quote Handling
- **Setup**: Insert lineage event with complex transform_expr (commas, quotes)
- **Expected**: CSV properly quotes and escapes the field
- **Validation**: Parse CSV with standard CSV library, verify field value matches source

### Test Case 4: Large Export
- **Setup**: Insert 1,000 lineage events, run LINEAGE_EXPORT
- **Expected**: CSV with 1,000 data rows (plus header)
- **Validation**: Row count matches, file size is reasonable (~100-200 KB)

---

## Success Criteria

✅ File generated at `/lineage/out/lineage.csv`  
✅ Header row matches specification exactly  
✅ All lineage events from database included in export  
✅ Chronological ordering (oldest to newest by ts_utc)  
✅ Proper quoting for fields containing commas/quotes  
✅ UTF-8 encoding with LF line endings  
✅ Valid CSV parseable by standard tools (Excel, Python pandas, etc.)  
✅ All 14 columns present in every row  

---

**Contract Version**: 1.0  
**Last Updated**: 2026-01-28  
**Owner**: COBOL Data Lineage Feature Team

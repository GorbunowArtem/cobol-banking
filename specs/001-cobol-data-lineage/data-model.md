# Data Model: COBOL Data Lineage Banking Component

**Feature**: COBOL Data Lineage Banking Component  
**Branch**: `001-cobol-data-lineage`  
**Date**: 2026-01-28

## Purpose

This document defines the data entities, relationships, validation rules, and state transitions for the COBOL banking data lineage system. The model spans three storage layers: CSV files (input), SQL Server operational database, and PostgreSQL reporting warehouse.

---

## Entity Definitions

### 1. Transaction (SQL Server: dbo.Transactions)

**Purpose**: Represents a retail banking transaction from CSV ingestion. Source of truth for all financial movements.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| TX_ID | BIGINT | PRIMARY KEY, IDENTITY | Auto-generated unique transaction identifier |
| ACC_ID | VARCHAR(32) | NOT NULL | Account identifier (alphanumeric, e.g., "ACC001") |
| AMOUNT | DECIMAL(18,2) | NOT NULL | Transaction amount (positive=credit, negative=debit) |
| CURRENCY | CHAR(3) | NOT NULL | ISO 4217 currency code (USD, EUR, GBP, etc.) |
| TX_TS_UTC | DATETIME2 | NOT NULL | Transaction timestamp in UTC |
| TX_TYPE | VARCHAR(32) | NOT NULL | Transaction type (DEPOSIT, WITHDRAWAL, TRANSFER, etc.) |

**Validation Rules**:
- `ACC_ID`: Non-empty, alphanumeric, max 32 characters
- `AMOUNT`: Numeric, range -1,000,000.00 to +1,000,000.00
- `CURRENCY`: Exactly 3 uppercase letters matching ISO 4217 codes
- `TX_TS_UTC`: Valid datetime, not future-dated beyond 1 hour
- `TX_TYPE`: Non-empty, max 32 characters, uppercase recommended

**Relationships**:
- One Transaction → Zero or One LedgerEntry (posted flag determines if posted)

**State**: Immutable after insert (no updates or deletes in demo)

---

### 2. LedgerEntry (SQL Server: dbo.LedgerEntries)

**Purpose**: Represents double-entry accounting records derived from transactions. Each entry has either a debit or credit (never both).

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| ENTRY_ID | BIGINT | PRIMARY KEY, IDENTITY | Auto-generated unique ledger entry identifier |
| TX_ID | BIGINT | NOT NULL | Foreign key to originating transaction |
| ACC_ID | VARCHAR(32) | NOT NULL | Account identifier (denormalized from transaction) |
| DEBIT | DECIMAL(18,2) | NULL | Debit amount (money out, only if CREDIT is NULL) |
| CREDIT | DECIMAL(18,2) | NULL | Credit amount (money in, only if DEBIT is NULL) |
| CURRENCY | CHAR(3) | NOT NULL | Currency code (must match transaction currency) |
| POSTED_TS_UTC | DATETIME2 | NOT NULL | Timestamp when ledger entry was posted |

**Validation Rules**:
- `TX_ID`: Must reference existing Transaction.TX_ID
- `ACC_ID`: Must match Transaction.ACC_ID for corresponding TX_ID
- `DEBIT` and `CREDIT`: Exactly one must be NULL, one must be NOT NULL
- `DEBIT` or `CREDIT` value: Must match ABS(Transaction.AMOUNT)
- `CURRENCY`: Must match Transaction.CURRENCY
- `POSTED_TS_UTC`: Must be >= Transaction.TX_TS_UTC

**Relationships**:
- One LedgerEntry → One Transaction (via TX_ID)
- Many LedgerEntries → One AccountBalance (aggregated by ACC_ID + CURRENCY)

**Business Logic**:
- If Transaction.AMOUNT >= 0, then CREDIT = AMOUNT, DEBIT = NULL
- If Transaction.AMOUNT < 0, then DEBIT = ABS(AMOUNT), CREDIT = NULL

**State**: Immutable after insert (accounting audit trail)

---

### 3. AccountBalance (SQL Server: dbo.AccountBalances)

**Purpose**: Represents current balance for each account-currency combination, calculated from ledger entries.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| ACC_ID | VARCHAR(32) | PRIMARY KEY (composite) | Account identifier |
| CURRENCY | CHAR(3) | PRIMARY KEY (composite) | Currency code |
| BALANCE | DECIMAL(18,2) | NOT NULL | Current balance (SUM(CREDIT) - SUM(DEBIT)) |
| AS_OF_UTC | DATETIME2 | NOT NULL | Timestamp of last balance calculation |

**Validation Rules**:
- `ACC_ID`: Non-empty, alphanumeric, max 32 characters
- `CURRENCY`: Exactly 3 uppercase letters
- `BALANCE`: Numeric, no specific range (can be negative for overdrafts)
- `AS_OF_UTC`: Valid datetime, typically current time when recalculated

**Relationships**:
- One AccountBalance ← Many LedgerEntries (aggregated)
- One AccountBalance → Zero or Many DailySnapshots (replicated to reporting DB)

**Calculation Logic**:
```sql
SELECT ACC_ID, CURRENCY,
       COALESCE(SUM(CREDIT), 0) - COALESCE(SUM(DEBIT), 0) AS BALANCE,
       GETUTCDATE() AS AS_OF_UTC
FROM dbo.LedgerEntries
GROUP BY ACC_ID, CURRENCY
```

**State**: Mutable (recalculated on each balance run, UPSERT pattern)

---

### 4. LineageEvent (SQL Server: dbo.LineageEvents)

**Purpose**: Captures runtime data transformation metadata for every processing step. Core entity for data governance and lineage tracing.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| EVENT_ID | BIGINT | PRIMARY KEY, IDENTITY | Auto-generated unique event identifier |
| PROGRAM | VARCHAR(64) | NOT NULL | COBOL program name (e.g., "TX_INBOUND", "POST_LEDGER") |
| SRC_ENGINE | VARCHAR(32) | NULL | Source database engine (csv, sqlserver, postgres) |
| SRC_SCHEMA | VARCHAR(64) | NULL | Source schema/namespace (dbo, public, filesystem) |
| SRC_TABLE | VARCHAR(64) | NULL | Source table or file name |
| SRC_COLS | VARCHAR(MAX) | NULL | Comma-separated list of source columns |
| TGT_ENGINE | VARCHAR(32) | NOT NULL | Target database engine |
| TGT_SCHEMA | VARCHAR(64) | NOT NULL | Target schema/namespace |
| TGT_TABLE | VARCHAR(64) | NOT NULL | Target table name |
| TGT_COLS | VARCHAR(MAX) | NULL | Comma-separated list of target columns |
| TRANSFORM_KIND | VARCHAR(32) | NOT NULL | Transformation type (ingest, posting, aggregate, replication) |
| TRANSFORM_EXPR | VARCHAR(MAX) | NULL | Human-readable transformation logic |
| COMMIT_SHA | VARCHAR(40) | NULL | Git commit SHA from .version file |
| RUN_ID | VARCHAR(64) | NOT NULL | Unique run identifier (e.g., "run-2026-01-28-103045") |
| TS_UTC | DATETIME2 | NOT NULL | Event timestamp in UTC |

**Validation Rules**:
- `PROGRAM`: Non-empty, max 64 characters, uppercase recommended
- `TRANSFORM_KIND`: Must be one of: ingest, posting, aggregate, replication
- `RUN_ID`: Non-empty, unique per program execution
- `TS_UTC`: Valid datetime, typically current time
- `COMMIT_SHA`: 40 hex characters (git SHA-1 format) or "UNKNOWN"

**Relationships**:
- No foreign keys (decoupled for flexibility)
- Conceptually links to all other entities via table names

**Transform Kinds**:
- **ingest**: CSV → database table (TX_INBOUND)
- **posting**: Transactions → LedgerEntries (POST_LEDGER)
- **aggregate**: LedgerEntries → AccountBalances (BALANCE_RECALC)
- **replication**: SQL Server → PostgreSQL (REPL_REPORTING)

**State**: Append-only (never updated or deleted, immutable audit trail)

---

### 5. PostingAudit (SQL Server: dbo.PostingAudit)

**Purpose**: Operational audit trail for program executions. Captures row counts and timing for reconciliation.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| AUDIT_ID | BIGINT | PRIMARY KEY, IDENTITY | Auto-generated unique audit record identifier |
| PROGRAM | VARCHAR(64) | NOT NULL | COBOL program name |
| RUN_ID | VARCHAR(64) | NOT NULL | Unique run identifier (matches LineageEvent.RUN_ID) |
| ROWS_IN | INT | NOT NULL | Number of input rows processed |
| ROWS_OUT | INT | NOT NULL | Number of output rows produced |
| TS_UTC | DATETIME2 | NOT NULL | Audit record timestamp in UTC |

**Validation Rules**:
- `PROGRAM`: Non-empty, max 64 characters
- `RUN_ID`: Non-empty, should match a LineageEvent.RUN_ID
- `ROWS_IN`, `ROWS_OUT`: Non-negative integers
- `TS_UTC`: Valid datetime

**Relationships**:
- Conceptually linked to LineageEvent via RUN_ID (no formal FK)

**Reconciliation Checks**:
- For POST_LEDGER: ROWS_IN (Transactions) should equal ROWS_OUT (LedgerEntries)
- For BALANCE_RECALC: ROWS_OUT should match distinct account-currency combinations
- For REPL_REPORTING: ROWS_OUT should match rows replicated to PostgreSQL

**State**: Append-only (audit trail)

---

### 6. DailySnapshot (PostgreSQL: public.daily_snapshots)

**Purpose**: Reporting warehouse table containing daily account balances replicated from operational database.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| snap_date | DATE | PRIMARY KEY (composite) | Snapshot date (typically today's date) |
| acc_id | VARCHAR(32) | PRIMARY KEY (composite) | Account identifier |
| end_balance | NUMERIC(18,2) | NOT NULL | Account balance as of snap_date |
| currency | CHAR(3) | NOT NULL | Currency code |

**Validation Rules**:
- `snap_date`: Valid date, typically current date (DATE type)
- `acc_id`: Non-empty, max 32 characters
- `end_balance`: Numeric (can be negative)
- `currency`: Exactly 3 uppercase letters

**Relationships**:
- Replicated from AccountBalance (SQL Server → PostgreSQL)

**Upsert Logic**:
```sql
INSERT INTO public.daily_snapshots (snap_date, acc_id, end_balance, currency)
VALUES (CURRENT_DATE, :acc_id, :balance, :currency)
ON CONFLICT (snap_date, acc_id)
DO UPDATE SET end_balance = EXCLUDED.end_balance, currency = EXCLUDED.currency;
```

**State**: Mutable (updated daily via upsert)

---

### 7. AccountRollup (PostgreSQL: public.account_rollups)

**Purpose**: Reporting warehouse table containing aggregated balance totals by currency for summary reporting.

**Attributes**:

| Column | Type | Constraints | Description |
|--------|------|-------------|-------------|
| as_of_utc | TIMESTAMPTZ | NOT NULL | Timestamp when rollup was calculated |
| currency | CHAR(3) | NOT NULL | Currency code |
| total_balance | NUMERIC(20,2) | NOT NULL | Sum of all account balances in this currency |

**Validation Rules**:
- `as_of_utc`: Valid timestamp with timezone
- `currency`: Exactly 3 uppercase letters
- `total_balance`: Numeric (can be negative if aggregate is negative)

**Relationships**:
- Aggregated from DailySnapshot (GROUP BY currency)

**Calculation Logic**:
```sql
SELECT CURRENT_TIMESTAMP AS as_of_utc,
       currency,
       SUM(end_balance) AS total_balance
FROM public.daily_snapshots
WHERE snap_date = CURRENT_DATE
GROUP BY currency;
```

**State**: Truncate-and-load or append (depending on reporting needs)

---

## Entity Relationship Diagram (Textual)

```
CSV File (transactions.csv)
  └─> [TX_INBOUND] ─> Transaction (SQL Server)
                        └─> [POST_LEDGER] ─> LedgerEntry (SQL Server)
                                               └─> [BALANCE_RECALC] ─> AccountBalance (SQL Server)
                                                                         └─> [REPL_REPORTING] ─> DailySnapshot (PostgreSQL)
                                                                                                  └─> [REPL_REPORTING] ─> AccountRollup (PostgreSQL)

All transformations [PROGRAM] ─> LineageEvent (SQL Server)
All transformations [PROGRAM] ─> PostingAudit (SQL Server)

[LINEAGE_EXPORT] reads LineageEvent ─> lineage.csv
```

**Key**:
- `[PROGRAM]` = COBOL program performing transformation
- `─>` = Data flow direction
- SQL Server entities: Transaction, LedgerEntry, AccountBalance, LineageEvent, PostingAudit
- PostgreSQL entities: DailySnapshot, AccountRollup
- CSV entities: transactions.csv (input), lineage.csv (output)

---

## Data Flow Sequence

1. **Ingestion**: CSV → Transaction (via TX_INBOUND)
   - Lineage: transform_kind=ingest
   
2. **Posting**: Transaction → LedgerEntry (via POST_LEDGER)
   - Lineage: transform_kind=posting
   - Audit: PostingAudit records row counts
   
3. **Aggregation**: LedgerEntry → AccountBalance (via BALANCE_RECALC)
   - Lineage: transform_kind=aggregate
   - Calculation: SUM(CREDIT) - SUM(DEBIT) per account-currency
   
4. **Replication**: AccountBalance → DailySnapshot (via REPL_REPORTING)
   - Lineage: transform_kind=replication
   - Cross-database: SQL Server → PostgreSQL
   
5. **Rollup**: DailySnapshot → AccountRollup (via REPL_REPORTING)
   - Lineage: transform_kind=replication
   - Aggregation: SUM(end_balance) per currency
   
6. **Export**: LineageEvent → lineage.csv (via LINEAGE_EXPORT)
   - Output: CSV file for governance tool ingestion

---

## State Transitions

### Transaction State
- **Created**: Inserted from CSV by TX_INBOUND
- **Posted**: (Implicit) When corresponding LedgerEntry exists
- **No deletion**: Immutable audit trail

### LedgerEntry State
- **Posted**: Inserted by POST_LEDGER
- **No updates or deletes**: Immutable accounting record

### AccountBalance State
- **Calculated**: Created/updated by BALANCE_RECALC
- **Recalculated**: Upserted on each run (mutable)

### LineageEvent State
- **Logged**: Inserted by any COBOL program during transformation
- **Append-only**: Never updated or deleted

### DailySnapshot State
- **Replicated**: Upserted by REPL_REPORTING daily
- **Updated**: If rerun on same date, upsert updates existing snapshot

---

## Indexing Recommendations

**SQL Server (dbo schema)**:
- `Transactions`: Clustered index on TX_ID (PK), Non-clustered index on (ACC_ID, CURRENCY)
- `LedgerEntries`: Clustered index on ENTRY_ID (PK), Non-clustered index on (ACC_ID, CURRENCY), Non-clustered index on TX_ID
- `AccountBalances`: Clustered index on (ACC_ID, CURRENCY) (PK)
- `LineageEvents`: Clustered index on EVENT_ID (PK), Non-clustered index on (RUN_ID, TS_UTC)
- `PostingAudit`: Clustered index on AUDIT_ID (PK), Non-clustered index on RUN_ID

**PostgreSQL (public schema)**:
- `daily_snapshots`: Primary key index on (snap_date, acc_id)
- `account_rollups`: Index on (as_of_utc, currency) if querying frequently

---

## Validation Summary

| Entity | Primary Validation | Secondary Validation |
|--------|-------------------|----------------------|
| Transaction | ACC_ID format, AMOUNT range | Currency code, timestamp validity |
| LedgerEntry | DEBIT/CREDIT mutual exclusivity | Amount matches transaction |
| AccountBalance | ACC_ID + CURRENCY uniqueness | Balance calculation accuracy |
| LineageEvent | RUN_ID uniqueness per program | Transform_kind enum values |
| PostingAudit | Row counts non-negative | RUN_ID consistency with lineage |
| DailySnapshot | snap_date + acc_id uniqueness | Balance consistency with source |
| AccountRollup | Currency rollup accuracy | Timestamp validity |

---

## Data Quality Rules

1. **Referential Integrity**: All LedgerEntry.TX_ID values must reference existing Transaction.TX_ID
2. **Balance Reconciliation**: SUM(AccountBalance.BALANCE) for a currency should equal SUM(CREDIT) - SUM(DEBIT) for that currency
3. **Lineage Completeness**: Every data transformation must generate at least one LineageEvent
4. **Audit Consistency**: PostingAudit.RUN_ID must match LineageEvent.RUN_ID for same program execution
5. **Timestamp Ordering**: LedgerEntry.POSTED_TS_UTC >= Transaction.TX_TS_UTC
6. **Cross-DB Consistency**: DailySnapshot.end_balance should match AccountBalance.BALANCE for same ACC_ID + CURRENCY

---

**Data Model Status**: ✅ **COMPLETE** - Ready for contract generation (Phase 1)

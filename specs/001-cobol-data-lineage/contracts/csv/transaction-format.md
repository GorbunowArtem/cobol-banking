# Transaction CSV Format Contract

**Feature**: COBOL Data Lineage Banking Component  
**Purpose**: Defines the input CSV format for retail banking transactions  
**Target Program**: TX_INBOUND.cbl

## Overview

The transaction CSV file serves as the primary input source for the banking transaction processing pipeline. Files are placed in the `/data/in/` directory and processed by the TX_INBOUND COBOL program.

---

## File Specification

**File Name Pattern**: `transactions.csv` or `transactions-YYYYMMDD.csv`  
**Location**: `/data/in/transactions.csv`  
**Encoding**: UTF-8  
**Line Endings**: LF (Unix) or CRLF (Windows) - both supported  
**Delimiter**: Comma (`,`)  
**Quoting**: Double quotes (`"`) for fields containing commas or quotes  
**Header Row**: Yes (first line contains column names)

---

## Column Definitions

| Position | Column Name | Type | Max Length | Required | Format | Description |
|----------|-------------|------|------------|----------|--------|-------------|
| 1 | ACC_ID | String | 32 | Yes | Alphanumeric | Account identifier (e.g., "ACC001", "CUST-12345") |
| 2 | AMOUNT | Decimal | - | Yes | [-]NNNN.NN | Transaction amount (positive=credit, negative=debit) |
| 3 | CURRENCY | String | 3 | Yes | ISO 4217 | Three-letter currency code (USD, EUR, GBP, etc.) |
| 4 | TX_TS_UTC | DateTime | - | Yes | ISO 8601 | Transaction timestamp in UTC (YYYY-MM-DDTHH:MM:SSZ) |
| 5 | TX_TYPE | String | 32 | Yes | Uppercase | Transaction type (DEPOSIT, WITHDRAWAL, TRANSFER, etc.) |

---

## Header Row

```
ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE
```

**Rules**:
- Header row must be the first line in the file
- Column names must match exactly (case-sensitive)
- Column order must match the specification above
- No extra columns allowed

---

## Data Validation Rules

### ACC_ID
- **Pattern**: Alphanumeric characters, hyphens, underscores allowed
- **Examples**: `ACC001`, `CUST-12345`, `ACCT_999`
- **Invalid**: Empty string, special characters beyond `-` and `_`, exceeds 32 chars

### AMOUNT
- **Range**: -1,000,000.00 to +1,000,000.00
- **Precision**: 2 decimal places (cents/pence)
- **Format**: Optional minus sign, digits, optional decimal point, digits
- **Examples**: `100.50`, `-25.00`, `1000`, `0.01`
- **Invalid**: Non-numeric, scientific notation, more than 2 decimal places

### CURRENCY
- **Pattern**: Exactly 3 uppercase letters
- **Standard**: ISO 4217 currency codes
- **Examples**: `USD`, `EUR`, `GBP`, `JPY`, `CAD`
- **Invalid**: Lowercase, numeric, not 3 characters, invalid codes (e.g., `XXX`)

### TX_TS_UTC
- **Format**: ISO 8601 datetime with UTC indicator
- **Pattern**: `YYYY-MM-DDTHH:MM:SSZ` or `YYYY-MM-DD HH:MM:SS`
- **Examples**: `2026-01-28T10:30:00Z`, `2026-01-28 10:30:00`
- **Invalid**: Future-dated beyond 1 hour, invalid date (e.g., 2026-02-30), missing time component

### TX_TYPE
- **Pattern**: Uppercase alphanumeric string
- **Common Values**: `DEPOSIT`, `WITHDRAWAL`, `TRANSFER`, `FEE`, `INTEREST`, `ADJUSTMENT`
- **Examples**: `DEPOSIT`, `TRANSFER`, `ATM_WITHDRAWAL`
- **Invalid**: Empty string, lowercase, exceeds 32 characters

---

## Example File

```csv
ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE
ACC001,100.50,USD,2026-01-28T10:30:00Z,DEPOSIT
ACC002,-25.00,USD,2026-01-28T10:35:00Z,WITHDRAWAL
ACC003,500.00,EUR,2026-01-28T10:40:00Z,TRANSFER
ACC001,75.25,USD,2026-01-28T10:45:00Z,DEPOSIT
ACC004,-150.00,GBP,2026-01-28T10:50:00Z,ATM_WITHDRAWAL
ACC005,1000.00,USD,2026-01-28T10:55:00Z,WIRE_TRANSFER
ACC002,50.00,USD,2026-01-28T11:00:00Z,DEPOSIT
ACC006,-5.00,EUR,2026-01-28T11:05:00Z,FEE
```

---

## Error Handling

### Invalid Row Processing

When the TX_INBOUND program encounters an invalid row:

1. **Validation Failure**: Row fails one or more validation rules
2. **Error Logging**: Error message written to stderr with row number and reason
3. **Row Skip**: Invalid row is skipped (not inserted into database)
4. **Continue Processing**: Subsequent rows are still processed (non-fatal error)
5. **Summary Report**: At end of processing, report count of valid vs. invalid rows

### Example Error Messages

```
ERROR: Row 3 - Invalid AMOUNT format: "abc.50" (must be numeric)
ERROR: Row 5 - Invalid CURRENCY: "US" (must be 3 characters)
ERROR: Row 7 - Amount out of range: "2000000.00" (max 1000000.00)
ERROR: Row 9 - ACC_ID exceeds max length: "ACCOUNTIDENTIFIER1234567890123" (max 32)
WARNING: Row 11 - Future-dated transaction: "2026-02-28T10:30:00Z" (beyond 1 hour tolerance)
```

---

## Edge Cases

### Empty File
- **Scenario**: CSV file contains only header row (no data rows)
- **Behavior**: Program completes successfully with 0 rows processed
- **Lineage**: No lineage event generated (no transformation occurred)

### Duplicate Transactions
- **Scenario**: Two rows with identical data (potential duplicate)
- **Behavior**: Both rows inserted (no duplicate detection in v1)
- **Note**: Database allows duplicates; deduplication is not in scope

### Large Files
- **Scenario**: CSV file with 10,000+ rows
- **Behavior**: Program processes all rows sequentially
- **Performance**: Expected processing time ~1-5 minutes for 10,000 rows
- **Note**: No streaming; entire file processed in single transaction

### Special Characters
- **Scenario**: ACC_ID contains comma (e.g., "ACCT,001")
- **Behavior**: Field must be quoted: `"ACCT,001",100.00,USD,...`
- **COBOL Parsing**: UNSTRING handles quoted fields correctly

### Unicode Characters
- **Scenario**: ACC_ID contains non-ASCII characters (e.g., "ACCÖÜNT")
- **Behavior**: UTF-8 encoding preserved if COBOL compiler supports UTF-8
- **Note**: Recommendation is to use ASCII-only for maximum compatibility

---

## Processing Workflow

1. **File Placement**: User/system places transactions.csv in `/data/in/`
2. **Program Execution**: TX_INBOUND.cbl is executed (manually or via test runner)
3. **File Opening**: COBOL opens file for sequential read
4. **Header Validation**: First line read and validated against expected header
5. **Row Iteration**: Each subsequent line parsed and validated
6. **Database Insert**: Valid rows inserted into dbo.Transactions (SQL Server)
7. **Lineage Logging**: One lineage event per batch with transform_kind=ingest
8. **File Closing**: CSV file closed after processing
9. **Summary Report**: Stdout shows count of rows processed (valid/invalid)

---

## Lineage Metadata

For each successful batch ingestion, TX_INBOUND generates one lineage event:

- **program**: `TX_INBOUND`
- **src_engine**: `csv`
- **src_schema**: `filesystem`
- **src_table**: `transactions.csv`
- **src_cols**: `ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE`
- **tgt_engine**: `sqlserver`
- **tgt_schema**: `dbo`
- **tgt_table**: `Transactions`
- **tgt_cols**: `TX_ID,ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE`
- **transform_kind**: `ingest`
- **transform_expr**: `CSV file parse and validate; insert valid rows`
- **commit_sha**: (from .version file)
- **run_id**: (unique per execution, e.g., `run-2026-01-28-103045`)
- **ts_utc**: (timestamp when ingestion completed)

---

## Testing Recommendations

### Test Case 1: Valid Transactions
- **File**: 5 rows, all valid data
- **Expected**: All 5 rows inserted into Transactions table
- **Validation**: Query SQL Server to confirm count and data accuracy

### Test Case 2: Mixed Valid/Invalid
- **File**: 10 rows, 3 invalid (bad currency, out-of-range amount, invalid date)
- **Expected**: 7 valid rows inserted, 3 error messages logged
- **Validation**: Verify 7 rows in database, stderr shows 3 errors

### Test Case 3: Edge Case - Empty File
- **File**: Header only, no data rows
- **Expected**: Program completes, 0 rows processed, no errors
- **Validation**: No new rows in Transactions table

### Test Case 4: Edge Case - Large File
- **File**: 1,000 rows, all valid
- **Expected**: All rows inserted, processing completes in <2 minutes
- **Validation**: Row count in database matches file row count (excluding header)

### Test Case 5: Special Characters
- **File**: Rows with quoted fields, commas in ACC_ID
- **Expected**: Quoted fields parsed correctly, data preserved
- **Validation**: ACC_ID in database matches quoted value from CSV

---

**Contract Version**: 1.0  
**Last Updated**: 2026-01-28  
**Owner**: COBOL Data Lineage Feature Team

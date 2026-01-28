# COBOL Banking Data Lineage System

**Project**: COBOL Banking Demo Application  
**Feature**: End-to-End Data Lineage Tracking  
**Version**: 1.0  
**Last Updated**: 2026-01-28

---

## Overview

A demonstration-ready COBOL banking application that processes retail transactions through a complete pipeline (CSV ingest → ledger posting → balance calculation → cross-database replication) while capturing comprehensive data lineage metadata at every transformation step.

The system showcases end-to-end traceability in legacy banking systems, spanning two AWS databases (SQL Server operational, PostgreSQL reporting) and exporting lineage events to CSV for governance tool ingestion.

---

## Table of Contents

1. [System Architecture](#system-architecture)
2. [Program Execution Order](#program-execution-order)
3. [Lineage Strategy](#lineage-strategy)
4. [File Structure](#file-structure)
5. [Quick Start](#quick-start)
6. [Data Flow](#data-flow)
7. [Troubleshooting](#troubleshooting)
8. [Development Guide](#development-guide)
9. [Testing Strategy](#testing-strategy)
10. [Additional Resources](#additional-resources)

---

## System Architecture

```
┌─────────────────┐
│  CSV Files      │
│  /data/in/      │
└────────┬────────┘
         │ TX_INBOUND.cbl (ingest)
         ▼
┌─────────────────────────────────────────────────┐
│  Amazon EC2 (Amazon Linux 2023)                 │
│  ┌──────────────────────────────────────────┐   │
│  │  COBOL Programs                          │   │
│  │  - TX_INBOUND.cbl                        │   │
│  │  - POST_LEDGER.cbl                       │   │
│  │  - BALANCE_RECALC.cbl                    │   │
│  │  - REPL_REPORTING.cbl                    │   │
│  │  - LINEAGE_EXPORT.cbl                    │   │
│  │                                          │   │
│  │  Copybooks:                              │   │
│  │  - DB-CONFIG.cpy (DSN constants)         │   │
│  │  - RECORD-DEFS.cpy (record layouts)      │   │
│  │  - LINEAGE-LOGGER.cpy (lineage events)   │   │
│  └──────────────────────────────────────────┘   │
└────┬────────────────────────────────────┬───────┘
     │ ODBC                               │ ODBC
     ▼                                    ▼
┌─────────────────┐              ┌─────────────────┐
│  RDS SQL Server │              │ Aurora PostgreSQL│
│  (cblr_ops)     │  Replication │  (cblr_report)   │
│  - Transactions │─────────────>│  - daily_snapshots│
│  - LedgerEntries│              │  - account_rollups│
│  - AccountBalances│            └─────────────────┘
│  - LineageEvents│
│  - PostingAudit │
└────────┬────────┘
         │ LINEAGE_EXPORT.cbl
         ▼
┌─────────────────┐
│  Lineage CSV    │
│  /lineage/out/  │
│  lineage.csv    │
└─────────────────┘
         │
         ▼
┌─────────────────┐
│  OpenMetadata   │
│  (or other      │
│   governance)   │
└─────────────────┘
```

### Technology Stack

- **COBOL**: COBOL-85 or later (Micro Focus Visual COBOL preferred, GnuCOBOL fallback)
- **Database Connectivity**: unixODBC with dual DSN configuration
  - `SQLSRV_CBLR`: SQL Server operational database (ODBC Driver 18 for SQL Server)
  - `PG_CBLR`: PostgreSQL reporting database (PostgreSQL Unicode driver)
- **Operational Database**: Amazon RDS SQL Server (database: `cblr_ops`)
- **Reporting Database**: Amazon Aurora PostgreSQL (database: `cblr_report`)
- **Platform**: Amazon EC2 running Amazon Linux 2023
- **Version Control**: Git (commit SHA tracked in `.version` file)

---

## Program Execution Order

**CRITICAL**: Programs must be executed in strict sequential order to maintain data integrity.

| Step | Program | Purpose | Transform Kind | Input | Output |
|------|---------|---------|----------------|-------|--------|
| 1 | `TX_INBOUND.cbl` | Ingest CSV transactions | `ingest` | CSV file | `dbo.Transactions` |
| 2 | `POST_LEDGER.cbl` | Post double-entry ledger | `posting` | `dbo.Transactions` | `dbo.LedgerEntries` |
| 3 | `BALANCE_RECALC.cbl` | Calculate balances | `aggregate` | `dbo.LedgerEntries` | `dbo.AccountBalances` |
| 4 | `REPL_REPORTING.cbl` | Replicate to warehouse | `replication` | `dbo.vw_*` views | `public.*` tables |
| 5 | `LINEAGE_EXPORT.cbl` | Export lineage metadata | - | `dbo.LineageEvents` | `lineage.csv` |

### Execution Commands

```bash
# Option 1: Run individual programs
./cobol/src/TX_INBOUND
./cobol/src/POST_LEDGER
./cobol/src/BALANCE_RECALC
./cobol/src/REPL_REPORTING
./cobol/src/LINEAGE_EXPORT

# Option 2: Run test suite
bash tests/01_seed_sqlserver.sh  # Includes TX_INBOUND
bash tests/02_post_ledger.sh
bash tests/03_balance.sh
bash tests/04_repl.sh
bash tests/05_export_lineage.sh
```

**Dependencies**:
- Step 2 requires Step 1 completion (Transactions must exist)
- Step 3 requires Step 2 completion (LedgerEntries must exist)
- Step 4 requires Step 3 completion (AccountBalances must exist)
- Step 5 requires Steps 1-4 completion (LineageEvents must be populated)

---

## Lineage Strategy

This system implements a **dual-track lineage approach** combining static code annotations with runtime event capture.

### 1. Static Lineage (Code-Based)

Embedded in COBOL source files as machine-parseable comments.

**Grammar**:
```cobol
*> LINEAGE: PROGRAM=<program_name>
*> LINEAGE: SRC=<engine>.<schema>.<table>(<columns>)
*> LINEAGE: TGT=<engine>.<schema>.<table>(<columns>)
*> LINEAGE: MAP=<source_col> -> <target_col> [<transformation>]
*> LINEAGE: REF=<commit_sha>
```

**Example** (from POST_LEDGER.cbl):
```cobol
*> LINEAGE: PROGRAM=POST_LEDGER
*> LINEAGE: SRC=sqlserver.dbo.Transactions(ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC)
*> LINEAGE: TGT=sqlserver.dbo.LedgerEntries(ACC_ID,DEBIT,CREDIT,CURRENCY,POSTED_TS_UTC)
*> LINEAGE: MAP=AMOUNT -> DEBIT|CREDIT [IF AMOUNT < 0 THEN DEBIT ELSE CREDIT]
*> LINEAGE: MAP=TX_TS_UTC -> POSTED_TS_UTC [COPY]
*> LINEAGE: REF={git_commit_sha}
```

**Benefits**:
- ✅ Survives code deployment (always in sync with source)
- ✅ Parseable by static analysis tools
- ✅ Visible during code review
- ✅ Version-controlled alongside logic

### 2. Runtime Lineage (Event-Based)

Captured in `dbo.LineageEvents` table during program execution.

**Event Schema**:
| Column | Example | Description |
|--------|---------|-------------|
| `PROGRAM` | `POST_LEDGER` | COBOL program name |
| `SRC_ENGINE` | `sqlserver` | Source database type |
| `SRC_SCHEMA` | `dbo` | Source schema name |
| `SRC_TABLE` | `Transactions` | Source table name |
| `SRC_COLS` | `ACC_ID,AMOUNT,CURRENCY` | Source columns (comma-separated) |
| `TGT_ENGINE` | `sqlserver` | Target database type |
| `TGT_SCHEMA` | `dbo` | Target schema name |
| `TGT_TABLE` | `LedgerEntries` | Target table name |
| `TGT_COLS` | `ACC_ID,DEBIT,CREDIT` | Target columns (comma-separated) |
| `TRANSFORM_KIND` | `posting` | Type: `ingest`, `posting`, `aggregate`, `replication` |
| `TRANSFORM_EXPR` | `IF AMOUNT<0 DEBIT ELSE CREDIT` | Transformation logic |
| `COMMIT_SHA` | `9e7a0644a0d5...` | Git commit SHA from `.version` file |
| `RUN_ID` | `POST_LEDGER_20260128_103045` | Unique execution identifier |
| `TS_UTC` | `2026-01-28 10:30:45.123` | Event timestamp (UTC) |

**Benefits**:
- ✅ Captures actual runtime behavior
- ✅ Queryable for impact analysis
- ✅ Supports governance tools (OpenMetadata, Collibra, etc.)
- ✅ Chronological ordering for lineage graphs

### 3. Lineage CSV Export

Runtime lineage events are exported to `lineage/out/lineage.csv` in a format compatible with OpenMetadata and other governance platforms.

**CSV Columns** (14 total):
```csv
program,src_engine,src_schema,src_table,src_cols,tgt_engine,tgt_schema,tgt_table,tgt_cols,transform_kind,transform_expr,commit_sha,run_id,ts_utc
```

**Example Row**:
```csv
POST_LEDGER,sqlserver,dbo,Transactions,"ACC_ID,AMOUNT",sqlserver,dbo,LedgerEntries,"ACC_ID,DEBIT,CREDIT",posting,"IF AMOUNT<0 THEN DEBIT ELSE CREDIT",9e7a0644a0d5,POST_LEDGER_20260128_103045,2026-01-28 10:30:45
```

**Ingestion Workflow**:
1. Run `LINEAGE_EXPORT.cbl` after processing pipeline completes
2. Upload `lineage.csv` to governance tool's ingestion endpoint
3. Governance tool parses CSV and constructs lineage graph
4. End-to-end lineage from CSV → SQL Server → PostgreSQL visible in UI

---

## File Structure

```
cobol-banking/
├── .version                      # Git commit SHA (auto-generated)
├── .gitignore                    # Excludes credentials, logs, outputs
├── cobol/
│   ├── src/                      # COBOL programs
│   │   ├── TX_INBOUND.cbl        # CSV ingestion
│   │   ├── POST_LEDGER.cbl       # Double-entry posting
│   │   ├── BALANCE_RECALC.cbl    # Balance aggregation
│   │   ├── REPL_REPORTING.cbl    # Cross-DB replication
│   │   └── LINEAGE_EXPORT.cbl    # Lineage CSV export
│   └── copybooks/                # Shared code libraries
│       ├── DB-CONFIG.cpy         # DSN constants, connection flags
│       ├── RECORD-DEFS.cpy       # Record layouts for all tables
│       └── LINEAGE-LOGGER.cpy    # ADD-LINEAGE-EVENT section
├── config/
│   ├── odbc.ini.template         # ODBC DSN configuration template
│   └── odbcinst.ini.template     # ODBC driver configuration template
├── sql/
│   ├── sqlserver/
│   │   └── ddl.sql               # Operational DB schema (5 tables, 2 views)
│   └── postgres/
│       └── ddl.sql               # Reporting DB schema (2 tables, 1 mat view)
├── data/
│   └── in/
│       └── transactions.csv      # Sample input data (8 transactions)
├── lineage/
│   └── out/
│       ├── .gitkeep              # Ensures directory exists in git
│       └── lineage.csv           # Output (generated by LINEAGE_EXPORT)
├── tests/
│   ├── 01_seed_sqlserver.sh      # DDL + seed data
│   ├── 02_post_ledger.sh         # Run POST_LEDGER
│   ├── 03_balance.sh             # Run BALANCE_RECALC
│   ├── 04_repl.sh                # Run REPL_REPORTING
│   └── 05_export_lineage.sh      # Run LINEAGE_EXPORT
├── docs/
│   └── README.md                 # This file
└── specs/
    └── 001-cobol-data-lineage/   # Design artifacts
        ├── spec.md               # Feature specification
        ├── plan.md               # Implementation plan
        ├── research.md           # Technical research
        ├── data-model.md         # Entity definitions
        ├── quickstart.md         # Setup guide
        └── contracts/            # Database/CSV schemas
```

---

## Quick Start

See [specs/001-cobol-data-lineage/quickstart.md](../specs/001-cobol-data-lineage/quickstart.md) for detailed setup instructions.

**TL;DR** (5-minute demo):
```bash
# 1. Clone and setup
git clone <repo> && cd cobol-banking
git rev-parse HEAD > .version

# 2. Configure ODBC (edit with your RDS/Aurora endpoints)
sudo vi /etc/odbc.ini  # Add SQLSRV_CBLR and PG_CBLR DSNs

# 3. Initialize databases
isql -v SQLSRV_CBLR < sql/sqlserver/ddl.sql
isql -v PG_CBLR < sql/postgres/ddl.sql

# 4. Compile COBOL programs
cd cobol/src
for prog in TX_INBOUND POST_LEDGER BALANCE_RECALC REPL_REPORTING LINEAGE_EXPORT; do
    cob -x ${prog}.cbl -o ${prog}
done
cd ../..

# 5. Run demo pipeline
bash tests/01_seed_sqlserver.sh
bash tests/02_post_ledger.sh
bash tests/03_balance.sh
bash tests/04_repl.sh
bash tests/05_export_lineage.sh

# 6. View results
cat lineage/out/lineage.csv
```

---

## Running on Windows with WSL (Ubuntu)

This guide shows how to run the COBOL banking application on Windows using Windows Subsystem for Linux (WSL) with Ubuntu. This is the recommended approach for Windows developers as it provides a native Linux environment with easy access to GnuCOBOL and ODBC drivers.

### Prerequisites

- Windows 10 version 2004+ or Windows 11
- Administrator access to install WSL
- At least 4GB free disk space

### Step 1: Install WSL with Ubuntu

Open PowerShell as Administrator and run:

```powershell
# Install WSL with Ubuntu (default distribution)
wsl --install

# Restart your computer when prompted
```

After restart, Ubuntu will automatically launch and prompt you to create a username and password.

**Alternatively**, if WSL is already installed:

```powershell
# Install Ubuntu specifically
wsl --install -d Ubuntu

# List available distributions
wsl --list --online
```

### Step 2: Access Your Project in WSL

Open Ubuntu from the Start menu or run `wsl` in PowerShell, then:

```bash
# Navigate to your Windows project directory
# Windows drives are mounted at /mnt/<drive-letter>
cd /mnt/c/EPAM/cobol-banking/cobol-banking

# Verify you're in the right directory
pwd
ls -la
```

### Step 3: Install GnuCOBOL and Dependencies

```bash
# Update package lists
sudo apt update

# Install GnuCOBOL and required dependencies
sudo apt install -y gnucobol3 libcob4 gcc make

# Install build tools
sudo apt install -y build-essential pkg-config

# Verify installation
cobc --version
# Expected output: cobc (GnuCOBOL) 3.x

# Check compiler info
cobc --info
```

### Step 4: Install ODBC Drivers

#### Install unixODBC

```bash
# Install unixODBC and development files
sudo apt install -y unixodbc unixodbc-dev odbcinst

# Verify installation
odbcinst -j
# Shows config file locations
```

#### Install SQL Server ODBC Driver

```bash
# Add Microsoft repository
curl https://packages.microsoft.com/keys/microsoft.asc | sudo apt-key add -
curl https://packages.microsoft.com/config/ubuntu/$(lsb_release -rs)/prod.list | sudo tee /etc/apt/sources.list.d/mssql-release.list

# Update and install
sudo apt update
sudo ACCEPT_EULA=Y apt install -y msodbcsql18

# Verify installation
odbcinst -q -d | grep -i "SQL Server"
```

#### Install PostgreSQL ODBC Driver

```bash
# Install PostgreSQL ODBC driver
sudo apt install -y odbc-postgresql

# Verify installation
odbcinst -q -d | grep -i postgres
```

### Step 5: Configure ODBC DSNs

Create ODBC configuration file:

```bash
# Copy templates to /etc (system-wide) or ~/.odbc.ini (user-specific)
# For development, user-specific is recommended

# Copy and edit the template
cp config/odbc.ini.template ~/.odbc.ini

# Edit with your database endpoints
nano ~/.odbc.ini
```

Update the file with your RDS/Aurora endpoints:

```ini
[SQLSRV_CBLR]
Description         = COBOL Banking Operational Database (SQL Server)
Driver              = ODBC Driver 18 for SQL Server
Server              = your-rds-instance.xxxxx.us-east-1.rds.amazonaws.com
Port                = 1433
Database            = cblr_ops
UID                 = your_username
PWD                 = your_password
TrustServerCertificate = yes
Encrypt             = yes

[PG_CBLR]
Description         = COBOL Banking Reporting Database (PostgreSQL)
Driver              = PostgreSQL Unicode
Server              = your-aurora-instance.cluster-xxxxx.us-east-1.rds.amazonaws.com
Port                = 5432
Database            = cblr_report
UID                 = your_username
PWD                 = your_password
SSLMode             = require
```

Save with `Ctrl+O`, `Enter`, `Ctrl+X`.

**Test ODBC connections:**

```bash
# Test SQL Server connection
isql -v SQLSRV_CBLR

# Test PostgreSQL connection
isql -v PG_CBLR

# If successful, you'll see a SQL> prompt
# Type 'quit' to exit
```

### Step 6: Initialize Databases

```bash
# Ensure you're in the project directory
cd /mnt/c/EPAM/cobol-banking/cobol-banking

# Initialize SQL Server schema
isql -v SQLSRV_CBLR < sql/sqlserver/ddl.sql

# Initialize PostgreSQL schema
isql -v PG_CBLR < sql/postgres/ddl.sql

# Verify tables were created
echo "SELECT name FROM sys.tables;" | isql -v SQLSRV_CBLR -b
echo "SELECT tablename FROM pg_tables WHERE schemaname='public';" | isql -v PG_CBLR -b
```

### Step 7: Generate Git Version File

```bash
# Generate .version file with current commit SHA
git rev-parse HEAD > .version

# Verify
cat .version
```

### Step 8: Compile COBOL Programs

GnuCOBOL requires special handling for embedded SQL. For this demo, we'll compile without SQL preprocessing first:

```bash
# Navigate to source directory
cd cobol/src

# Set copybook path
export COB_COPY_DIR=../copybooks

# Compile each program
# Note: Production deployment would use SQL preprocessor (ESQL)
# For demo purposes, compile with warnings suppressed

cobc -x -free -std=cobol85 TX_INBOUND.cbl -o TX_INBOUND 2>&1 | grep -v "warning:"
cobc -x -free -std=cobol85 POST_LEDGER.cbl -o POST_LEDGER 2>&1 | grep -v "warning:"
cobc -x -free -std=cobol85 BALANCE_RECALC.cbl -o BALANCE_RECALC 2>&1 | grep -v "warning:"
cobc -x -free -std=cobol85 REPL_REPORTING.cbl -o REPL_REPORTING 2>&1 | grep -v "warning:"
cobc -x -free -std=cobol85 LINEAGE_EXPORT.cbl -o LINEAGE_EXPORT 2>&1 | grep -v "warning:"

# Return to project root
cd ../..

# Verify executables were created
ls -lh cobol/src/TX_INBOUND cobol/src/POST_LEDGER cobol/src/BALANCE_RECALC cobol/src/REPL_REPORTING cobol/src/LINEAGE_EXPORT
```

**Note**: The COBOL programs contain embedded SQL (`EXEC SQL`) which requires a SQL preprocessor. For production:
- Use `esqlcob` (ESQL/COBOL preprocessor) with GnuCOBOL
- Or use Micro Focus Visual COBOL which has built-in SQL preprocessing

For this demo, the programs compile but won't execute SQL statements without preprocessing.

### Step 9: Run the Application Pipeline

Make test scripts executable:

```bash
# Make all test scripts executable
chmod +x tests/*.sh

# Verify
ls -l tests/
```

Run the pipeline in sequence:

```bash
# 1. Seed database with sample data
bash tests/01_seed_sqlserver.sh

# 2. Post transactions to ledger
bash tests/02_post_ledger.sh

# 3. Calculate account balances
bash tests/03_balance.sh

# 4. Replicate to reporting database
bash tests/04_repl.sh

# 5. Export lineage metadata
bash tests/05_export_lineage.sh
```

### Step 10: Verify Results

```bash
# Check SQL Server data
echo "SELECT COUNT(*) AS transaction_count FROM dbo.Transactions;" | isql -v SQLSRV_CBLR -b
echo "SELECT COUNT(*) AS ledger_count FROM dbo.LedgerEntries;" | isql -v SQLSRV_CBLR -b
echo "SELECT * FROM dbo.AccountBalances;" | isql -v SQLSRV_CBLR -b

# Check PostgreSQL data
echo "SELECT COUNT(*) AS snapshot_count FROM public.daily_snapshots;" | isql -v PG_CBLR -b
echo "SELECT * FROM public.account_rollups;" | isql -v PG_CBLR -b

# View lineage export
cat lineage/out/lineage.csv

# View with column headers
head -20 lineage/out/lineage.csv | column -t -s ','
```

### WSL Tips and Troubleshooting

#### Accessing Files Between Windows and WSL

```bash
# From WSL, access Windows files at /mnt/<drive>
cd /mnt/c/Users/YourName/Documents

# From Windows, access WSL files at \\wsl$\Ubuntu\home\username
# Example in Explorer: \\wsl$\Ubuntu\home\yourname\cobol-banking
```

#### Permission Issues

If you encounter permission errors:

```bash
# Fix file permissions (if files were created in Windows)
chmod +x tests/*.sh
chmod 644 cobol/src/*.cbl
chmod 644 cobol/copybooks/*.cpy

# Fix line endings (convert CRLF to LF)
sudo apt install -y dos2unix
dos2unix tests/*.sh
dos2unix cobol/src/*.cbl
dos2unix cobol/copybooks/*.cpy
```

#### ODBC Connection Issues

```bash
# Check ODBC driver installation
odbcinst -q -d

# Test driver loading
isql -v SQLSRV_CBLR <<< "SELECT 1;"

# Check detailed connection info
odbcinst -j

# Enable ODBC tracing (for debugging)
cat >> ~/.odbc.ini << EOF

[ODBC]
Trace=Yes
TraceFile=/tmp/odbc.log
EOF
```

#### Memory or Performance Issues

```bash
# Increase WSL memory limit
# Create/edit .wslconfig in Windows user directory:
notepad.exe ~/.wslconfig

# Add:
# [wsl2]
# memory=4GB
# processors=2

# Restart WSL
wsl --shutdown
# Then reopen Ubuntu
```

#### Working with SQL Preprocessing

For production use with embedded SQL:

```bash
# Install ESQL preprocessor for COBOL
# Option 1: Use cobpp (COBOL preprocessor)
sudo apt install -y libecpg-dev  # PostgreSQL embedded SQL

# Option 2: Use Pro*COBOL for Oracle (if needed)
# Option 3: Use Micro Focus Visual COBOL (commercial)

# Preprocess and compile example:
# esqlcob -o TX_INBOUND.cob TX_INBOUND.cbl
# cobc -x -free TX_INBOUND.cob -o TX_INBOUND
```

### Quick WSL Development Workflow

```bash
# 1. Start WSL from Windows Terminal or PowerShell
wsl

# 2. Navigate to project
cd /mnt/c/EPAM/cobol-banking/cobol-banking

# 3. Make code changes in Windows using VS Code or any editor
# Files auto-sync with WSL

# 4. Compile in WSL
cd cobol/src
cobc -x -free PROGRAM.cbl -o PROGRAM

# 5. Run tests
cd ../..
bash tests/0X_test.sh

# 6. View results
cat lineage/out/lineage.csv
```

### Cleanup

```bash
# Truncate database tables
echo "TRUNCATE TABLE dbo.LineageEvents; TRUNCATE TABLE dbo.PostingAudit; DELETE FROM dbo.LedgerEntries; DELETE FROM dbo.Transactions;" | isql -v SQLSRV_CBLR -b
echo "TRUNCATE TABLE public.account_rollups; TRUNCATE TABLE public.daily_snapshots;" | isql -v PG_CBLR -b

# Remove compiled binaries
rm -f cobol/src/TX_INBOUND cobol/src/POST_LEDGER cobol/src/BALANCE_RECALC cobol/src/REPL_REPORTING cobol/src/LINEAGE_EXPORT

# Remove output files
rm -f lineage/out/lineage.csv
```

---

## Data Flow

### Step 1: CSV Ingestion (TX_INBOUND)

**Input**: `data/in/transactions.csv`
```csv
ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE
ACC001,1500.00,USD,2024-01-15 09:30:00,DEPOSIT
ACC001,-45.50,USD,2024-01-15 14:22:00,WITHDRAWAL
```

**Output**: `dbo.Transactions` table
```sql
TX_ID | ACC_ID | AMOUNT  | CURRENCY | TX_TS_UTC           | TX_TYPE
------|--------|---------|----------|---------------------|----------
1     | ACC001 | 1500.00 | USD      | 2024-01-15 09:30:00 | DEPOSIT
2     | ACC001 | -45.50  | USD      | 2024-01-15 14:22:00 | WITHDRAWAL
```

**Lineage Event**: `transform_kind=ingest`, `src_table=CSV_FILE`, `tgt_table=Transactions`

---

### Step 2: Ledger Posting (POST_LEDGER)

**Logic**: Double-entry bookkeeping
- If `AMOUNT >= 0`: Create CREDIT entry
- If `AMOUNT < 0`: Create DEBIT entry (absolute value)

**Output**: `dbo.LedgerEntries` table
```sql
ENTRY_ID | TX_ID | ACC_ID | DEBIT | CREDIT  | CURRENCY
---------|-------|--------|-------|---------|----------
1        | 1     | ACC001 | NULL  | 1500.00 | USD
2        | 2     | ACC001 | 45.50 | NULL    | USD
```

**Lineage Event**: `transform_kind=posting`, `transform_expr=IF AMOUNT<0 THEN DEBIT ELSE CREDIT`

---

### Step 3: Balance Calculation (BALANCE_RECALC)

**Logic**: Aggregate by account and currency
```sql
BALANCE = SUM(CREDIT) - SUM(DEBIT)
```

**Output**: `dbo.AccountBalances` table
```sql
ACC_ID | CURRENCY | BALANCE  | AS_OF_UTC
-------|----------|----------|-------------------
ACC001 | USD      | 1454.50  | 2024-01-17 10:00:00
```

**Lineage Event**: `transform_kind=aggregate`, `transform_expr=SUM(CREDIT)-SUM(DEBIT) GROUP BY ACC_ID,CURRENCY`

---

### Step 4: Cross-Database Replication (REPL_REPORTING)

**Source**: `dbo.vw_DailyBalances` (SQL Server view)  
**Target**: `public.daily_snapshots` (PostgreSQL table)

**Logic**: UPSERT (INSERT with ON CONFLICT UPDATE)

**Output**: `public.daily_snapshots` table
```sql
snap_date  | acc_id | end_balance | currency
-----------|--------|-------------|----------
2024-01-17 | ACC001 | 1454.50     | USD
```

**Lineage Event**: `transform_kind=replication`, `src_engine=sqlserver`, `tgt_engine=postgres`

---

### Step 5: Lineage Export (LINEAGE_EXPORT)

**Output**: `lineage/out/lineage.csv`
```csv
program,src_engine,src_schema,src_table,src_cols,tgt_engine,tgt_schema,tgt_table,tgt_cols,transform_kind,transform_expr,commit_sha,run_id,ts_utc
TX_INBOUND,csv,,transactions.csv,"ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",sqlserver,dbo,Transactions,"ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE",ingest,UNSTRING CSV FIELDS,9e7a0644,TX_INBOUND_20240117_100000,2024-01-17 10:00:00
POST_LEDGER,sqlserver,dbo,Transactions,"ACC_ID,AMOUNT",sqlserver,dbo,LedgerEntries,"ACC_ID,DEBIT,CREDIT",posting,IF AMOUNT<0 THEN DEBIT ELSE CREDIT,9e7a0644,POST_LEDGER_20240117_100100,2024-01-17 10:01:00
BALANCE_RECALC,sqlserver,dbo,LedgerEntries,"ACC_ID,CURRENCY,DEBIT,CREDIT",sqlserver,dbo,AccountBalances,"ACC_ID,CURRENCY,BALANCE",aggregate,SUM(CREDIT)-SUM(DEBIT) GROUP BY ACC_ID CURRENCY,9e7a0644,BALANCE_RECALC_20240117_100200,2024-01-17 10:02:00
REPL_REPORTING,sqlserver,dbo,AccountBalances,"ACC_ID,BALANCE,CURRENCY",postgres,public,daily_snapshots,"acc_id,end_balance,currency",replication,COPY WITH UPSERT,9e7a0644,REPL_REPORTING_20240117_100300,2024-01-17 10:03:00
```

---

## Troubleshooting

### Problem: ODBC Connection Failure

**Symptoms**:
```
[S1000][unixODBC][Driver Manager]Can't open lib 'ODBC Driver 18 for SQL Server' : file not found
```

**Solution**:
```bash
# 1. Verify drivers are installed
odbcinst -q -d

# 2. Check driver library paths
find /opt /usr -name "libmsodbcsql*.so" 2>/dev/null

# 3. Update /etc/odbcinst.ini with correct path
sudo vi /etc/odbcinst.ini

# 4. Test connection
isql -v SQLSRV_CBLR
```

---

### Problem: COBOL Compilation Errors

**Symptoms**:
```
Error: LINEAGE-LOGGER.cpy: No such file or directory
```

**Solution**:
```bash
# Set COBCPY environment variable
export COBCPY=$PWD/cobol/copybooks

# Or compile with explicit include path
cob -x -I./cobol/copybooks TX_INBOUND.cbl -o TX_INBOUND
```

---

### Problem: SQL Server Authentication Failure

**Symptoms**:
```
[28000][Microsoft][ODBC Driver 18 for SQL Server][SQL Server]Login failed for user 'sa'
```

**Solution**:
```bash
# 1. Verify RDS endpoint is correct
ping <rds-endpoint>.us-east-1.rds.amazonaws.com

# 2. Check security group allows port 1433 from EC2
aws ec2 describe-security-groups --group-ids <sg-id>

# 3. Test credentials manually
sqlcmd -S <rds-endpoint>,1433 -U <username> -P <password> -d cblr_ops

# 4. Verify SSL/TLS settings in odbc.ini
TrustServerCertificate=yes
Encrypt=yes
```

---

### Problem: Empty Lineage CSV

**Symptoms**:
```
lineage.csv created but has only header row
```

**Solution**:
```bash
# 1. Check if programs logged lineage events
echo "SELECT COUNT(*) FROM dbo.LineageEvents;" | isql -v SQLSRV_CBLR -b

# 2. Verify .version file exists
cat .version

# 3. Check LINEAGE_EXPORT program ran successfully
bash tests/05_export_lineage.sh

# 4. Review COBOL program source for ADD-LINEAGE-EVENT calls
grep -n "PERFORM ADD-LINEAGE-EVENT" cobol/src/*.cbl
```

---

### Problem: Duplicate Key Errors

**Symptoms**:
```
[23000][Microsoft][ODBC Driver 18 for SQL Server][SQL Server]Violation of PRIMARY KEY constraint
```

**Solution**:
```bash
# Truncate tables before re-running pipeline
echo "TRUNCATE TABLE dbo.LineageEvents; TRUNCATE TABLE dbo.PostingAudit; DELETE FROM dbo.LedgerEntries; DELETE FROM dbo.Transactions;" | isql -v SQLSRV_CBLR -b

# Or use IDENTITY_INSERT for controlled re-seeding
```

---

### Problem: CSV Parsing Failures

**Symptoms**:
```
TX_INBOUND: ERROR - Invalid AMOUNT format on row 3
```

**Solution**:
```bash
# 1. Check CSV encoding (must be UTF-8)
file data/in/transactions.csv

# 2. Verify line endings (LF or CRLF both supported)
dos2unix data/in/transactions.csv

# 3. Validate numeric fields have no extra spaces
sed 's/[[:space:]]*,/,/g' data/in/transactions.csv > data/in/transactions_clean.csv

# 4. Check currency codes are uppercase
awk -F, 'NR>1 {print $3}' data/in/transactions.csv | sort | uniq
```

---

## Development Guide

### Adding a New COBOL Program

1. **Create the program**: `cobol/src/NEW_PROGRAM.cbl`
2. **Add copybooks**: `COPY DB-CONFIG.`, `COPY RECORD-DEFS.`, `COPY LINEAGE-LOGGER.`
3. **Implement logic**: Follow existing program structure (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions)
4. **Add static lineage**: Include `*> LINEAGE:` comments before each transformation
5. **Log runtime lineage**: Call `PERFORM ADD-LINEAGE-EVENT` after each data operation
6. **Create test runner**: `tests/06_new_program.sh`
7. **Compile and test**: `cob -x NEW_PROGRAM.cbl -o NEW_PROGRAM && ./NEW_PROGRAM`

### Modifying Transformation Logic

1. **Update COBOL source**: Edit the transformation logic in the PROCEDURE DIVISION
2. **Update static lineage**: Modify `*> LINEAGE: MAP=` comments to reflect new logic
3. **Update runtime lineage**: Change `WS-LIN-TRANSFORM-EXPR` value before calling `ADD-LINEAGE-EVENT`
4. **Recompile**: `cob -x PROGRAM.cbl -o PROGRAM`
5. **Test**: Run test runner and verify lineage events reflect the change

### Adding New Database Tables

1. **Update DDL**: Add `CREATE TABLE` statement to `sql/sqlserver/ddl.sql` or `sql/postgres/ddl.sql`
2. **Update RECORD-DEFS.cpy**: Add new record layout in copybook
3. **Add indexes**: Include appropriate indexes for performance
4. **Re-initialize database**: `isql -v SQLSRV_CBLR < sql/sqlserver/ddl.sql`

---

## Testing Strategy

Per the project constitution, formal unit testing is **not required**. Testing focuses on demonstration execution and validation.

### Test Runner Scripts

Located in `tests/` directory:
- `01_seed_sqlserver.sh`: Initialize SQL Server schema and seed data
- `02_post_ledger.sh`: Execute POST_LEDGER program
- `03_balance.sh`: Execute BALANCE_RECALC program
- `04_repl.sh`: Execute REPL_REPORTING program
- `05_export_lineage.sh`: Execute LINEAGE_EXPORT program

### Validation Queries

**SQL Server**:
```sql
-- Verify transaction ingestion
SELECT COUNT(*) AS tx_count FROM dbo.Transactions;

-- Verify ledger posting
SELECT COUNT(*) AS entry_count FROM dbo.LedgerEntries;

-- Verify balance calculation
SELECT * FROM dbo.AccountBalances ORDER BY ACC_ID, CURRENCY;

-- Verify lineage capture
SELECT PROGRAM, TRANSFORM_KIND, COUNT(*) 
FROM dbo.LineageEvents 
GROUP BY PROGRAM, TRANSFORM_KIND;
```

**PostgreSQL**:
```sql
-- Verify replication
SELECT COUNT(*) AS snapshot_count FROM public.daily_snapshots;

-- View currency summary
SELECT * FROM public.vw_currency_summary;
```

### Edge Cases to Test

- Empty CSV file (header only)
- Invalid CSV rows (bad currency, out-of-range amount)
- Missing .version file (should default to "UNKNOWN")
- Idempotency (re-run programs with same data)
- Cross-database failure (SQL Server up, PostgreSQL down)

---

## Additional Resources

### Documentation

- **Feature Specification**: `specs/001-cobol-data-lineage/spec.md` - Requirements and success criteria
- **Implementation Plan**: `specs/001-cobol-data-lineage/plan.md` - Technical context and project structure
- **Research Findings**: `specs/001-cobol-data-lineage/research.md` - Key technical decisions
- **Data Model**: `specs/001-cobol-data-lineage/data-model.md` - Entity definitions and relationships
- **Quickstart Guide**: `specs/001-cobol-data-lineage/quickstart.md` - Step-by-step setup instructions

### Contracts

- **SQL Server DDL**: `specs/001-cobol-data-lineage/contracts/databases/sqlserver-ddl.sql`
- **PostgreSQL DDL**: `specs/001-cobol-data-lineage/contracts/databases/postgresql-ddl.sql`
- **CSV Input Format**: `specs/001-cobol-data-lineage/contracts/csv/transaction-format.md`
- **Lineage CSV Format**: `specs/001-cobol-data-lineage/contracts/lineage/lineage-csv-format.md`

### External References

- **Micro Focus Visual COBOL**: https://www.microfocus.com/products/visual-cobol/
- **GnuCOBOL**: https://gnucobol.sourceforge.io/
- **unixODBC**: http://www.unixodbc.org/
- **Microsoft ODBC Driver for SQL Server**: https://learn.microsoft.com/en-us/sql/connect/odbc/linux-mac/
- **PostgreSQL ODBC**: https://odbc.postgresql.org/
- **OpenMetadata**: https://open-metadata.org/

---

## License

This is a demonstration project. See repository LICENSE file for details.

---

## Maintainers

COBOL Data Lineage Feature Team  
**Version**: 1.0  
**Last Updated**: 2026-01-28

For questions or issues, please file a GitHub issue in the repository.

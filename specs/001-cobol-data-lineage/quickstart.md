# Quickstart Guide: COBOL Data Lineage Banking Component

**Feature**: COBOL Data Lineage Banking Component  
**Branch**: `001-cobol-data-lineage`  
**Date**: 2026-01-28

## Purpose

This guide provides step-by-step instructions for setting up and running the COBOL banking data lineage demonstration system on Amazon EC2 (Amazon Linux 2023).

---

## Prerequisites

Before starting, ensure you have:

✅ **AWS Resources Provisioned**:
- Amazon EC2 instance (Amazon Linux 2023, t3.medium or larger)
- Amazon RDS SQL Server instance (database: `cblr_ops`)
- Amazon Aurora PostgreSQL instance (database: `cblr_report`)
- Security groups configured for EC2→RDS and EC2→Aurora connectivity

✅ **Access & Credentials**:
- SSH access to EC2 instance
- Database credentials for SQL Server and PostgreSQL
- IAM role attached to EC2 instance (for RDS/Aurora access)

✅ **Local Workstation** (for setup):
- SSH client
- Git installed
- Text editor for configuration files

---

## Architecture Overview

```
┌─────────────────┐
│  CSV Files      │
│  /data/in/      │
└────────┬────────┘
         │
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
│  └──────────────────────────────────────────┘   │
└────┬────────────────────────────────────┬───────┘
     │                                    │
     ▼                                    ▼
┌─────────────────┐              ┌─────────────────┐
│  RDS SQL Server │              │ Aurora PostgreSQL│
│  (cblr_ops)     │              │  (cblr_report)   │
│  - Transactions │              │  - daily_snapshots│
│  - LedgerEntries│              │  - account_rollups│
│  - AccountBalances│            └─────────────────┘
│  - LineageEvents│
└─────────────────┘
         │
         ▼
┌─────────────────┐
│  Lineage CSV    │
│  /lineage/out/  │
└─────────────────┘
```

---

## Setup Steps

### Step 1: Connect to EC2 Instance

```bash
# SSH to your EC2 instance
ssh -i your-key.pem ec2-user@<ec2-public-ip>

# Update system packages
sudo yum update -y
```

### Step 2: Install COBOL Compiler and Dependencies

**Option A: Micro Focus Visual COBOL (Preferred)**

```bash
# Download Micro Focus Visual COBOL Developer Edition for Linux
# (Follow Micro Focus licensing and download instructions)

# Install prerequisite libraries
sudo yum install -y gcc glibc-devel

# Install Visual COBOL (example path - adjust for your download)
sudo sh VisualCOBOL_installer.sh

# Verify installation
cob --version
```

**Option B: GnuCOBOL (Open Source Alternative)**

```bash
# Install GnuCOBOL
sudo yum install -y gcc make gmp-devel ncurses-devel db4-devel

# Download and build GnuCOBOL 3.x
wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2.tar.gz
tar -xzf gnucobol-3.2.tar.gz
cd gnucobol-3.2
./configure
make
sudo make install

# Verify installation
cobc --version
```

### Step 3: Install ODBC Drivers

```bash
# Install unixODBC
sudo yum install -y unixODBC unixODBC-devel

# Install Microsoft ODBC Driver for SQL Server
curl https://packages.microsoft.com/config/rhel/8/prod.repo | sudo tee /etc/yum.repos.d/mssql-release.repo
sudo yum remove -y unixODBC-utf16 unixODBC-utf16-devel
sudo ACCEPT_EULA=Y yum install -y msodbcsql18

# Install PostgreSQL ODBC driver
sudo yum install -y postgresql-odbc

# Verify drivers
odbcinst -q -d
```

Expected output:
```
[ODBC Driver 18 for SQL Server]
[PostgreSQL]
```

### Step 4: Clone Repository and Configure

```bash
# Clone the repository
cd /home/ec2-user
git clone <repository-url> cobol-banking
cd cobol-banking

# Create .version file with current commit
git rev-parse HEAD > .version

# Create output directories
mkdir -p lineage/out
mkdir -p data/in
```

### Step 5: Configure ODBC DSNs

```bash
# Edit /etc/odbc.ini (system-wide DSNs)
sudo vi /etc/odbc.ini
```

Add the following configuration (replace `<placeholders>` with actual values):

```ini
[SQLSRV_CBLR]
Driver=ODBC Driver 18 for SQL Server
Server=<rds-endpoint>.us-east-1.rds.amazonaws.com,1433
Database=cblr_ops
UID=<sql-username>
PWD=<sql-password>
Encrypt=yes
TrustServerCertificate=no

[PG_CBLR]
Driver=PostgreSQL
Server=<aurora-endpoint>.us-east-1.rds.amazonaws.com
Database=cblr_report
Port=5432
UID=<postgres-username>
PWD=<postgres-password>
SSLMode=require
```

```bash
# Secure the file
sudo chmod 600 /etc/odbc.ini

# Test connectivity
isql -v SQLSRV_CBLR
isql -v PG_CBLR
```

If connections succeed, you'll see a SQL prompt. Type `quit` to exit.

### Step 6: Initialize Databases

```bash
# Initialize SQL Server schema
isql -v SQLSRV_CBLR < sql/sqlserver/ddl.sql

# Initialize PostgreSQL schema
isql -v PG_CBLR < sql/postgres/ddl.sql

# Verify tables created
# SQL Server
echo "SELECT name FROM sys.tables;" | isql -v SQLSRV_CBLR -b

# PostgreSQL
echo "SELECT tablename FROM pg_tables WHERE schemaname='public';" | isql -v PG_CBLR -b
```

Expected tables:
- **SQL Server**: Transactions, LedgerEntries, AccountBalances, LineageEvents, PostingAudit
- **PostgreSQL**: daily_snapshots, account_rollups

---

## Running the Demo

### Step 1: Prepare Sample Data

```bash
# Create sample transactions CSV
cat > data/in/transactions.csv <<EOF
ACC_ID,AMOUNT,CURRENCY,TX_TS_UTC,TX_TYPE
ACC001,100.50,USD,2026-01-28T10:30:00Z,DEPOSIT
ACC002,-25.00,USD,2026-01-28T10:35:00Z,WITHDRAWAL
ACC003,500.00,EUR,2026-01-28T10:40:00Z,TRANSFER
ACC001,75.25,USD,2026-01-28T10:45:00Z,DEPOSIT
ACC004,-150.00,GBP,2026-01-28T10:50:00Z,WITHDRAWAL
EOF

# Verify file created
cat data/in/transactions.csv
```

### Step 2: Compile COBOL Programs

**For Micro Focus Visual COBOL:**

```bash
cd cobol/src

# Compile each program
cob -x TX_INBOUND.cbl -o ../../bin/TX_INBOUND
cob -x POST_LEDGER.cbl -o ../../bin/POST_LEDGER
cob -x BALANCE_RECALC.cbl -o ../../bin/BALANCE_RECALC
cob -x REPL_REPORTING.cbl -o ../../bin/REPL_REPORTING
cob -x LINEAGE_EXPORT.cbl -o ../../bin/LINEAGE_EXPORT

cd ../..
```

**For GnuCOBOL:**

```bash
cd cobol/src

# Preprocess SQL and compile
cobc -x -free TX_INBOUND.cbl -o ../../bin/TX_INBOUND
cobc -x -free POST_LEDGER.cbl -o ../../bin/POST_LEDGER
cobc -x -free BALANCE_RECALC.cbl -o ../../bin/BALANCE_RECALC
cobc -x -free REPL_REPORTING.cbl -o ../../bin/REPL_REPORTING
cobc -x -free LINEAGE_EXPORT.cbl -o ../../bin/LINEAGE_EXPORT

cd ../..
```

### Step 3: Run the Processing Pipeline

Execute the test runners in sequence:

```bash
# Test 1: Seed database and ingest transactions
bash tests/01_seed_sqlserver.sh

# Expected output:
# ✓ 5 transactions inserted
# ✓ Lineage event logged (transform_kind=ingest)
```

```bash
# Test 2: Post transactions to ledger
bash tests/02_post_ledger.sh

# Expected output:
# ✓ 5 ledger entries created
# ✓ Lineage event logged (transform_kind=posting)
# ✓ PostingAudit record created (5 rows in, 5 rows out)
```

```bash
# Test 3: Recalculate account balances
bash tests/03_balance.sh

# Expected output:
# ✓ 4 account balances updated (ACC001, ACC002, ACC003, ACC004)
# ✓ Lineage event logged (transform_kind=aggregate)
```

```bash
# Test 4: Replicate to reporting database
bash tests/04_repl.sh

# Expected output:
# ✓ 4 daily snapshots inserted/updated (PostgreSQL)
# ✓ 3 currency rollups inserted (USD, EUR, GBP)
# ✓ Lineage events logged (transform_kind=replication)
```

```bash
# Test 5: Export lineage metadata
bash tests/05_export_lineage.sh

# Expected output:
# ✓ lineage.csv created with 5+ rows
# ✓ All transform_kind types present (ingest, posting, aggregate, replication)
```

### Step 4: Verify Results

```bash
# Check SQL Server data
echo "SELECT COUNT(*) FROM dbo.Transactions;" | isql -v SQLSRV_CBLR -b
echo "SELECT COUNT(*) FROM dbo.LedgerEntries;" | isql -v SQLSRV_CBLR -b
echo "SELECT * FROM dbo.AccountBalances;" | isql -v SQLSRV_CBLR -b
echo "SELECT COUNT(*) FROM dbo.LineageEvents;" | isql -v SQLSRV_CBLR -b

# Check PostgreSQL data
echo "SELECT COUNT(*) FROM public.daily_snapshots;" | isql -v PG_CBLR -b
echo "SELECT * FROM public.account_rollups;" | isql -v PG_CBLR -b

# View lineage export
cat lineage/out/lineage.csv
```

---

## Expected Results

### SQL Server (cblr_ops)

**Transactions**: 5 rows
```
TX_ID | ACC_ID | AMOUNT  | CURRENCY | TX_TYPE
------|--------|---------|----------|----------
1     | ACC001 | 100.50  | USD      | DEPOSIT
2     | ACC002 | -25.00  | USD      | WITHDRAWAL
3     | ACC003 | 500.00  | EUR      | TRANSFER
4     | ACC001 | 75.25   | USD      | DEPOSIT
5     | ACC004 | -150.00 | GBP      | WITHDRAWAL
```

**LedgerEntries**: 5 rows
```
ENTRY_ID | TX_ID | ACC_ID | DEBIT  | CREDIT | CURRENCY
---------|-------|--------|--------|--------|----------
1        | 1     | ACC001 | NULL   | 100.50 | USD
2        | 2     | ACC002 | 25.00  | NULL   | USD
3        | 3     | ACC003 | NULL   | 500.00 | EUR
4        | 4     | ACC001 | NULL   | 75.25  | USD
5        | 5     | ACC004 | 150.00 | NULL   | GBP
```

**AccountBalances**: 4 rows
```
ACC_ID | CURRENCY | BALANCE
-------|----------|--------
ACC001 | USD      | 175.75  (100.50 + 75.25)
ACC002 | USD      | -25.00
ACC003 | EUR      | 500.00
ACC004 | GBP      | -150.00
```

**LineageEvents**: 5+ rows (ingest, posting, aggregate, 2× replication)

### PostgreSQL (cblr_report)

**daily_snapshots**: 4 rows
```
snap_date  | acc_id | end_balance | currency
-----------|--------|-------------|----------
2026-01-28 | ACC001 | 175.75      | USD
2026-01-28 | ACC002 | -25.00      | USD
2026-01-28 | ACC003 | 500.00      | EUR
2026-01-28 | ACC004 | -150.00     | GBP
```

**account_rollups**: 3 rows
```
as_of_utc           | currency | total_balance
--------------------|----------|---------------
2026-01-28 10:31:05 | USD      | 150.75
2026-01-28 10:31:05 | EUR      | 500.00
2026-01-28 10:31:05 | GBP      | -150.00
```

### Lineage CSV (lineage/out/lineage.csv)

5 rows showing complete data flow from CSV → SQL Server → PostgreSQL with all transformation metadata.

---

## Troubleshooting

### Issue: ODBC Connection Fails

**Symptoms**: `isql: error while loading shared libraries`

**Solution**:
```bash
# Check library path
export LD_LIBRARY_PATH=/opt/microsoft/msodbcsql18/lib64:$LD_LIBRARY_PATH

# Add to ~/.bashrc for persistence
echo 'export LD_LIBRARY_PATH=/opt/microsoft/msodbcsql18/lib64:$LD_LIBRARY_PATH' >> ~/.bashrc
```

### Issue: COBOL Compilation Errors

**Symptoms**: `Error: LINEAGE-LOGGER not found`

**Solution**:
```bash
# Ensure copybooks are in COBCPY path
export COBCPY=$PWD/cobol/copybooks

# Recompile with explicit copybook path
cob -x -I./cobol/copybooks TX_INBOUND.cbl -o bin/TX_INBOUND
```

### Issue: SQL Server Authentication Fails

**Symptoms**: `Login failed for user`

**Solution**:
- Verify security group allows EC2→RDS traffic on port 1433
- Check SQL Server user exists and has appropriate permissions
- Ensure RDS instance is publicly accessible OR EC2 is in same VPC
- Test connectivity: `telnet <rds-endpoint> 1433`

### Issue: .version File Missing

**Symptoms**: Lineage events show `commit_sha=UNKNOWN`

**Solution**:
```bash
# Regenerate .version file
git rev-parse HEAD > .version

# Verify
cat .version
```

### Issue: CSV Parsing Errors

**Symptoms**: `ERROR: Row 3 - Invalid AMOUNT format`

**Solution**:
- Check CSV file encoding (must be UTF-8)
- Verify line endings (LF or CRLF both supported)
- Ensure no extra spaces in numeric fields
- Validate currency codes are 3 uppercase letters

---

## Next Steps

Once the demo is running successfully:

1. **Modify Sample Data**: Edit `data/in/transactions.csv` with different scenarios
2. **Review Lineage**: Open `lineage/out/lineage.csv` in Excel or text editor
3. **Explore Static Lineage**: Review COBOL source files for `*> LINEAGE:` comments
4. **Query Databases**: Use SQL clients to explore data and relationships
5. **Extend Programs**: Add new COBOL programs or modify transformation logic
6. **(Optional) Ingest to OpenMetadata**: Configure external lineage catalog tool

---

## Demo Script (5-Minute Walkthrough)

For live demonstrations:

1. **Show CSV Input** (30 sec): `cat data/in/transactions.csv`
2. **Run Pipeline** (2 min): Execute test runners 01-05 sequentially
3. **Show Results** (1 min): Query databases to show data flow
4. **Display Lineage** (1 min): `cat lineage/out/lineage.csv` - highlight transform_kind values
5. **Show Code** (30 sec): Open `POST_LEDGER.cbl` and point out static lineage comments

**Key Talking Points**:
- Complete data lineage from CSV to reporting warehouse
- COBOL programs capturing metadata at each transformation step
- Cross-database lineage (SQL Server → PostgreSQL)
- Static (code comments) + Dynamic (runtime events) lineage approach
- Export-ready for data governance tools

---

## Cleanup

To shut down the demo environment:

```bash
# Stop COBOL processes (if any running)
pkill -f "TX_INBOUND|POST_LEDGER|BALANCE_RECALC|REPL_REPORTING|LINEAGE_EXPORT"

# Truncate database tables (optional - preserves schema)
echo "TRUNCATE TABLE dbo.LineageEvents; TRUNCATE TABLE dbo.PostingAudit; TRUNCATE TABLE dbo.AccountBalances; TRUNCATE TABLE dbo.LedgerEntries; DELETE FROM dbo.Transactions;" | isql -v SQLSRV_CBLR
echo "TRUNCATE TABLE public.account_rollups; TRUNCATE TABLE public.daily_snapshots;" | isql -v PG_CBLR

# Remove output files
rm -f lineage/out/lineage.csv
rm -f data/in/transactions.csv
```

**AWS Resources**: Remember to stop/terminate EC2, RDS, and Aurora instances to avoid charges.

---

## Additional Resources

- **Feature Specification**: [spec.md](spec.md)
- **Research Findings**: [research.md](research.md)
- **Data Model**: [data-model.md](data-model.md)
- **Database Contracts**: [contracts/databases/](contracts/databases/)
- **CSV Formats**: [contracts/csv/](contracts/csv/) and [contracts/lineage/](contracts/lineage/)

---

**Quickstart Version**: 1.0  
**Last Updated**: 2026-01-28  
**Maintainer**: COBOL Data Lineage Feature Team

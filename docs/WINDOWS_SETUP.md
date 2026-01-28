# COBOL Banking Application - Windows Setup and Execution Guide

## Current Status
✅ All COBOL source code implemented  
✅ Database DDL scripts ready  
✅ Configuration templates created  
❌ COBOL compiler not installed  
❌ Database servers not configured  
❌ ODBC drivers not installed  

---

## Prerequisites for Running on Windows

### 1. Install GnuCOBOL (Open Source - Recommended for Windows)

**Download and Install:**
```powershell
# Option A: Using Chocolatey (if installed)
choco install gnucobol

# Option B: Manual Installation
# 1. Download from: https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/
# 2. Download Windows binary: gnucobol-3.2-windows-x64.zip
# 3. Extract to C:\GnuCOBOL
# 4. Add to PATH:
$env:Path += ";C:\GnuCOBOL\bin"
[Environment]::SetEnvironmentVariable("Path", $env:Path, [System.EnvironmentVariableTarget]::User)
```

**Verify Installation:**
```powershell
cobc --version
```

### 2. Install Database Servers

**Option A: SQL Server Express (Local Development)**
```powershell
# Download SQL Server 2022 Express
# https://www.microsoft.com/en-us/sql-server/sql-server-downloads

# After installation, create database:
sqlcmd -S localhost -E -Q "CREATE DATABASE cblr_ops"
sqlcmd -S localhost -E -d cblr_ops -i sql\sqlserver\ddl.sql
```

**Option B: PostgreSQL (Local Development)**
```powershell
# Using Chocolatey
choco install postgresql

# Or download from: https://www.postgresql.org/download/windows/

# After installation, create database:
psql -U postgres -c "CREATE DATABASE cblr_report"
psql -U postgres -d cblr_report -f sql\postgres\ddl.sql
```

### 3. Install ODBC Drivers

**SQL Server ODBC Driver:**
```powershell
# Download from Microsoft:
# https://learn.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server

# Install ODBC Driver 18 for SQL Server
```

**PostgreSQL ODBC Driver:**
```powershell
# Download from:
# https://www.postgresql.org/ftp/odbc/versions/msi/

# Install psqlODBC (64-bit)
```

### 4. Configure ODBC Data Sources

**Create DSNs using ODBC Data Source Administrator (odbcad32.exe):**

```powershell
# Open ODBC Administrator
odbcad32.exe
```

**Add System DSN for SQL Server:**
- Name: `SQLSRV_CBLR`
- Driver: ODBC Driver 18 for SQL Server
- Server: `localhost` (or your SQL Server instance)
- Database: `cblr_ops`
- Authentication: Windows Authentication or SQL Server Authentication

**Add System DSN for PostgreSQL:**
- Name: `PG_CBLR`
- Driver: PostgreSQL Unicode
- Server: `localhost`
- Database: `cblr_report`
- Port: `5432`
- Username: `postgres`
- Password: (your password)

---

## Compilation Instructions

### Compile All COBOL Programs

```powershell
# Navigate to source directory
cd cobol\src

# Set copybook path
$env:COBCPY = "..\..\cobol\copybooks"

# Compile each program
cobc -x -free TX_INBOUND.cbl -o TX_INBOUND.exe
cobc -x -free POST_LEDGER.cbl -o POST_LEDGER.exe
cobc -x -free BALANCE_RECALC.cbl -o BALANCE_RECALC.exe
cobc -x -free REPL_REPORTING.cbl -o REPL_REPORTING.exe
cobc -x -free LINEAGE_EXPORT.cbl -o LINEAGE_EXPORT.exe

# Return to project root
cd ..\..
```

**Note**: The `-free` flag enables free-format COBOL. If using Micro Focus Visual COBOL, use:
```powershell
cob -x TX_INBOUND.cbl -o TX_INBOUND.exe
```

---

## Execution Instructions

### Full Pipeline Execution

**Using PowerShell (Windows equivalent of Bash scripts):**

```powershell
# 1. Seed SQL Server database
Write-Host "Step 1: Seeding SQL Server database..."
sqlcmd -S localhost -E -d cblr_ops -i sql\sqlserver\ddl.sql

# 2. Run TX_INBOUND (CSV ingestion)
Write-Host "Step 2: Ingesting CSV transactions..."
.\cobol\src\TX_INBOUND.exe

# 3. Run POST_LEDGER (double-entry posting)
Write-Host "Step 3: Posting ledger entries..."
.\cobol\src\POST_LEDGER.exe

# 4. Run BALANCE_RECALC (balance calculation)
Write-Host "Step 4: Calculating balances..."
.\cobol\src\BALANCE_RECALC.exe

# 5. Run REPL_REPORTING (cross-database replication)
Write-Host "Step 5: Replicating to PostgreSQL..."
.\cobol\src\REPL_REPORTING.exe

# 6. Run LINEAGE_EXPORT (lineage CSV export)
Write-Host "Step 6: Exporting lineage metadata..."
.\cobol\src\LINEAGE_EXPORT.exe

# 7. View results
Write-Host "Lineage Export:"
Get-Content lineage\out\lineage.csv
```

---

## Alternative: Docker-Based Execution (Recommended)

For a fully isolated environment, use Docker:

### Create Dockerfile

```dockerfile
FROM ubuntu:22.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    gnucobol \
    unixodbc \
    unixodbc-dev \
    odbc-postgresql \
    postgresql-client \
    git

# Copy application
COPY . /app
WORKDIR /app

# Compile COBOL programs
RUN cd cobol/src && \
    export COBCPY=../copybooks && \
    cobc -x -free TX_INBOUND.cbl -o TX_INBOUND && \
    cobc -x -free POST_LEDGER.cbl -o POST_LEDGER && \
    cobc -x -free BALANCE_RECALC.cbl -o BALANCE_RECALC && \
    cobc -x -free REPL_REPORTING.cbl -o REPL_REPORTING && \
    cobc -x -free LINEAGE_EXPORT.cbl -o LINEAGE_EXPORT

CMD ["/bin/bash"]
```

### Run with Docker Compose

```yaml
version: '3.8'
services:
  sqlserver:
    image: mcr.microsoft.com/mssql/server:2022-latest
    environment:
      SA_PASSWORD: "YourStrong@Passw0rd"
      ACCEPT_EULA: "Y"
    ports:
      - "1433:1433"
  
  postgres:
    image: postgres:15
    environment:
      POSTGRES_PASSWORD: postgres
      POSTGRES_DB: cblr_report
    ports:
      - "5432:5432"
  
  cobol-app:
    build: .
    depends_on:
      - sqlserver
      - postgres
    volumes:
      - ./lineage/out:/app/lineage/out
```

**Execute:**
```powershell
docker-compose up -d
docker-compose exec cobol-app bash
# Then run the pipeline inside the container
```

---

## Quick Demo (Without Full Setup)

If you want to see what the application does without full setup, run the simulation:

```powershell
# Run demonstration simulation
.\docs\demo-simulation.ps1
```

This will:
1. Show sample CSV input
2. Demonstrate data transformations
3. Display expected lineage output
4. Explain each processing step

---

## Troubleshooting

### Issue: "EXEC SQL not recognized"
**Solution**: Use a COBOL compiler with embedded SQL support (Micro Focus) or preprocess with ESQL/C

### Issue: "Cannot connect to database"
**Solution**: 
1. Verify database servers are running
2. Check ODBC DSNs are configured correctly
3. Test connection: `isql -v SQLSRV_CBLR`

### Issue: "Copybook not found"
**Solution**: Set COBCPY environment variable:
```powershell
$env:COBCPY = "$PWD\cobol\copybooks"
```

### Issue: "File not found: transactions.csv"
**Solution**: Ensure CSV file exists in `data/in/transactions.csv` and run from project root

---

## Next Steps

**Choose your path:**

1. **Full Setup (1-2 hours)**: Install all prerequisites and run on Windows
2. **Docker Setup (30 minutes)**: Use Docker containers for isolated environment
3. **AWS Deployment (1 hour)**: Deploy to EC2 as per quickstart.md
4. **Demo Simulation (5 minutes)**: Run PowerShell simulation script

**Recommended**: Start with Docker setup for fastest results, then optionally deploy to AWS for production-like environment.

---

**For AWS EC2 deployment**, see: [specs/001-cobol-data-lineage/quickstart.md](../specs/001-cobol-data-lineage/quickstart.md)

**For detailed documentation**, see: [docs/README.md](README.md)

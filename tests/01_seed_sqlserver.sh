#!/bin/bash
################################################################################
# 01_seed_sqlserver.sh - SQL Server Database Initialization
################################################################################
# Purpose: Create SQL Server schema and seed sample data
# Usage: ./tests/01_seed_sqlserver.sh
# Prerequisites: 
#   - SQL Server RDS instance running
#   - ODBC configuration complete (config/odbc.ini)
#   - sqlcmd or isql installed
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
DDL_FILE="$PROJECT_ROOT/sql/sqlserver/ddl.sql"
DSN="SQLSRV_CBLR"

echo "============================================================================"
echo " SQL Server Database Seeding"
echo "============================================================================"
echo "DDL File: $DDL_FILE"
echo "DSN: $DSN"
echo ""

# Check if DDL file exists
if [ ! -f "$DDL_FILE" ]; then
    echo "ERROR: DDL file not found: $DDL_FILE"
    exit 1
fi

# Test ODBC connection
echo "Testing ODBC connection to $DSN..."
if ! echo "SELECT 1;" | isql -v "$DSN" >/dev/null 2>&1; then
    echo "ERROR: Cannot connect to DSN $DSN"
    echo "Please check config/odbc.ini configuration"
    exit 1
fi
echo "✓ Connection successful"
echo ""

# Execute DDL script
echo "Executing SQL Server DDL script..."
if command -v sqlcmd &> /dev/null; then
    # Use sqlcmd if available (preferred for SQL Server)
    sqlcmd -S "$DSN" -d cblr_ops -i "$DDL_FILE"
else
    # Fallback to isql
    cat "$DDL_FILE" | isql -v "$DSN"
fi

echo ""
echo "✓ SQL Server schema created successfully"
echo ""

# Optional: Insert sample data
echo "Inserting sample transaction data..."
cat <<EOF | isql -v "$DSN"
USE cblr_ops;
DELETE FROM dbo.LineageEvents;
DELETE FROM dbo.PostingAudit;
DELETE FROM dbo.LedgerEntries;
DELETE FROM dbo.AccountBalances;
DELETE FROM dbo.Transactions;

-- Sample transactions matching CSV format
INSERT INTO dbo.Transactions (ACC_ID, AMOUNT, CURRENCY, TX_TS_UTC, TX_TYPE) VALUES
('ACC001', 1500.00, 'USD', '2024-01-15 09:30:00', 'DEPOSIT'),
('ACC001', -45.50, 'USD', '2024-01-15 14:22:00', 'WITHDRAWAL'),
('ACC002', 2500.00, 'EUR', '2024-01-15 10:15:00', 'DEPOSIT'),
('ACC002', -150.00, 'EUR', '2024-01-16 11:45:00', 'WITHDRAWAL'),
('ACC003', 500.00, 'GBP', '2024-01-16 08:00:00', 'DEPOSIT'),
('ACC001', 75.25, 'USD', '2024-01-16 16:30:00', 'DEPOSIT'),
('ACC003', -200.00, 'GBP', '2024-01-17 09:15:00', 'WITHDRAWAL'),
('ACC002', 1000.00, 'EUR', '2024-01-17 13:45:00', 'TRANSFER');
EOF

echo ""
echo "============================================================================"
echo " SQL Server Seed Complete"
echo "============================================================================"
echo "✓ Schema created"
echo "✓ Sample data inserted (8 transactions)"
echo ""
echo "Verification queries:"
echo "  SELECT COUNT(*) FROM dbo.Transactions;"
echo "  SELECT * FROM dbo.Transactions ORDER BY TX_TS_UTC;"
echo "============================================================================"

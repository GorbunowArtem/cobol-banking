#!/bin/bash
#########################################################
# Extract CustomerAccounts data from MySQL Aurora
# to input file for COBOL program
#########################################################

# MySQL Aurora connection details
DB_HOST="${MYSQL_HOST:-your-aurora-cluster.region.rds.amazonaws.com}"
DB_USER="${MYSQL_USER:-admin}"
DB_PASS="${MYSQL_PASS:-your-password}"
# Source database (CustomerAccounts table)
DB_SOURCE="${MYSQL_DB_SOURCE:-banking}"
# Target database (AccountTransactions table) - not used in this script but for reference
DB_TARGET="${MYSQL_DB_TARGET:-bank}"

echo "================================================"
echo "Extracting Active Accounts from MySQL Aurora"
echo "================================================"

# Export data from MySQL to CSV format (from source database: banking)
mysql -h "$DB_HOST" \
      -u "$DB_USER" \
      -p"$DB_PASS" \
      -D "$DB_SOURCE" \
      --batch \
      --skip-column-names \
      -e "SELECT AccountID, CustomerID, CustomerName, AccountNumber,
                 AccountType, Balance, BranchCode, KYCStatus, RiskScore,
                 AccountStatus, CreatedDate, LastUpdated
          FROM CustomerAccounts
          WHERE AccountStatus = 'Active'
          ORDER BY AccountID" \
      | sed 's/\t/|/g' > accounts.dat

if [ $? -eq 0 ]; then
    RECORD_COUNT=$(wc -l < accounts.dat)
    echo "Successfully extracted $RECORD_COUNT records"
    echo "Output file: accounts.dat"
    echo "================================================"
else
    echo "ERROR: Failed to extract data from MySQL"
    exit 1
fi

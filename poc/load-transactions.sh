#!/bin/bash
#########################################################
# Load transformed transactions into MySQL Aurora
#########################################################

# MySQL Aurora connection details
DB_HOST="${MYSQL_HOST:-your-aurora-cluster.region.rds.amazonaws.com}"
DB_USER="${MYSQL_USER:-admin}"
DB_PASS="${MYSQL_PASS:-your-password}"
DB_NAME="${MYSQL_DB:-bank}"

SQL_FILE="transactions.sql"

if [ ! -f "$SQL_FILE" ]; then
    echo "ERROR: $SQL_FILE not found"
    echo "Run the COBOL program first to generate the SQL file"
    exit 1
fi

echo "================================================"
echo "Loading Transactions to MySQL Aurora"
echo "================================================"

# Execute SQL file
mysql -h "$DB_HOST" \
      -u "$DB_USER" \
      -p"$DB_PASS" \
      -D "$DB_NAME" \
      < "$SQL_FILE"

if [ $? -eq 0 ]; then
    # Count inserted records
    RECORD_COUNT=$(mysql -h "$DB_HOST" \
                         -u "$DB_USER" \
                         -p"$DB_PASS" \
                         -D "$DB_NAME" \
                         --batch \
                         --skip-column-names \
                         -e "SELECT COUNT(*) FROM AccountTransactions WHERE Channel='COBOL-Batch'")

    echo "Successfully loaded transactions"
    echo "Total COBOL-Batch records: $RECORD_COUNT"
    echo "================================================"
else
    echo "ERROR: Failed to load data into MySQL"
    exit 1
fi

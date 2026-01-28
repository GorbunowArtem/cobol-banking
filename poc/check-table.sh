#!/bin/bash
# Quick check of MySQL table structure
export MYSQL_HOST="dev-lineage-db.cluster-cbcjculryebe.eu-west-1.rds.amazonaws.com"
export MYSQL_USER="admin"
export MYSQL_PASS="C0bo11-ll-POC-7"
export MYSQL_DB="bank"

echo "=== AccountTransactions Table Structure ==="
mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" -p"$MYSQL_PASS" -D "$MYSQL_DB" -e "DESCRIBE AccountTransactions;"

#!/bin/bash
echo "=== AccountTransactions Table Structure ==="
mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" -p"$MYSQL_PASS" -D "$MYSQL_DB" -e "DESCRIBE AccountTransactions;"

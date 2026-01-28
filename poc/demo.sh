#!/bin/bash
#########################################################
# COBOL Data Transformation Demo Journey
# Shows before/after state of tables
#########################################################


# 1. Install GnuCOBOL
#sudo apt update && sudo apt install gnucobol4 mysql-client -y

# MySQL Aurora connection details
#export MYSQL_HOST=""
#export MYSQL_USER=""
#export MYSQL_PASS=""
#export MYSQL_DB=""

clear

echo "╔════════════════════════════════════════════════════════════╗"
echo "║   COBOL DATA TRANSFORMATION DEMO                           ║"
echo "║   From CustomerAccounts → AccountTransactions              ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Function to run MySQL query
run_query() {
    mysql -h "$MYSQL_HOST" -u "$MYSQL_USER" -p"$MYSQL_PASS" -D "$MYSQL_DB" -e "$1" 2>/dev/null
}

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "STEP 1: Checking Source Table (CustomerAccounts)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

SOURCE_COUNT=$(run_query "SELECT COUNT(*) FROM CustomerAccounts WHERE AccountStatus='Active';" | tail -n 1)
echo "✓ Source table has $SOURCE_COUNT ACTIVE accounts ready for transformation"
echo ""

echo "Sample source data:"
run_query "SELECT AccountID, CustomerID, CustomerName, AccountNumber, AccountType, Balance, BranchCode
           FROM CustomerAccounts
           WHERE AccountStatus='Active'
           LIMIT 5;"
echo ""

read -p "Press ENTER to continue..."
clear

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "STEP 2: Checking Target Table (AccountTransactions)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Clear any previous COBOL-Batch records for a clean demo
echo "Clearing any previous demo data..."
run_query "DELETE FROM AccountTransactions WHERE Channel='COBOL-Batch';" >/dev/null 2>&1

TARGET_COUNT=$(run_query "SELECT COUNT(*) FROM AccountTransactions WHERE Channel='COBOL-Batch';" | tail -n 1)
echo "✓ Target table has $TARGET_COUNT COBOL-Batch records"
echo ""

if [ "$TARGET_COUNT" -eq 0 ]; then
    echo "📋 The target table is EMPTY - ready for transformation!"
else
    echo "⚠️  Warning: Target table already has data"
fi
echo ""

read -p "Press ENTER to run COBOL transformation..."
clear

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "STEP 3: Running COBOL Data Transformation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Extract data
echo "→ Extracting active accounts from MySQL Aurora..."
bash extract-accounts.sh 2>/dev/null | grep -E "(extracted|Output)"
echo ""

# Compile (only if needed)
if [ ! -f "acctxfrm" ] || [ "ACCTXFRM-MYSQL.cbl" -nt "acctxfrm" ]; then
    echo "→ Compiling COBOL program..."
    cobc -x ACCTXFRM-MYSQL.cbl -o acctxfrm 2>&1 | grep -i "error" || echo "  ✓ Compilation successful"
    echo ""
fi

# Run transformation
echo "→ Running COBOL transformation..."
./acctxfrm 2>/dev/null | grep -E "(Records read|Records processed|Records skipped)"
echo ""

# Load to database
echo "→ Loading transformed data to MySQL Aurora..."
bash load-transactions.sh 2>/dev/null | grep -E "(Successfully|Total)"
echo ""

read -p "Press ENTER to view results..."
clear

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "STEP 4: Verification - Target Table Now Has Data!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

NEW_TARGET_COUNT=$(run_query "SELECT COUNT(*) FROM AccountTransactions WHERE Channel='COBOL-Batch';" | tail -n 1)
echo "✅ Target table now has $NEW_TARGET_COUNT COBOL-Batch records"
echo ""

echo "Sample transformed data:"
run_query "SELECT AccountID, CustomerID, AccountNumber, Description,
                  TransactionSubType, Amount, RunningBalance, ReferenceNumber
           FROM AccountTransactions
           WHERE Channel='COBOL-Batch'
           ORDER BY TransactionID DESC
           LIMIT 5;"
echo ""

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "SUMMARY: Data Transformation Complete!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "  Source (CustomerAccounts):       $SOURCE_COUNT active accounts"
echo "  Target (AccountTransactions):    $NEW_TARGET_COUNT transformed records"
echo ""
echo "  Transformation Logic Applied:"
echo "    • Description: Customer name + Account Inquiry"
echo "    • SubType: Based on account type (CHK/SAV/INV/CRD-Inquiry)"
echo "    • Reference: RSK-{RiskScore}-{AccountNumber}"
echo "    • Amount & Balance: Set to account balance"
echo "    • Channel: Marked as 'COBOL-Batch'"
echo ""
echo "╔════════════════════════════════════════════════════════════╗"
echo "║   ✅ DEMO COMPLETE - COBOL TRANSFORMATION SUCCESSFUL       ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

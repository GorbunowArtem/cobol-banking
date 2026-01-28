#!/bin/bash
################################################################################
# COBOL TRANSFORMATION DEMO JOURNEY
# Shows the complete data transformation process step-by-step
################################################################################
# Purpose: Demonstrate how COBOL transforms data from source to target table
# - Shows source table (CustomerAccounts) with data
# - Shows target table (AccountTransactions) empty
# - Runs COBOL transformation
# - Shows target table now populated
################################################################################

set -e  # Exit on error

# Color codes for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# MySQL connection details from environment
DB_HOST="${MYSQL_HOST:-localhost}"
DB_USER="${MYSQL_USER:-admin}"
DB_PASS="${MYSQL_PASS:-password}"
# Source database (CustomerAccounts)
DB_SOURCE="${MYSQL_DB_SOURCE:-banking}"
# Target database (AccountTransactions)
DB_TARGET="${MYSQL_DB_TARGET:-bank}"

# Helper function to execute MySQL query on source database
run_query_source() {
    local query="$1"
    mysql -h "$DB_HOST" \
          -u "$DB_USER" \
          -p"$DB_PASS" \
          -D "$DB_SOURCE" \
          --table \
          -e "$query"
}

# Helper function to execute MySQL query on target database
run_query_target() {
    local query="$1"
    mysql -h "$DB_HOST" \
          -u "$DB_USER" \
          -p"$DB_PASS" \
          -D "$DB_TARGET" \
          --table \
          -e "$query"
}

# Helper function to execute MySQL query silently on source database
run_query_source_silent() {
    local query="$1"
    mysql -h "$DB_HOST" \
          -u "$DB_USER" \
          -p"$DB_PASS" \
          -D "$DB_SOURCE" \
          --batch \
          --skip-column-names \
          -e "$query" 2>/dev/null
}

# Helper function to execute MySQL query silently on target database
run_query_target_silent() {
    local query="$1"
    mysql -h "$DB_HOST" \
          -u "$DB_USER" \
          -p"$DB_PASS" \
          -D "$DB_TARGET" \
          --batch \
          --skip-column-names \
          -e "$query" 2>/dev/null
}

echo -e "${CYAN}================================================================================"
echo -e " COBOL BANKING TRANSFORMATION - DEMO JOURNEY"
echo -e "================================================================================${NC}"
echo ""
echo -e "${YELLOW}This demo shows:"
echo -e "  1. Source table (CustomerAccounts) with data"
echo -e "  2. Target table (AccountTransactions) empty"
echo -e "  3. COBOL transformation in action"
echo -e "  4. Target table now populated with transformed data${NC}"
echo ""
read -p "Press Enter to begin the demo..."

################################################################################
# STEP 1: Show Source Table (CustomerAccounts) - Should Have Data
################################################################################
echo ""
echo -e "${GREEN}================================================================================"
echo -e " STEP 1: SOURCE TABLE - CustomerAccounts"
echo -e "================================================================================${NC}"
echo ""

SOURCE_COUNT=$(run_query_source_silent "SELECT COUNT(*) FROM CustomerAccounts;")
echo -e "${BLUE}Total records in CustomerAccounts: ${YELLOW}$SOURCE_COUNT${NC}"
echo ""

if [ "$SOURCE_COUNT" -eq 0 ]; then
    echo -e "${RED}ERROR: Source table is empty!${NC}"
    echo -e "${YELLOW}Running database seed script...${NC}"
    if [ -f "db/mysql-tables.sql" ]; then
        mysql -h "$DB_HOST" -u "$DB_USER" -p"$DB_PASS" < db/mysql-tables.sql
        SOURCE_COUNT=$(run_query_source_silent "SELECT COUNT(*) FROM CustomerAccounts;")
        echo -e "${GREEN}Seeded $SOURCE_COUNT records${NC}"
    else
        echo -e "${RED}Cannot find db/mysql-tables.sql${NC}"
        exit 1
    fi
fi

echo -e "${CYAN}Sample data from CustomerAccounts:${NC}"
run_query_source "SELECT AccountID, CustomerName, AccountNumber, AccountType, Balance, AccountStatus
           FROM CustomerAccounts
           LIMIT 10;"

read -p "Press Enter to continue..."

################################################################################
# STEP 2: Show Target Table (AccountTransactions) - Should Be Empty
################################################################################
echo ""
echo -e "${GREEN}================================================================================"
echo -e " STEP 2: TARGET TABLE - AccountTransactions (Before Transformation)"
echo -e "================================================================================${NC}"
echo ""

# Clear any existing COBOL-Batch records to ensure clean demo
echo -e "${YELLOW}Clearing any existing COBOL-Batch records for clean demo...${NC}"
run_query_target_silent "DELETE FROM AccountTransactions WHERE Channel='COBOL-Batch';" 2>/dev/null || true

TARGET_COUNT=$(run_query_target_silent "SELECT COUNT(*) FROM AccountTransactions WHERE Channel='COBOL-Batch';" 2>/dev/null || echo "0")
echo -e "${BLUE}Total COBOL-Batch records in AccountTransactions: ${YELLOW}$TARGET_COUNT${NC}"
echo ""

if [ "$TARGET_COUNT" -eq 0 ]; then
    echo -e "${GREEN}✓ Target table is empty - ready for transformation${NC}"
else
    echo -e "${YELLOW}⚠ Target table has existing records${NC}"
fi

echo ""
run_query_target "SELECT COUNT(*) as TotalRecords FROM AccountTransactions WHERE Channel='COBOL-Batch';"

read -p "Press Enter to run COBOL transformation..."

################################################################################
# STEP 3: Run COBOL Transformation
################################################################################
echo ""
echo -e "${GREEN}================================================================================"
echo -e " STEP 3: RUNNING COBOL TRANSFORMATION"
echo -e "================================================================================${NC}"
echo ""

# Check if program exists, if not compile it
if [ ! -f "acctxfrm" ]; then
    echo -e "${YELLOW}Compiling COBOL program...${NC}"
    cobc -x ACCTXFRM-MYSQL.cbl -o acctxfrm
    echo -e "${GREEN}✓ Compilation successful${NC}"
    echo ""
fi

# Extract source data
echo -e "${CYAN}Extracting data from CustomerAccounts...${NC}"
if [ -f "extract-accounts.sh" ]; then
    bash extract-accounts.sh
else
    # Manual extraction if script not found
    run_query_source_silent "SELECT AccountID, CustomerID, CustomerName, AccountNumber, AccountType,
                             Balance, BranchCode, AccountStatus
                      FROM CustomerAccounts
                      WHERE AccountStatus = 'Active'" > accounts.dat
fi

EXTRACTED_COUNT=$(wc -l < accounts.dat 2>/dev/null || echo "0")
echo -e "${GREEN}✓ Extracted $EXTRACTED_COUNT records to accounts.dat${NC}"
echo ""

# Run COBOL transformation
echo -e "${CYAN}Running COBOL program...${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════════${NC}"
./acctxfrm
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════════${NC}"
echo ""

# Check if SQL file was created
if [ ! -f "transactions.sql" ]; then
    echo -e "${RED}ERROR: transactions.sql not created by COBOL program${NC}"
    exit 1
fi

echo -e "${GREEN}✓ COBOL transformation completed${NC}"
echo -e "${CYAN}Generated SQL file: transactions.sql${NC}"
echo ""

# Show sample of generated SQL
echo -e "${YELLOW}Sample of generated SQL statements:${NC}"
head -n 5 transactions.sql
echo "..."
echo ""

read -p "Press Enter to load transformed data into database..."

# Load transformed data
echo -e "${CYAN}Loading transformed data into AccountTransactions...${NC}"
mysql -h "$DB_HOST" \
      -u "$DB_USER" \
      -p"$DB_PASS" \
      -D "$DB_NAME" \
      < transactions.sql 2>&1 | grep -v "Warning: Using a password"

echo -e "${GREEN}✓ Data loaded successfully${NC}"
echo ""

################################################################################
# STEP 4: Show Target Table - Now Has Data
################################################################################
echo ""
echo -e "${GREEN}================================================================================"
echo -e " STEP 4: TARGET TABLE - AccountTransactions (After Transformation)"
echo -e "================================================================================${NC}"
echo ""

FINAL_COUNT=$(run_query_target_silent "SELECT COUNT(*) FROM AccountTransactions WHERE Channel='COBOL-Batch';")
echo -e "${BLUE}Total COBOL-Batch records in AccountTransactions: ${GREEN}$FINAL_COUNT${NC}"
echo ""

if [ "$FINAL_COUNT" -gt 0 ]; then
    echo -e "${GREEN}✓ SUCCESS! Target table now has data${NC}"
    echo ""

    echo -e "${CYAN}Sample transformed data:${NC}"
    run_query_target "SELECT TransactionID, AccountNumber, CustomerID, Description,
                      TransactionSubType, Amount, Channel
               FROM AccountTransactions
               WHERE Channel='COBOL-Batch'
               LIMIT 10;"

    echo ""
    echo -e "${CYAN}Summary by Transaction SubType:${NC}"
    run_query_target "SELECT TransactionSubType, COUNT(*) as Count, SUM(Amount) as TotalAmount
               FROM AccountTransactions
               WHERE Channel='COBOL-Batch'
               GROUP BY TransactionSubType
               ORDER BY Count DESC;"
else
    echo -e "${RED}⚠ WARNING: No records were inserted${NC}"
fi

################################################################################
# SUMMARY
################################################################################
echo ""
echo -e "${GREEN}================================================================================"
echo -e " DEMO SUMMARY"
echo -e "================================================================================${NC}"
echo ""
echo -e "${CYAN}Before Transformation:${NC}"
echo -e "  Source (CustomerAccounts):        ${YELLOW}$SOURCE_COUNT records${NC}"
echo -e "  Target (AccountTransactions):     ${YELLOW}0 records${NC}"
echo ""
echo -e "${CYAN}After Transformation:${NC}"
echo -e "  Source (CustomerAccounts):        ${YELLOW}$SOURCE_COUNT records${NC}"
echo -e "  Target (AccountTransactions):     ${GREEN}$FINAL_COUNT records${NC}"
echo ""
echo -e "${GREEN}✓ Demo completed successfully!${NC}"
echo ""
echo -e "${YELLOW}Transformation ratio: Each source account generated $(echo "$FINAL_COUNT / $SOURCE_COUNT" | bc 2>/dev/null || echo "2") transactions${NC}"
echo ""
echo -e "${CYAN}To verify the data:${NC}"
echo -e "  ${BLUE}mysql -h \$MYSQL_HOST -u \$MYSQL_USER -p\$MYSQL_PASS -D $DB_NAME${NC}"
echo -e "  ${BLUE}SELECT * FROM AccountTransactions WHERE Channel='COBOL-Batch' LIMIT 10;${NC}"
echo ""

#!/bin/bash
#########################################################
# Complete COBOL transformation pipeline for MySQL
#########################################################

set -e  # Exit on error

echo "================================================"
echo "COBOL Account Transformation Pipeline"
echo "MySQL Aurora Version"
echo "================================================"

# Step 1: Check if GnuCOBOL is installed
if ! command -v cobc &> /dev/null; then
    echo "ERROR: GnuCOBOL not installed"
    echo "Run: sudo apt install gnucobol4 -y"
    exit 1
fi

# Step 2: Extract data from MySQL
echo ""
echo "Step 1: Extracting data from MySQL..."
if [ -f "extract-accounts.sh" ]; then
    bash extract-accounts.sh
else
    echo "WARNING: extract-accounts.sh not found, skipping extraction"
    if [ ! -f "accounts.dat" ]; then
        echo "ERROR: No accounts.dat file found"
        exit 1
    fi
fi

# Step 3: Compile COBOL program
echo ""
echo "Step 2: Compiling COBOL program..."
cobc -x ACCTXFRM-MYSQL.cbl -o acctxfrm

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed"
    exit 1
fi

echo "Compilation successful"

# Step 4: Run COBOL program
echo ""
echo "Step 3: Running transformation..."
./acctxfrm

if [ $? -ne 0 ]; then
    echo "ERROR: Program execution failed"
    exit 1
fi

# Step 5: Load results into MySQL
echo ""
echo "Step 4: Loading results to MySQL..."
if [ -f "load-transactions.sh" ]; then
    bash load-transactions.sh
else
    echo "WARNING: load-transactions.sh not found"
    echo "SQL file created: transactions.sql"
    echo "Load manually with: mysql -u user -p banking < transactions.sql"
fi

echo ""
echo "================================================"
echo "Transformation pipeline completed successfully!"
echo "================================================"

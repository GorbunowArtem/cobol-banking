#!/bin/bash
################################################################################
# 03_balance.sh - Execute Balance Calculation Program
################################################################################
# Purpose: Run BALANCE_RECALC.cbl to aggregate account balances
# Usage: ./tests/03_balance.sh
# Prerequisites:
#   - Ledger entries posted (run 02_post_ledger.sh first)
#   - BALANCE_RECALC program compiled
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
PROGRAM="$PROJECT_ROOT/cobol/src/BALANCE_RECALC"

echo "============================================================================"
echo " Balance Calculation Execution"
echo "============================================================================"
echo "Program: $PROGRAM"
echo ""

# Check if program exists
if [ ! -f "$PROGRAM" ]; then
    echo "ERROR: BALANCE_RECALC program not found: $PROGRAM"
    echo "Please compile the COBOL program first:"
    echo "  cd cobol/src && cob -x BALANCE_RECALC.cbl"
    exit 1
fi

# Execute the program
echo "Running BALANCE_RECALC program..."
cd "$PROJECT_ROOT"
"$PROGRAM"

RETURN_CODE=$?

echo ""
if [ $RETURN_CODE -eq 0 ]; then
    echo "============================================================================"
    echo " ✓ BALANCE_RECALC completed successfully"
    echo "============================================================================"
    echo ""
    echo "Verification queries:"
    echo "  SELECT COUNT(*) FROM dbo.AccountBalances;"
    echo "  SELECT * FROM dbo.AccountBalances ORDER BY ACC_ID, CURRENCY;"
    echo "  SELECT * FROM dbo.vw_DailyBalances;"
    echo "============================================================================"
else
    echo "============================================================================"
    echo " ✗ BALANCE_RECALC failed with return code: $RETURN_CODE"
    echo "============================================================================"
    exit $RETURN_CODE
fi

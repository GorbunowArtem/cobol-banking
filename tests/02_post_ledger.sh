#!/bin/bash
################################################################################
# 02_post_ledger.sh - Execute Ledger Posting Program
################################################################################
# Purpose: Run POST_LEDGER.cbl to create double-entry ledger entries
# Usage: ./tests/02_post_ledger.sh
# Prerequisites:
#   - SQL Server seeded with transactions (run 01_seed_sqlserver.sh first)
#   - POST_LEDGER program compiled
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
PROGRAM="$PROJECT_ROOT/cobol/src/POST_LEDGER"

echo "============================================================================"
echo " Ledger Posting Execution"
echo "============================================================================"
echo "Program: $PROGRAM"
echo ""

# Check if program exists
if [ ! -f "$PROGRAM" ]; then
    echo "ERROR: POST_LEDGER program not found: $PROGRAM"
    echo "Please compile the COBOL program first:"
    echo "  cd cobol/src && cob -x POST_LEDGER.cbl"
    exit 1
fi

# Execute the program
echo "Running POST_LEDGER program..."
cd "$PROJECT_ROOT"
"$PROGRAM"

RETURN_CODE=$?

echo ""
if [ $RETURN_CODE -eq 0 ]; then
    echo "============================================================================"
    echo " ✓ POST_LEDGER completed successfully"
    echo "============================================================================"
    echo ""
    echo "Verification queries:"
    echo "  SELECT COUNT(*) FROM dbo.LedgerEntries;"
    echo "  SELECT * FROM dbo.LedgerEntries ORDER BY POSTED_TS_UTC;"
    echo "  SELECT * FROM dbo.PostingAudit ORDER BY TS_UTC DESC;"
    echo "============================================================================"
else
    echo "============================================================================"
    echo " ✗ POST_LEDGER failed with return code: $RETURN_CODE"
    echo "============================================================================"
    exit $RETURN_CODE
fi

#!/bin/bash
################################################################################
# 05_export_lineage.sh - Execute Lineage Export Program
################################################################################
# Purpose: Run LINEAGE_EXPORT.cbl to export lineage events to CSV
# Usage: ./tests/05_export_lineage.sh
# Prerequisites:
#   - All previous steps completed (lineage events recorded)
#   - LINEAGE_EXPORT program compiled
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
PROGRAM="$PROJECT_ROOT/cobol/src/LINEAGE_EXPORT"
OUTPUT_FILE="$PROJECT_ROOT/lineage/out/lineage.csv"

echo "============================================================================"
echo " Lineage Export Execution"
echo "============================================================================"
echo "Program: $PROGRAM"
echo "Output: $OUTPUT_FILE"
echo ""

# Check if program exists
if [ ! -f "$PROGRAM" ]; then
    echo "ERROR: LINEAGE_EXPORT program not found: $PROGRAM"
    echo "Please compile the COBOL program first:"
    echo "  cd cobol/src && cob -x LINEAGE_EXPORT.cbl"
    exit 1
fi

# Create output directory if needed
mkdir -p "$(dirname "$OUTPUT_FILE")"

# Execute the program
echo "Running LINEAGE_EXPORT program..."
cd "$PROJECT_ROOT"
"$PROGRAM"

RETURN_CODE=$?

echo ""
if [ $RETURN_CODE -eq 0 ]; then
    echo "============================================================================"
    echo " ✓ LINEAGE_EXPORT completed successfully"
    echo "============================================================================"
    echo ""
    
    if [ -f "$OUTPUT_FILE" ]; then
        LINE_COUNT=$(wc -l < "$OUTPUT_FILE")
        echo "Lineage CSV generated:"
        echo "  File: $OUTPUT_FILE"
        echo "  Lines: $LINE_COUNT"
        echo ""
        echo "Preview (first 10 lines):"
        head -10 "$OUTPUT_FILE"
        echo ""
        echo "Lineage flow summary:"
        echo "  - CSV → dbo.Transactions (ingest)"
        echo "  - dbo.Transactions → dbo.LedgerEntries (posting)"
        echo "  - dbo.LedgerEntries → dbo.AccountBalances (aggregate)"
        echo "  - SQL Server → PostgreSQL (replication)"
    else
        echo "WARNING: Lineage CSV file not found: $OUTPUT_FILE"
    fi
    
    echo "============================================================================"
else
    echo "============================================================================"
    echo " ✗ LINEAGE_EXPORT failed with return code: $RETURN_CODE"
    echo "============================================================================"
    exit $RETURN_CODE
fi

#!/bin/bash
################################################################################
# 04_repl.sh - Execute Cross-Database Replication Program
################################################################################
# Purpose: Run REPL_REPORTING.cbl to replicate data to PostgreSQL
# Usage: ./tests/04_repl.sh
# Prerequisites:
#   - Account balances calculated (run 03_balance.sh first)
#   - PostgreSQL database seeded
#   - REPL_REPORTING program compiled
################################################################################

set -e  # Exit on error
set -u  # Exit on undefined variable

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
PROGRAM="$PROJECT_ROOT/cobol/src/REPL_REPORTING"
PG_DDL="$PROJECT_ROOT/sql/postgres/ddl.sql"
PG_DSN="PG_CBLR"

echo "============================================================================"
echo " Cross-Database Replication Execution"
echo "============================================================================"
echo "Program: $PROGRAM"
echo ""

# Initialize PostgreSQL schema if needed
if [ -f "$PG_DDL" ]; then
    echo "Initializing PostgreSQL schema..."
    cat "$PG_DDL" | isql -v "$PG_DSN" || true
    echo "✓ PostgreSQL schema ready"
    echo ""
fi

# Check if program exists
if [ ! -f "$PROGRAM" ]; then
    echo "ERROR: REPL_REPORTING program not found: $PROGRAM"
    echo "Please compile the COBOL program first:"
    echo "  cd cobol/src && cob -x REPL_REPORTING.cbl"
    exit 1
fi

# Execute the program
echo "Running REPL_REPORTING program..."
cd "$PROJECT_ROOT"
"$PROGRAM"

RETURN_CODE=$?

echo ""
if [ $RETURN_CODE -eq 0 ]; then
    echo "============================================================================"
    echo " ✓ REPL_REPORTING completed successfully"
    echo "============================================================================"
    echo ""
    echo "Verification queries (PostgreSQL):"
    echo "  SELECT COUNT(*) FROM public.daily_snapshots;"
    echo "  SELECT * FROM public.daily_snapshots ORDER BY snap_date, acc_id;"
    echo "  SELECT * FROM public.account_rollups ORDER BY as_of_utc DESC;"
    echo "  SELECT * FROM public.vw_currency_summary;"
    echo "============================================================================"
else
    echo "============================================================================"
    echo " ✗ REPL_REPORTING failed with return code: $RETURN_CODE"
    echo "============================================================================"
    exit $RETURN_CODE
fi

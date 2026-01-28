-- ============================================================================
-- PostgreSQL DDL: COBOL Banking Data Lineage - Reporting Database
-- ============================================================================
-- Database: cblr_report
-- Purpose: Reporting warehouse for replicated balances and rollups
-- Target: Amazon Aurora PostgreSQL
-- ============================================================================

\c cblr_report;

-- ----------------------------------------------------------------------------
-- Table: daily_snapshots
-- Purpose: Daily account balance snapshots replicated from operational DB
-- ----------------------------------------------------------------------------
CREATE TABLE public.daily_snapshots (
    snap_date     DATE NOT NULL,
    acc_id        VARCHAR(32) NOT NULL,
    end_balance   NUMERIC(18,2) NOT NULL,
    currency      CHAR(3) NOT NULL,
    
    CONSTRAINT pk_daily_snapshots PRIMARY KEY (snap_date, acc_id),
    CONSTRAINT ck_daily_snapshots_currency CHECK (currency = UPPER(currency))
);

CREATE INDEX ix_daily_snapshots_currency 
    ON public.daily_snapshots (currency);

CREATE INDEX ix_daily_snapshots_date 
    ON public.daily_snapshots (snap_date);

COMMENT ON TABLE public.daily_snapshots IS 
    'Daily account balance snapshots replicated from SQL Server operational database';
COMMENT ON COLUMN public.daily_snapshots.snap_date IS 
    'Snapshot date (typically current date when replicated)';
COMMENT ON COLUMN public.daily_snapshots.acc_id IS 
    'Account identifier from source system';
COMMENT ON COLUMN public.daily_snapshots.end_balance IS 
    'Account balance as of snap_date (can be negative)';
COMMENT ON COLUMN public.daily_snapshots.currency IS 
    'ISO 4217 currency code (USD, EUR, GBP, etc.)';

-- ----------------------------------------------------------------------------
-- Table: account_rollups
-- Purpose: Aggregated balance totals by currency for summary reporting
-- ----------------------------------------------------------------------------
CREATE TABLE public.account_rollups (
    as_of_utc     TIMESTAMPTZ NOT NULL,
    currency      CHAR(3) NOT NULL,
    total_balance NUMERIC(20,2) NOT NULL,
    
    CONSTRAINT ck_account_rollups_currency CHECK (currency = UPPER(currency))
);

CREATE INDEX ix_account_rollups_timestamp 
    ON public.account_rollups (as_of_utc);

CREATE INDEX ix_account_rollups_currency 
    ON public.account_rollups (currency);

COMMENT ON TABLE public.account_rollups IS 
    'Currency-level rollups of account balances for reporting';
COMMENT ON COLUMN public.account_rollups.as_of_utc IS 
    'Timestamp when rollup was calculated (UTC with timezone)';
COMMENT ON COLUMN public.account_rollups.currency IS 
    'ISO 4217 currency code';
COMMENT ON COLUMN public.account_rollups.total_balance IS 
    'Sum of all account balances in this currency';

-- ----------------------------------------------------------------------------
-- Materialized View: Latest Snapshots
-- Purpose: Frequently accessed query for most recent snapshot per account
-- ----------------------------------------------------------------------------
CREATE MATERIALIZED VIEW public.mv_latest_snapshots AS
SELECT DISTINCT ON (acc_id)
    acc_id,
    snap_date,
    end_balance,
    currency
FROM public.daily_snapshots
ORDER BY acc_id, snap_date DESC;

CREATE UNIQUE INDEX ix_mv_latest_snapshots_acc_id 
    ON public.mv_latest_snapshots (acc_id);

COMMENT ON MATERIALIZED VIEW public.mv_latest_snapshots IS 
    'Most recent snapshot for each account (refresh after replication)';

-- Refresh function (to be called after replication)
CREATE OR REPLACE FUNCTION public.refresh_latest_snapshots()
RETURNS void AS $$
BEGIN
    REFRESH MATERIALIZED VIEW CONCURRENTLY public.mv_latest_snapshots;
END;
$$ LANGUAGE plpgsql;

-- ----------------------------------------------------------------------------
-- Sample Query: Currency Summary Report
-- Purpose: Demo query for reporting use case
-- ----------------------------------------------------------------------------

-- View for currency summary
CREATE VIEW public.vw_currency_summary AS
SELECT 
    currency,
    COUNT(DISTINCT acc_id) AS account_count,
    SUM(end_balance) AS total_balance,
    AVG(end_balance) AS avg_balance,
    MIN(end_balance) AS min_balance,
    MAX(end_balance) AS max_balance
FROM public.daily_snapshots
WHERE snap_date = CURRENT_DATE
GROUP BY currency
ORDER BY total_balance DESC;

COMMENT ON VIEW public.vw_currency_summary IS 
    'Summary statistics by currency for today''s snapshots';

-- ============================================================================
-- Grant Permissions (for replication user)
-- ============================================================================

-- Note: Adjust username as needed for your environment
-- GRANT INSERT, UPDATE, SELECT ON public.daily_snapshots TO cblr_repl_user;
-- GRANT INSERT, SELECT ON public.account_rollups TO cblr_repl_user;
-- GRANT SELECT ON public.mv_latest_snapshots TO cblr_repl_user;
-- GRANT EXECUTE ON FUNCTION public.refresh_latest_snapshots() TO cblr_repl_user;

-- ============================================================================
-- End of PostgreSQL DDL
-- ============================================================================

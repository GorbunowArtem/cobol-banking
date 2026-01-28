-- ============================================================================
-- SQL Server DDL: COBOL Banking Data Lineage - Operational Database
-- ============================================================================
-- Database: cblr_ops
-- Purpose: Operational system of record for banking transactions,
--          ledger entries, balances, and lineage events
-- Target: Amazon RDS SQL Server
-- ============================================================================

USE cblr_ops;
GO

-- ----------------------------------------------------------------------------
-- Table: Transactions
-- Purpose: Retail banking transactions ingested from CSV files
-- ----------------------------------------------------------------------------
CREATE TABLE dbo.Transactions (
    TX_ID         BIGINT IDENTITY(1,1) PRIMARY KEY,
    ACC_ID        VARCHAR(32) NOT NULL,
    AMOUNT        DECIMAL(18,2) NOT NULL,
    CURRENCY      CHAR(3) NOT NULL,
    TX_TS_UTC     DATETIME2 NOT NULL,
    TX_TYPE       VARCHAR(32) NOT NULL,
    
    CONSTRAINT CK_Transactions_Currency CHECK (CURRENCY = UPPER(CURRENCY)),
    CONSTRAINT CK_Transactions_Amount CHECK (AMOUNT BETWEEN -1000000.00 AND 1000000.00)
);

CREATE NONCLUSTERED INDEX IX_Transactions_AccCurrency 
    ON dbo.Transactions (ACC_ID, CURRENCY);

CREATE NONCLUSTERED INDEX IX_Transactions_Timestamp 
    ON dbo.Transactions (TX_TS_UTC);
GO

-- ----------------------------------------------------------------------------
-- Table: LedgerEntries
-- Purpose: Double-entry accounting records derived from transactions
-- ----------------------------------------------------------------------------
CREATE TABLE dbo.LedgerEntries (
    ENTRY_ID      BIGINT IDENTITY(1,1) PRIMARY KEY,
    TX_ID         BIGINT NOT NULL,
    ACC_ID        VARCHAR(32) NOT NULL,
    DEBIT         DECIMAL(18,2) NULL,
    CREDIT        DECIMAL(18,2) NULL,
    CURRENCY      CHAR(3) NOT NULL,
    POSTED_TS_UTC DATETIME2 NOT NULL,
    
    CONSTRAINT FK_LedgerEntries_Transactions FOREIGN KEY (TX_ID) 
        REFERENCES dbo.Transactions(TX_ID),
    CONSTRAINT CK_LedgerEntries_DebitCredit CHECK (
        (DEBIT IS NOT NULL AND CREDIT IS NULL) OR 
        (DEBIT IS NULL AND CREDIT IS NOT NULL)
    ),
    CONSTRAINT CK_LedgerEntries_Currency CHECK (CURRENCY = UPPER(CURRENCY))
);

CREATE NONCLUSTERED INDEX IX_LedgerEntries_AccCurrency 
    ON dbo.LedgerEntries (ACC_ID, CURRENCY);

CREATE NONCLUSTERED INDEX IX_LedgerEntries_TxId 
    ON dbo.LedgerEntries (TX_ID);

CREATE NONCLUSTERED INDEX IX_LedgerEntries_Timestamp 
    ON dbo.LedgerEntries (POSTED_TS_UTC);
GO

-- ----------------------------------------------------------------------------
-- Table: AccountBalances
-- Purpose: Current balance for each account-currency combination
-- ----------------------------------------------------------------------------
CREATE TABLE dbo.AccountBalances (
    ACC_ID        VARCHAR(32) NOT NULL,
    CURRENCY      CHAR(3) NOT NULL,
    BALANCE       DECIMAL(18,2) NOT NULL,
    AS_OF_UTC     DATETIME2 NOT NULL,
    
    CONSTRAINT PK_AccountBalances PRIMARY KEY (ACC_ID, CURRENCY),
    CONSTRAINT CK_AccountBalances_Currency CHECK (CURRENCY = UPPER(CURRENCY))
);

CREATE NONCLUSTERED INDEX IX_AccountBalances_Timestamp 
    ON dbo.AccountBalances (AS_OF_UTC);
GO

-- ----------------------------------------------------------------------------
-- Table: LineageEvents
-- Purpose: Runtime data transformation metadata for lineage tracking
-- ----------------------------------------------------------------------------
CREATE TABLE dbo.LineageEvents (
    EVENT_ID        BIGINT IDENTITY(1,1) PRIMARY KEY,
    PROGRAM         VARCHAR(64) NOT NULL,
    SRC_ENGINE      VARCHAR(32) NULL,
    SRC_SCHEMA      VARCHAR(64) NULL,
    SRC_TABLE       VARCHAR(64) NULL,
    SRC_COLS        VARCHAR(MAX) NULL,
    TGT_ENGINE      VARCHAR(32) NOT NULL,
    TGT_SCHEMA      VARCHAR(64) NOT NULL,
    TGT_TABLE       VARCHAR(64) NOT NULL,
    TGT_COLS        VARCHAR(MAX) NULL,
    TRANSFORM_KIND  VARCHAR(32) NOT NULL,
    TRANSFORM_EXPR  VARCHAR(MAX) NULL,
    COMMIT_SHA      VARCHAR(40) NULL,
    RUN_ID          VARCHAR(64) NOT NULL,
    TS_UTC          DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    
    CONSTRAINT CK_LineageEvents_TransformKind CHECK (
        TRANSFORM_KIND IN ('ingest', 'posting', 'aggregate', 'replication')
    )
);

CREATE NONCLUSTERED INDEX IX_LineageEvents_RunId 
    ON dbo.LineageEvents (RUN_ID, TS_UTC);

CREATE NONCLUSTERED INDEX IX_LineageEvents_Program 
    ON dbo.LineageEvents (PROGRAM);

CREATE NONCLUSTERED INDEX IX_LineageEvents_Timestamp 
    ON dbo.LineageEvents (TS_UTC);
GO

-- ----------------------------------------------------------------------------
-- Table: PostingAudit
-- Purpose: Operational audit trail for program executions
-- ----------------------------------------------------------------------------
CREATE TABLE dbo.PostingAudit (
    AUDIT_ID      BIGINT IDENTITY(1,1) PRIMARY KEY,
    PROGRAM       VARCHAR(64) NOT NULL,
    RUN_ID        VARCHAR(64) NOT NULL,
    ROWS_IN       INT NOT NULL,
    ROWS_OUT      INT NOT NULL,
    TS_UTC        DATETIME2 NOT NULL DEFAULT SYSUTCDATETIME(),
    
    CONSTRAINT CK_PostingAudit_RowCounts CHECK (ROWS_IN >= 0 AND ROWS_OUT >= 0)
);

CREATE NONCLUSTERED INDEX IX_PostingAudit_RunId 
    ON dbo.PostingAudit (RUN_ID);

CREATE NONCLUSTERED INDEX IX_PostingAudit_Timestamp 
    ON dbo.PostingAudit (TS_UTC);
GO

-- ----------------------------------------------------------------------------
-- Views for Replication
-- Purpose: Simplified read-only views for cross-database replication
-- ----------------------------------------------------------------------------

-- View: Daily balances for replication to reporting DB
CREATE VIEW dbo.vw_DailyBalances AS
SELECT 
    CAST(GETUTCDATE() AS DATE) AS snap_date,
    ACC_ID,
    BALANCE,
    CURRENCY
FROM dbo.AccountBalances;
GO

-- View: Currency rollups for reporting
CREATE VIEW dbo.vw_CurrencyRollups AS
SELECT 
    GETUTCDATE() AS as_of_utc,
    CURRENCY,
    SUM(BALANCE) AS total_balance
FROM dbo.AccountBalances
GROUP BY CURRENCY;
GO

-- ============================================================================
-- Sample Data Seed (for demo purposes)
-- ============================================================================

-- Note: Sample data will be inserted via test runner scripts
-- The following is for reference only

/*
INSERT INTO dbo.Transactions (ACC_ID, AMOUNT, CURRENCY, TX_TS_UTC, TX_TYPE) VALUES
('ACC001', 100.50, 'USD', '2026-01-28T10:30:00', 'DEPOSIT'),
('ACC002', -25.00, 'USD', '2026-01-28T10:35:00', 'WITHDRAWAL'),
('ACC003', 500.00, 'EUR', '2026-01-28T10:40:00', 'TRANSFER'),
('ACC001', 75.25, 'USD', '2026-01-28T10:45:00', 'DEPOSIT'),
('ACC004', -150.00, 'GBP', '2026-01-28T10:50:00', 'WITHDRAWAL');
*/

-- ============================================================================
-- End of SQL Server DDL
-- ============================================================================

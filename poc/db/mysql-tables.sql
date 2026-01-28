-- =========================================================
-- MySQL DDL FOR ACCOUNT TRANSFORMATION APPLICATION
-- Compatible with AWS Aurora MySQL
-- =========================================================

-- Use or create the banking database
CREATE DATABASE IF NOT EXISTS banking;

USE banking;

-- ---------------------------------------------------------
-- SOURCE TABLE: CustomerAccounts
-- ---------------------------------------------------------
DROP TABLE IF EXISTS CustomerAccounts;

CREATE TABLE CustomerAccounts (
    AccountID INT NOT NULL AUTO_INCREMENT,
    CustomerID INT NOT NULL,
    CustomerName VARCHAR(100) NOT NULL,
    AccountNumber VARCHAR(20) NOT NULL,
    AccountType VARCHAR(20) NOT NULL,
    Balance DECIMAL(15, 2) DEFAULT 0.00,
    BranchCode VARCHAR(10),
    KYCStatus VARCHAR(20),
    RiskScore SMALLINT DEFAULT 0,
    AccountStatus VARCHAR(20) DEFAULT 'Active',
    CreatedDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    LastUpdated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    PRIMARY KEY (AccountID),
    UNIQUE KEY UK_AccountNumber (AccountNumber),
    KEY IDX_CustomerAccounts_Status (AccountStatus),
    KEY IDX_CustomerAccounts_Customer (CustomerID),
    KEY IDX_CustomerAccounts_Branch (BranchCode),
    CONSTRAINT CHK_AccountType CHECK (
        AccountType IN (
            'Checking',
            'Savings',
            'Investment',
            'Credit'
        )
    ),
    CONSTRAINT CHK_AccountStatus CHECK (
        AccountStatus IN (
            'Active',
            'Inactive',
            'Closed',
            'Suspended'
        )
    )
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4;

-- ---------------------------------------------------------
-- TARGET TABLE: AccountTransactions
-- ---------------------------------------------------------
DROP TABLE IF EXISTS AccountTransactions;

CREATE TABLE AccountTransactions (
    TransactionID BIGINT NOT NULL AUTO_INCREMENT,
    AccountID INT NOT NULL,
    CustomerID INT NOT NULL,
    AccountNumber VARCHAR(20) NOT NULL,
    Description VARCHAR(200),
    TransactionSubType VARCHAR(30),
    Amount DECIMAL(15, 2) DEFAULT 0.00,
    RunningBalance DECIMAL(15, 2) DEFAULT 0.00,
    BranchCode VARCHAR(10),
    ReferenceNumber VARCHAR(50),
    Channel VARCHAR(30) DEFAULT 'Manual',
    TransactionDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ProcessingStatus VARCHAR(20) DEFAULT 'PENDING',
    CreatedTimestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (TransactionID),
    KEY IDX_AccountTrans_Account (AccountID),
    KEY IDX_AccountTrans_Customer (CustomerID),
    KEY IDX_AccountTrans_Date (TransactionDate),
    KEY IDX_AccountTrans_Status (ProcessingStatus),
    KEY IDX_AccountTrans_RefNum (ReferenceNumber),
    KEY IDX_AccountTrans_Channel (Channel)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4;

-- ---------------------------------------------------------
-- SAMPLE DATA FOR TESTING
-- ---------------------------------------------------------
INSERT INTO
    CustomerAccounts (
        CustomerID,
        CustomerName,
        AccountNumber,
        AccountType,
        Balance,
        BranchCode,
        KYCStatus,
        RiskScore,
        AccountStatus
    )
VALUES (
        5001,
        'John Smith',
        'CHK-2024-001',
        'Checking',
        5000.00,
        'BR-NYC-01',
        'VERIFIED',
        25,
        'Active'
    ),
    (
        5002,
        'Mary Johnson',
        'SAV-2024-002',
        'Savings',
        15000.00,
        'BR-NYC-01',
        'VERIFIED',
        10,
        'Active'
    ),
    (
        5003,
        'Robert Brown',
        'INV-2024-003',
        'Investment',
        50000.00,
        'BR-LA-01',
        'VERIFIED',
        45,
        'Active'
    ),
    (
        5004,
        'Patricia Davis',
        'CRD-2024-004',
        'Credit',
        -2500.00,
        'BR-CHI-01',
        'PENDING',
        65,
        'Active'
    ),
    (
        5005,
        'Michael Wilson',
        'CHK-2024-005',
        'Checking',
        7500.00,
        'BR-NYC-01',
        'VERIFIED',
        20,
        'Active'
    ),
    (
        5006,
        'Jennifer Martinez',
        'SAV-2024-006',
        'Savings',
        25000.00,
        'BR-LA-01',
        'VERIFIED',
        15,
        'Active'
    ),
    (
        5007,
        'William Anderson',
        'CHK-2024-007',
        'Checking',
        3200.00,
        'BR-CHI-01',
        'VERIFIED',
        30,
        'Inactive'
    ),
    (
        5008,
        'Elizabeth Taylor',
        'SAV-2024-008',
        'Savings',
        18000.00,
        'BR-NYC-01',
        'VERIFIED',
        12,
        'Active'
    );

-- ---------------------------------------------------------
-- VERIFY DATA
-- ---------------------------------------------------------
SELECT 'Source Table Count' AS Description, COUNT(*) AS RecordCount
FROM CustomerAccounts;

SELECT 'Active Accounts' AS Description, COUNT(*) AS RecordCount
FROM CustomerAccounts
WHERE
    AccountStatus = 'Active';

-- ---------------------------------------------------------
-- USEFUL QUERIES
-- ---------------------------------------------------------

-- View transformed data
-- SELECT * FROM AccountTransactions ORDER BY TransactionID DESC LIMIT 10;

-- Compare source and target counts
-- SELECT
--     (SELECT COUNT(*) FROM CustomerAccounts WHERE AccountStatus='Active') AS SourceCount,
--     (SELECT COUNT(*) FROM AccountTransactions) AS TargetCount;
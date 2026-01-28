-- =========================================================
-- SQL DDL FOR ACCOUNT TRANSFORMATION APPLICATION
-- =========================================================
-- Database: BANKING
-- Purpose:  Create tables for Customer Account transformation
-- =========================================================

-- ---------------------------------------------------------
-- SOURCE TABLE: CustomerAccounts
-- ---------------------------------------------------------
CREATE TABLE CustomerAccounts (
    AccountID INTEGER NOT NULL,
    CustomerID INTEGER NOT NULL,
    CustomerName VARCHAR(100) NOT NULL,
    AccountNumber VARCHAR(20) NOT NULL,
    AccountType VARCHAR(20) NOT NULL,
    Balance DECIMAL(15, 2) DEFAULT 0.00,
    BranchCode VARCHAR(10),
    KYCStatus VARCHAR(20),
    RiskScore SMALLINT DEFAULT 0,
    AccountStatus VARCHAR(20) DEFAULT 'Active',
    CreatedDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    LastUpdated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_CustomerAccounts PRIMARY KEY (AccountID),
    CONSTRAINT UK_AccountNumber UNIQUE (AccountNumber),
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
);

CREATE INDEX IDX_CustomerAccounts_Status ON CustomerAccounts (AccountStatus);

CREATE INDEX IDX_CustomerAccounts_Customer ON CustomerAccounts (CustomerID);

CREATE INDEX IDX_CustomerAccounts_Branch ON CustomerAccounts (BranchCode);

-- ---------------------------------------------------------
-- TARGET TABLE: AccountTransactions
-- ---------------------------------------------------------
CREATE TABLE AccountTransactions (
    TransactionID BIGINT NOT NULL GENERATED ALWAYS AS IDENTITY,
    AccountID INTEGER NOT NULL,
    CustomerID INTEGER NOT NULL,
    AccountNumber VARCHAR(20) NOT NULL,
    Description VARCHAR(200),
    TransactionSubType VARCHAR(30),
    Amount DECIMAL(15, 2) DEFAULT 0.00,
    RunningBalance DECIMAL(15, 2) DEFAULT 0.00,
    BranchCode VARCHAR(10),
    ReferenceNumber VARCHAR(50),
    TransactionDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ProcessingStatus VARCHAR(20) DEFAULT 'PENDING',
    CreatedTimestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT PK_AccountTransactions PRIMARY KEY (TransactionID)
);

CREATE INDEX IDX_AccountTrans_Account ON AccountTransactions (AccountID);

CREATE INDEX IDX_AccountTrans_Customer ON AccountTransactions (CustomerID);

CREATE INDEX IDX_AccountTrans_Date ON AccountTransactions (TransactionDate);

CREATE INDEX IDX_AccountTrans_Status ON AccountTransactions (ProcessingStatus);

CREATE INDEX IDX_AccountTrans_RefNum ON AccountTransactions (ReferenceNumber);

-- ---------------------------------------------------------
-- SAMPLE DATA FOR TESTING
-- ---------------------------------------------------------
INSERT INTO
    CustomerAccounts (
        AccountID,
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
        1001,
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
        1002,
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
        1003,
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
        1004,
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
        1005,
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
        1006,
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
        1007,
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
        1008,
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
-- GRANTS (Adjust as needed for your environment)
-- ---------------------------------------------------------
-- GRANT SELECT ON CustomerAccounts TO PUBLIC;
-- GRANT INSERT ON AccountTransactions TO PUBLIC;

COMMIT;
# COBOL Account Transformation Application

## Overview

This application transforms data from the `CustomerAccounts` table to the `AccountTransactions` table using embedded SQL in COBOL.

## Files Created

### 1. **ACCTXFRM.cbl**

Main COBOL program that:

- Declares a cursor to fetch active customer accounts
- Fetches records from CustomerAccounts table
- Transforms data according to business rules
- Inserts transformed records into AccountTransactions table
- Provides progress tracking and error handling

### 2. **ACCTXFRM.jcl**

JCL (Job Control Language) file that:

- Precompiles the COBOL program with DB2
- Compiles the COBOL source code
- Links the program with DB2 libraries
- Binds the DB2 plan
- Executes the transformation program

### 3. **db/table-definitions.sql**

SQL DDL script that:

- Creates CustomerAccounts table (source)
- Creates AccountTransactions table (target)
- Creates necessary indexes
- Inserts sample test data

## Prerequisites

### Environment Requirements

- IBM z/OS Mainframe environment
- COBOL compiler (Enterprise COBOL v6.3 or later)
- DB2 database (v12 or later)
- TSO/ISPF access
- JES2/JES3 job submission capability

### Database Setup

- DB2 subsystem must be running
- Database connection configured
- User must have appropriate privileges:
  - SELECT on CustomerAccounts
  - INSERT on AccountTransactions
  - BIND privilege for DB2 plans

## Installation Steps

### Step 1: Set Up Database Tables

```sql
-- Connect to your DB2 subsystem
db2 connect to BANKING

-- Run the table creation script
db2 -tvf db/table-definitions.sql

-- Verify tables were created
db2 "SELECT COUNT(*) FROM CustomerAccounts"
```

### Step 2: Upload Files to Mainframe

#### Option A: Using FTP

```bash
# Upload COBOL source
ftp mainframe.yourcompany.com
> put ACCTXFRM.cbl 'USERID.SOURCE.COBOL(ACCTXFRM)'
> put ACCTXFRM.jcl 'USERID.JCL(ACCTXFRM)'
> quit
```

#### Option B: Using ISPF

1. Upload files using ISPF file transfer (IND$FILE)
2. Place ACCTXFRM.cbl in your COBOL source library
3. Place ACCTXFRM.jcl in your JCL library

### Step 3: Update JCL Configuration

Edit `ACCTXFRM.jcl` and update the following:

- Replace `&SYSUID` with your actual TSO user ID
- Update DB2 subsystem name (default: DB2P)
- Verify dataset names match your naming conventions
- Update library names for your environment:
  - `DB2.V12R1.SDSNLOAD` (DB2 load library)
  - `IGY.V6R3M0.SIGYCOMP` (COBOL compiler library)
  - `CEE.SCEELKED` (LE runtime library)

### Step 4: Create Required Datasets

Create the following datasets if they don't exist:

```jcl
//CREATE   EXEC PGM=IEFBR14
//DBRMLIB  DD DSN=USERID.DBRMLIB.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
//LOADLIB  DD DSN=USERID.LOAD.LIB,DISP=(NEW,CATLG),
//            SPACE=(TRK,(10,5,5)),
//            DCB=(RECFM=U,BLKSIZE=32760)
//SRCLIB   DD DSN=USERID.SOURCE.COBOL,DISP=(NEW,CATLG),
//            SPACE=(TRK,(10,5,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)
```

## Running the Application

### Method 1: Submit JCL Job (Recommended)

```
1. Log into TSO/ISPF
2. Navigate to your JCL library
3. Open ACCTXFRM.jcl
4. Enter SUB on command line to submit
5. Check job output in SDSF or output queue
```

### Method 2: Individual Steps

If you prefer to run steps individually:

#### Precompile

```jcl
//PRECOMP  EXEC PGM=DSNHPC,PARM='HOST(COBOL)'
// (see step 1 in ACCTXFRM.jcl)
```

#### Compile

```jcl
//COMPILE  EXEC PGM=IGYCRCTL,PARM='LIB,OBJECT,RENT'
// (see step 2 in ACCTXFRM.jcl)
```

#### Link-Edit

```jcl
//LKED     EXEC PGM=IEWL
// (see step 3 in ACCTXFRM.jcl)
```

#### Bind Plan

```jcl
DSN SYSTEM(DB2P)
BIND PLAN(ACCTXFRM) MEMBER(ACCTXFRM) ACTION(REPLACE)
END
```

#### Execute

```jcl
DSN SYSTEM(DB2P)
RUN PROGRAM(ACCTXFRM) PLAN(ACCTXFRM)
END
```

### Method 3: DB2I Panel (Alternative)

```
1. Enter ISPF Option =DB2I
2. Select option 1 (SPUFI) or 3 (BIND/REBIND/FREE)
3. Follow prompts to bind plan
4. Select option 4 (RUN) to execute program
```

## Verifying Results

After running the program, verify the transformation:

```sql
-- Check how many records were transformed
SELECT COUNT(*) AS TransformedRecords
FROM AccountTransactions;

-- View sample transformed data
SELECT
    AccountID,
    CustomerID,
    AccountNumber,
    Description,
    TransactionSubType,
    Amount,
    ReferenceNumber,
    ProcessingStatus
FROM AccountTransactions
ORDER BY AccountID
FETCH FIRST 10 ROWS ONLY;

-- Verify all active accounts were processed
SELECT
    CA.AccountID,
    CA.AccountNumber,
    CA.AccountStatus,
    CASE WHEN AT.AccountID IS NULL
         THEN 'NOT PROCESSED'
         ELSE 'PROCESSED'
    END AS Status
FROM CustomerAccounts CA
LEFT JOIN AccountTransactions AT
    ON CA.AccountID = AT.AccountID
WHERE CA.AccountStatus = 'Active';
```

## Expected Output

The program will display:

```
================================================
ACCOUNT TRANSFORMATION PROGRAM STARTED
================================================
CURSOR OPENED SUCCESSFULLY
PROCESSED 1000 RECORDS
PROCESSED 2000 RECORDS
...
================================================
TRANSFORMATION COMPLETED SUCCESSFULLY
RECORDS READ:     7
RECORDS INSERTED: 7
RECORDS FAILED:   0
================================================
```

## Transformation Rules

The program applies these transformations:

1. **Description**: Concatenates "Customer: " + customer name + " - Account Inquiry"
2. **SubType**: Maps based on account type:
   - Checking → CHK-Inquiry
   - Savings → SAV-Inquiry
   - Investment → INV-Inquiry
   - Credit → CRD-Inquiry
   - Other → GEN-Inquiry
3. **Reference Number**: Format "RSK-{RiskScore}-{AccountNumber}"
4. **Amount & Running Balance**: Both set to account balance
5. **Processing Status**: Set to "PROCESSED"

## Troubleshooting

### Common Issues

**SQLCODE -204**: Table not found

- Solution: Run table-definitions.sql first

**SQLCODE -911**: Deadlock or timeout

- Solution: Ensure no other processes are locking the tables

**SQLCODE -551**: Insufficient privileges

- Solution: Grant SELECT on CustomerAccounts and INSERT on AccountTransactions

**JCL Error: Dataset not found**

- Solution: Create required datasets (DBRMLIB, LOAD.LIB, SOURCE.COBOL)

**Compilation Error: SQLCA not found**

- Solution: Ensure DB2 SDSNSAMP library is in SYSLIB concatenation

### Checking Job Output

```
1. Enter SDSF (usually =SD or =S.ST)
2. Find job ACCTXFRM in output queue
3. Enter S next to job to view output
4. Check each step for CC 0000 (successful completion)
5. Review SYSPRINT for any error messages
```

## Performance Considerations

- Program processes records one at a time (cursor fetch)
- Commits all changes at end of program
- For large datasets (>100K records), consider:
  - Adding periodic commits (every 10,000 records)
  - Using array fetching for better performance
  - Running during off-peak hours

## Maintenance

### Re-running the Program

To re-run the transformation:

```sql
-- Clear previous transformation data
DELETE FROM AccountTransactions
WHERE ProcessingStatus = 'PROCESSED';
COMMIT;
```

Then resubmit the JCL.

### Modifying Transformation Logic

1. Edit ACCTXFRM.cbl
2. Update the transformation in section 2300-TRANSFORM-DATA
3. Resubmit ACCTXFRM.jcl to recompile and execute

## Support Files Location

```
cobol-banking/
├── ACCTXFRM.cbl           # Main COBOL program
├── ACCTXFRM.jcl           # JCL to compile and run
├── db/
│   ├── table-definitions.sql  # Table DDL and sample data
│   └── creation           # (your existing file)
└── README.md              # This file
```

## Contact & Support

For issues or questions:

- Review SYSPRINT output in job results
- Check SQLCODE in error messages
- Verify DB2 subsystem is active
- Ensure all prerequisites are met

---
**Version:** 1.0
**Last Updated:** January 28, 2026
**Author:** Banking System Team

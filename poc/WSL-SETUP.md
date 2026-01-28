# Running COBOL with MySQL Aurora on WSL

## Quick Setup Guide

### 1. Install GnuCOBOL on WSL (Ubuntu)

```bash
# Update packages
sudo apt update

# Install GnuCOBOL compiler
sudo apt install gnucobol4 -y

# Verify installation
cobc --version
```

### 2. Install MySQL Client

```bash
# Install MySQL client
sudo apt install mysql-client -y

# Verify installation
mysql --version
```

### 3. Set Up MySQL Aurora Connection

Create a configuration file with your Aurora credentials:

```bash
# Create .my.cnf for storing credentials (optional, more secure)
cat > ~/.my.cnf << 'EOF'
[client]
host=your-aurora-cluster.region.rds.amazonaws.com
user=admin
password=your-password
database=banking
EOF

# Secure the file
chmod 600 ~/.my.cnf
```

Or export environment variables:

```bash
export MYSQL_HOST="your-aurora-cluster.region.rds.amazonaws.com"
export MYSQL_USER="admin"
export MYSQL_PASS="your-password"
export MYSQL_DB="banking"
```

### 4. Create Database Tables in Aurora

```bash
# Navigate to project directory (from WSL)
cd /mnt/c/projects/bode-dev/git/cobol-banking

# Create tables in Aurora MySQL
mysql -h $MYSQL_HOST -u $MYSQL_USER -p$MYSQL_PASS < db/mysql-tables.sql

# Verify tables were created
mysql -h $MYSQL_HOST -u $MYSQL_USER -p$MYSQL_PASS -D banking -e "SHOW TABLES;"
```

### 5. Make Shell Scripts Executable

```bash
chmod +x extract-accounts.sh
chmod +x load-transactions.sh
chmod +x run-transformation.sh
```

## Running the Complete Transformation

### Option 1: Automated Pipeline (Recommended)

```bash
# Run the complete pipeline
./run-transformation.sh
```

This will:

1. Extract data from Aurora MySQL
2. Compile the COBOL program
3. Run the transformation
4. Load results back to Aurora

### Option 2: Step-by-Step Execution

```bash
# Step 1: Extract data from MySQL Aurora
./extract-accounts.sh

# Step 2: Compile COBOL program
cobc -x -free ACCTXFRM-MYSQL.cbl -o acctxfrm

# Step 3: Run transformation
./acctxfrm

# Step 4: Load results to MySQL
./load-transactions.sh
```

### Option 3: Manual Testing (No Database)

Create test data file manually:

```bash
# Create sample input file
cat > accounts.dat << 'EOF'
1001|5001|John Smith|CHK-2024-001|Checking|5000.00|BR-NYC-01|VERIFIED|25|Active|2024-01-01|2024-01-28
1002|5002|Mary Johnson|SAV-2024-002|Savings|15000.00|BR-NYC-01|VERIFIED|10|Active|2024-01-01|2024-01-28
1003|5003|Robert Brown|INV-2024-003|Investment|50000.00|BR-LA-01|VERIFIED|45|Active|2024-01-01|2024-01-28
EOF

# Compile and run
cobc -x -free ACCTXFRM-MYSQL.cbl -o acctxfrm
./acctxfrm

# View generated SQL
cat transactions.sql
```

## Verifying Results

### Check in MySQL Aurora

```bash
# Connect to Aurora
mysql -h $MYSQL_HOST -u $MYSQL_USER -p$MYSQL_PASS -D banking

# In MySQL prompt:
# Count transformed records
SELECT COUNT(*) FROM AccountTransactions;

# View sample data
SELECT AccountID, CustomerID, AccountNumber,
       TransactionSubType, Amount, ReferenceNumber
FROM AccountTransactions
LIMIT 10;

# Compare source vs target
SELECT
    (SELECT COUNT(*) FROM CustomerAccounts WHERE AccountStatus='Active') AS SourceActive,
    (SELECT COUNT(*) FROM AccountTransactions) AS Transformed;
```

## File Structure

```
cobol-banking/
├── ACCTXFRM-MYSQL.cbl      # COBOL program (MySQL version)
├── run-transformation.sh    # Complete automation script
├── extract-accounts.sh      # Extract data from Aurora
├── load-transactions.sh     # Load results to Aurora
├── db/
│   └── mysql-tables.sql    # MySQL table definitions
├── accounts.dat            # Input file (generated)
├── transactions.sql        # Output file (generated)
└── acctxfrm               # Compiled executable (generated)
```

## Troubleshooting

### Connection Issues

```bash
# Test Aurora connection
mysql -h $MYSQL_HOST -u $MYSQL_USER -p$MYSQL_PASS -e "SELECT 1;"

# Check security group allows your IP
# Aurora must allow inbound connections on port 3306
```

### Compilation Errors

```bash
# If you see "fixed format" errors, use -free flag
cobc -x -free ACCTXFRM-MYSQL.cbl

# For verbose output
cobc -x -free -v ACCTXFRM-MYSQL.cbl
```

### File Not Found

```bash
# Ensure you're in the right directory
pwd
cd /mnt/c/projects/bode-dev/git/cobol-banking

# Check files exist
ls -la *.sh *.cbl
```

### Line Ending Issues (Windows/Linux)

```bash
# Convert Windows line endings to Unix
sudo apt install dos2unix -y
dos2unix *.sh *.cbl
```

## Performance Tips

1. **For Large Datasets**: Modify the extract script to process in batches
2. **Network Latency**: Consider running the script on an EC2 instance in the same region as Aurora
3. **Indexes**: Ensure indexes exist on AccountStatus for fast extraction

## Security Best Practices

1. **Never commit credentials** - Use environment variables or AWS Secrets Manager
2. **Use IAM Authentication** for Aurora (requires additional setup)
3. **Encrypt connections** - Add `--ssl-mode=REQUIRED` to mysql commands
4. **Rotate passwords** regularly

## Example: Complete Run

```bash
# Set credentials
export MYSQL_HOST="my-cluster.cluster-xyz.us-east-1.rds.amazonaws.com"
export MYSQL_USER="admin"
export MYSQL_PASS="MySecurePassword123"
export MYSQL_DB="banking"

# Run transformation
./run-transformation.sh

# Expected output:
# ================================================
# COBOL Account Transformation Pipeline
# MySQL Aurora Version
# ================================================
#
# Step 1: Extracting data from MySQL...
# Successfully extracted 7 records
#
# Step 2: Compiling COBOL program...
# Compilation successful
#
# Step 3: Running transformation...
# ================================================
# ACCOUNT TRANSFORMATION PROGRAM STARTED
# Records read:     7
# Records processed: 7
# Records skipped:  0
# ================================================
#
# Step 4: Loading results to MySQL...
# Successfully loaded transactions
# Total PROCESSED records: 7
# ================================================
```

---
**Note**: This setup works with both AWS Aurora MySQL and standard MySQL databases.

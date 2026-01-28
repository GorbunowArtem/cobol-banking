# Feature Specification: COBOL Data Lineage Banking Component

**Feature Branch**: `001-cobol-data-lineage`  
**Created**: 2026-01-28  
**Status**: Draft  
**Input**: User description: "A minimal core banking component implemented in COBOL that ingests retail transactions, posts double‑entry ledger entries, maintains daily balances, and publishes reporting views. It demonstrates end‑to‑end data lineage traced directly from COBOL programs across two AWS‑hosted databases"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Transaction Processing with Lineage Capture (Priority: P1)

A data governance analyst needs to trace how retail banking transactions flow from initial CSV ingestion through the complete processing pipeline, including which COBOL programs touch the data and what transformations are applied at each step.

**Why this priority**: This is the core value proposition - demonstrating traceable data lineage through COBOL programs. Without this, the feature has no purpose.

**Independent Test**: Can be fully tested by ingesting a sample CSV file with 5 transactions, verifying they appear in the operational database, and checking that lineage events are recorded showing the source (CSV), target (Transactions table), and transformation logic.

**Acceptance Scenarios**:

1. **Given** a CSV file with 5 retail transactions, **When** the ingest program runs, **Then** all 5 transactions are inserted into the operational database and lineage events capture the CSV-to-database mapping
2. **Given** transactions in the operational database, **When** the posting program executes, **Then** double-entry ledger entries are created (one debit or credit per transaction) and lineage events record the transformation rules
3. **Given** completed lineage events in the system, **When** the lineage export runs, **Then** a CSV file is generated with all transformation steps traceable from source to destination

---

### User Story 2 - Cross-Database Replication Tracking (Priority: P2)

A compliance officer needs to verify that data replicated from the operational SQL Server database to the PostgreSQL reporting warehouse maintains complete lineage traceability across database engines.

**Why this priority**: Cross-database lineage is a key differentiator showing enterprise-grade data governance across heterogeneous systems, but depends on P1 functionality being operational first.

**Independent Test**: Can be fully tested by running the replication program and verifying that daily snapshots appear in PostgreSQL with lineage events showing the source database (SQL Server), target database (PostgreSQL), columns mapped, and replication timestamp.

**Acceptance Scenarios**:

1. **Given** account balances calculated in SQL Server, **When** the replication program runs, **Then** daily snapshots are written to PostgreSQL and lineage events capture the cross-database movement with engine-qualified identifiers (sqlserver→postgres)
2. **Given** currency-based rollups in PostgreSQL, **When** the analyst queries the reporting database, **Then** the data matches source calculations and lineage CSV shows the transformation chain from operational to reporting

---

### User Story 3 - Static Lineage Documentation Review (Priority: P3)

A developer reviewing COBOL code needs to understand data transformations without executing the programs by reading machine-parseable lineage annotations directly in the source code.

**Why this priority**: Static lineage complements runtime lineage and supports code review processes, but the system functions without it since runtime lineage captures actual execution.

**Independent Test**: Can be fully tested by opening COBOL source files and verifying that lineage comment blocks exist adjacent to transformation logic, following the defined grammar (PROGRAM, SRC, TGT, MAP fields).

**Acceptance Scenarios**:

1. **Given** the POST_LEDGER.cbl source file, **When** a developer reviews the code, **Then** lineage annotations clearly document the transformation from Transactions to LedgerEntries with the conditional logic for debit/credit assignment
2. **Given** any COBOL program with data transformations, **When** a lineage parser scans the comments, **Then** it can extract structured lineage metadata matching the runtime lineage events

---

### User Story 4 - Balance Calculation with Audit Trail (Priority: P2)

An operations manager needs to verify that account balances are correctly aggregated from ledger entries and that each calculation run is audited with row counts and timestamps for reconciliation purposes.

**Why this priority**: Balance accuracy is critical for banking operations, and audit trails support operational monitoring, but this is a downstream process from transaction ingestion.

**Independent Test**: Can be fully tested by posting ledger entries, running the balance recalculation program, and verifying that AccountBalances table reflects correct aggregations and PostingAudit table contains run metrics.

**Acceptance Scenarios**:

1. **Given** 10 ledger entries for account ACC123 (5 credits totaling $500, 5 debits totaling $200), **When** balance recalculation runs, **Then** AccountBalances shows $300 for ACC123 and lineage events record the aggregation logic
2. **Given** a balance calculation run completes, **When** the operations manager checks the audit table, **Then** PostingAudit contains the program name, run ID, input row count, output row count, and timestamp

---

### Edge Cases

- What happens when the CSV file contains invalid data (negative account IDs, missing currency codes, malformed amounts)? System must reject invalid rows and log errors without stopping processing of valid rows.
- How does the system handle duplicate transaction IDs in the same batch? System must detect duplicates and either skip or fail the batch based on configured idempotency rules.
- What happens when SQL Server or PostgreSQL is unavailable during processing? Programs must fail gracefully with clear error messages and allow safe retry without data corruption.
- How does lineage capture work when a program execution fails mid-stream? Lineage events must be transactional - either all events for a successful run are committed, or none are (rollback on failure).
- What happens when the .version file (commit SHA) is missing or corrupted? System should use a default value like "UNKNOWN" rather than failing, allowing lineage capture to continue.
- How are concurrent runs of the same program distinguished in lineage events? Each run must generate a unique run_id (timestamp-based or GUID) to separate lineage events.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST read retail transaction data from CSV files in a configured input directory
- **FR-002**: System MUST validate transaction data (account ID format, currency code, amount range, timestamp format) before database insertion
- **FR-003**: System MUST insert validated transactions into the operational SQL Server database (dbo.Transactions table)
- **FR-004**: System MUST convert each transaction into double-entry ledger format, creating debit entries for negative amounts and credit entries for positive amounts
- **FR-005**: System MUST maintain account balances by aggregating all ledger entries per account and currency
- **FR-006**: System MUST replicate calculated balances from SQL Server to PostgreSQL reporting database on demand
- **FR-007**: System MUST capture static lineage annotations in COBOL source code comments following the defined grammar (PROGRAM, SRC, TGT, MAP, REF, PURPOSE, TX_BOUNDARY)
- **FR-008**: System MUST record runtime lineage events for every data transformation, including source table, target table, column mappings, transformation expression, program name, and timestamp
- **FR-009**: System MUST export all lineage events to a CSV file suitable for ingestion by data governance tools
- **FR-010**: System MUST include git commit SHA in lineage events to tie data transformations to specific code versions
- **FR-011**: System MUST distinguish lineage events by transformation type: ingest, posting, aggregate, replication
- **FR-012**: System MUST use engine-qualified identifiers (sqlserver, postgres) in lineage events for cross-database transformations
- **FR-013**: System MUST record audit metrics (program name, run ID, input row count, output row count, timestamp) for each posting run
- **FR-014**: System MUST maintain transactional integrity - database writes and lineage events must commit together or roll back together
- **FR-015**: System MUST generate unique run identifiers for each program execution to correlate lineage events

### Key Entities *(include if feature involves data)*

- **Transaction**: Represents a retail banking transaction with account ID, amount (positive or negative), currency code, timestamp, and transaction type. Source data from CSV files.
- **LedgerEntry**: Represents a double-entry accounting record with either a debit or credit amount, linked to the originating transaction. Multiple ledger entries can derive from one transaction.
- **AccountBalance**: Represents the current balance for an account in a specific currency, calculated by aggregating all ledger entries. Updated as of a specific timestamp.
- **LineageEvent**: Represents a single data transformation step with source/target database, schema, table, column lists, transformation logic, program name, commit SHA, run ID, and timestamp. Enables complete data lineage tracing.
- **PostingAudit**: Represents execution metrics for a program run including row counts and timing for operational monitoring and reconciliation.
- **DailySnapshot**: Represents point-in-time balance data replicated to the reporting warehouse, keyed by date and account ID.
- **AccountRollup**: Represents aggregated balance totals by currency for reporting purposes.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: System successfully processes 100 transactions from CSV to final reporting tables with complete lineage in under 5 minutes
- **SC-002**: Every data transformation (minimum 4 types: ingest, posting, aggregate, replication) generates lineage events with all required fields populated
- **SC-003**: Lineage CSV export contains all transformation steps in chronological order, enabling complete traceability from source CSV to reporting tables
- **SC-004**: Account balances calculated by the system match manual reconciliation of ledger entries with 100% accuracy
- **SC-005**: Cross-database replication completes successfully with data verified in both SQL Server and PostgreSQL reporting tables
- **SC-006**: Static lineage annotations in COBOL code are machine-parseable and match runtime lineage events for the same transformations
- **SC-007**: System handles missing or corrupted .version file gracefully without failing lineage capture
- **SC-008**: All database operations complete within transactions - no partial writes in case of errors
- **SC-009**: Documentation clearly explains the lineage strategy, run order for programs, and troubleshooting steps for common issues

## Scope & Boundaries *(mandatory)*

### In Scope

- CSV-based transaction ingestion with validation
- Double-entry accounting ledger posting
- Account balance aggregation and maintenance
- Cross-database replication from SQL Server to PostgreSQL
- Static lineage annotations in COBOL source code
- Runtime lineage event capture to operational database
- Lineage CSV export for governance tool ingestion
- Audit trail for program executions
- Sample data and test runner scripts
- Documentation of lineage strategy and program execution order

### Out of Scope

- Real-time payment processing or streaming data
- Fraud detection or transaction limits logic
- External system integrations (messaging, APIs, channels)
- High availability or disaster recovery configuration beyond default AWS RDS settings
- Production-grade security hardening (using simplified demo-level security)
- Automated lineage ingestion into OpenMetadata (manual process documented but not automated)
- User interface or web application for viewing lineage
- Performance optimization for large transaction volumes
- Multi-currency conversion or foreign exchange
- Customer or account master data management

## Assumptions *(mandatory)*

- AWS EC2 instance running Amazon Linux 2023 is provisioned and accessible
- COBOL runtime (Micro Focus Visual COBOL or GnuCOBOL) is installed and configured on the EC2 instance
- Amazon RDS SQL Server instance (database: cblr_ops) is provisioned and accessible from EC2
- Amazon Aurora PostgreSQL instance (database: cblr_report) is provisioned and accessible from EC2
- unixODBC is installed and configured with working DSNs for both databases
- Database credentials are available and stored securely on the EC2 instance
- EC2 instance has appropriate IAM roles and security group rules for database access
- Git repository is initialized and the .version file can be created with commit SHAs
- Input CSV files follow the expected format (account ID, amount, currency, timestamp, transaction type)
- All transactions in a single CSV batch use standard currency codes (USD, EUR, GBP, etc.)
- Double-entry accounting rules: positive amounts → credit, negative amounts → debit (absolute value)
- The operational database is the authoritative system of record; reporting database is read-only for analysts
- Programs run sequentially in a defined order (ingest → post → balance → replicate → export), not concurrently
- Idempotent reruns are acceptable - programs can be safely re-executed with the same input data
- This is a demonstration system - production-level monitoring and alerting are not required
- Lineage CSV format is compatible with OpenMetadata ingestion based on prior demos

## Dependencies *(mandatory)*

- AWS EC2 instance with Amazon Linux 2023 operating system
- COBOL compiler and runtime environment (Micro Focus Visual COBOL preferred, GnuCOBOL acceptable)
- Amazon RDS SQL Server instance with database cblr_ops created
- Amazon Aurora PostgreSQL instance with database cblr_report created
- unixODBC and database drivers (ODBC Driver for SQL Server, psqlODBC)
- Git for version control and .version file generation
- Bash shell environment for test runner scripts
- Database DDL scripts executed (table creation for both SQL Server and PostgreSQL)
- Network connectivity and security group configuration allowing EC2→RDS and EC2→Aurora traffic
- Database credentials and ODBC DSN configuration files

## Non-Functional Considerations

### Performance
- Batch processing is acceptable - no real-time latency requirements
- Programs should complete processing of 100-1000 transactions within minutes
- Database queries should use appropriate indexes on account ID and transaction ID columns

### Security
- Database credentials stored encrypted on EC2 instance (demo-level encryption acceptable)
- Database connections use TLS/SSL for data in transit
- Least-privilege IAM roles for EC2 instance
- Security groups restrict database access to EC2 instance only
- No sensitive customer PII in demo data - use synthetic account identifiers

### Observability
- PostingAudit table captures per-run metrics for operational monitoring
- Programs log start and completion messages to stdout
- Errors written to stderr with sufficient detail for troubleshooting
- Lineage events provide complete audit trail of data transformations

### Reliability
- Programs wrapped in SQL transactions for atomicity
- Safe to re-run programs with same input (idempotent where possible)
- Graceful failure handling with clear error messages
- No data corruption if program fails mid-execution

### Maintainability
- COBOL code follows standard formatting conventions for readability
- Copybooks used for shared record layouts and reusable sections
- Static lineage annotations keep documentation in sync with code
- README.md provides clear setup and execution instructions

## Risk Assessment

### Technical Risks
- **Risk**: COBOL-to-database connectivity issues with ODBC drivers
  - **Mitigation**: Test both Micro Focus and GnuCOBOL; provide detailed ODBC configuration examples
  
- **Risk**: Cross-database replication failures due to schema differences
  - **Mitigation**: Keep schemas simple; use explicit column mappings; test replication thoroughly
  
- **Risk**: Lineage events missing data due to program errors
  - **Mitigation**: Wrap lineage logging in error handlers; use transactional commits

### Operational Risks
- **Risk**: AWS costs for running EC2 and databases continuously
  - **Mitigation**: Document resource shutdown procedures; use smallest viable instance sizes
  
- **Risk**: Incomplete or incorrect lineage metadata
  - **Mitigation**: Validate lineage CSV against expected transformations; compare static vs runtime lineage

### Process Risks
- **Risk**: Unclear program execution order leading to data inconsistencies
  - **Mitigation**: Document execution sequence in README; provide numbered test runner scripts
  
- **Risk**: Demo data not representative of real banking scenarios
  - **Mitigation**: Include realistic transaction types and amounts in sample CSV

## Open Questions

*None - the feature description is sufficiently detailed for implementation to proceed.*

# Implementation Plan: COBOL Data Lineage Banking Component

**Branch**: `001-cobol-data-lineage` | **Date**: 2026-01-28 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-cobol-data-lineage/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

A demonstration-ready COBOL banking application that processes retail transactions through a complete pipeline (CSV ingest → ledger posting → balance calculation → cross-database replication) while capturing comprehensive data lineage metadata at every transformation step. The system spans two AWS databases (SQL Server operational, PostgreSQL reporting) and exports lineage events to CSV for governance tool ingestion, showcasing end-to-end traceability in legacy banking systems.

## Technical Context

**Language/Version**: COBOL-85 or later (targeting Micro Focus Visual COBOL on Linux, with GnuCOBOL fallback)  
**Primary Dependencies**: unixODBC, ODBC Driver for SQL Server, psqlODBC, Git  
**Storage**: Amazon RDS SQL Server (cblr_ops operational), Amazon Aurora PostgreSQL (cblr_report reporting), CSV files  
**Testing**: Shell-based test runners (no formal unit tests per constitution)  
**Target Platform**: Amazon EC2 running Amazon Linux 2023  
**Project Type**: Single project (batch processing COBOL programs)  
**Performance Goals**: Process 100-1000 transactions with complete lineage in under 5 minutes  
**Constraints**: Sequential execution order (not concurrent), transactional integrity required, demo-ready presentation quality  
**Scale/Scope**: 5 COBOL programs, 3 copybooks, 7 database tables (SQL Server), 2 tables (PostgreSQL), 5 test runners, sample CSV data

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### ✅ I. Demo-Ready Code
**Status**: PASS  
**Evidence**: Feature spec emphasizes demonstration quality throughout. Success criteria SC-009 requires clear documentation. All programs designed to showcase COBOL capabilities with realistic banking domain examples.

### ✅ II. COBOL Best Practices
**Status**: PASS  
**Evidence**: Feature requires proper COBOL structure (copybooks for shared logic, standard divisions). Static lineage comments demonstrate documentation practices. Clean, readable code is an explicit requirement for demo walkthroughs.

### ✅ III. No Testing Required
**Status**: PASS  
**Evidence**: Constitution explicitly states "formal unit testing and test-driven development are not required." Feature provides shell-based test runners for demonstration purposes, not TDD. Focus is on working functionality, not test coverage.

### ✅ IV. Banking Domain Focus
**Status**: PASS  
**Evidence**: Feature implements core banking concepts: transaction processing, double-entry ledger, account balances, reconciliation audit trails. All entities (Transaction, LedgerEntry, AccountBalance) align with banking domain.

### ✅ V. Simplicity and Clarity
**Status**: PASS  
**Evidence**: Non-goals explicitly exclude complex features (fraud detection, HA/DR, real-time processing). Simple CSV input, straightforward double-entry rules, clear sequential execution order. Designed for audiences with varying COBOL expertise.

### ✅ Technical Standards: COBOL Standards
**Status**: PASS  
**Evidence**: COBOL-85+ specified in technical context. Copybooks enforce consistent record layouts. Static lineage comments provide meaningful documentation. File handling via ODBC with proper error checking.

### ✅ Technical Standards: Data Management
**Status**: PASS  
**Evidence**: Data structures are well-documented (7 entities with clear attributes). Simple schemas in both databases. CSV files for demo data. Self-documenting through lineage annotations.

### ✅ Demo Requirements: Presentation Quality
**Status**: PASS  
**Evidence**: SC-001 requires working end-to-end processing. Sample data included. Test runners provide clear execution steps. Documentation (quickstart, README) required to explain setup and troubleshooting.

### ✅ Demo Requirements: Feature Scope
**Status**: PASS  
**Evidence**: Each program is complete and independently demonstrable (TX_INBOUND, POST_LEDGER, BALANCE_RECALC, REPL_REPORTING, LINEAGE_EXPORT). No stubs or placeholders - all features fully implemented per spec.

### Constitution Gate Summary
**Overall Status**: ✅ **PASS** - All constitutional principles satisfied. No violations requiring justification. Feature is well-aligned with demo-focused, COBOL-showcasing, banking-domain objectives without testing overhead.

## Project Structure

### Documentation (this feature)

```text
specs/001-cobol-data-lineage/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
│   ├── databases/
│   │   ├── sqlserver-ddl.sql
│   │   └── postgresql-ddl.sql
│   ├── csv/
│   │   └── transaction-format.md
│   └── lineage/
│       └── lineage-csv-format.md
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
cobol-banking/
├── cobol/
│   ├── src/
│   │   ├── TX_INBOUND.cbl        # CSV ingestion program
│   │   ├── POST_LEDGER.cbl       # Double-entry posting
│   │   ├── BALANCE_RECALC.cbl    # Balance aggregation
│   │   ├── REPL_REPORTING.cbl    # Cross-DB replication
│   │   └── LINEAGE_EXPORT.cbl    # Lineage CSV export
│   └── copybooks/
│       ├── LINEAGE-LOGGER.cpy    # Lineage event logging
│       ├── RECORD-DEFS.cpy       # Common record layouts
│       └── DB-CONFIG.cpy         # DSN/env constants
├── config/
│   ├── odbc.ini                  # ODBC DSN configuration
│   └── odbcinst.ini              # ODBC driver configuration
├── sql/
│   ├── sqlserver/
│   │   └── ddl.sql               # Operational DB schema
│   └── postgres/
│       └── ddl.sql               # Reporting DB schema
├── data/
│   └── in/
│       └── transactions.csv      # Sample input data
├── lineage/
│   └── out/
│       └── .gitkeep              # Output directory for lineage.csv
├── tests/
│   ├── 01_seed_sqlserver.sh      # DDL + seed data
│   ├── 02_post_ledger.sh         # Run posting
│   ├── 03_balance.sh             # Run balance calc
│   ├── 04_repl.sh                # Run replication
│   └── 05_export_lineage.sh      # Run lineage export
├── docs/
│   └── README.md                 # Setup, lineage strategy, troubleshooting
└── .version                      # Git commit SHA for lineage tagging
```

**Structure Decision**: Single project structure chosen. This is a batch processing system with sequential COBOL programs - not a web or mobile application. All programs share common copybooks and operate on the same data pipeline. The structure separates concerns logically: source code (cobol/), configuration (config/), database schemas (sql/), data files (data/), lineage output (lineage/), test runners (tests/), and documentation (docs/).

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

*No violations identified. Constitution check passed completely.*

---

## Phase 0: Research (COMPLETE)

**Status**: ✅ **COMPLETE**  
**Output**: [research.md](research.md)

### Research Areas Covered

1. **COBOL-ODBC Database Connectivity**: Embedded SQL with unixODBC, dual DSN configuration
2. **Lineage Metadata Capture**: Dual-track approach (static comments + runtime events)
3. **Cross-Database Replication**: Application-level COBOL program with full lineage
4. **Double-Entry Ledger Logic**: Conditional debit/credit based on amount sign
5. **Git Commit SHA Integration**: .version file approach with graceful fallback
6. **CSV File Format**: Standard CSV with header, validation rules defined
7. **ODBC Configuration**: Separate DSNs with SSL/TLS for both databases
8. **Lineage CSV Export**: OpenMetadata-compatible flat CSV format

All technical unknowns resolved. Ready for implementation.

---

## Phase 1: Design & Contracts (COMPLETE)

**Status**: ✅ **COMPLETE**  
**Outputs**:
- [data-model.md](data-model.md) - 7 entities with validation rules and relationships
- [contracts/databases/sqlserver-ddl.sql](contracts/databases/sqlserver-ddl.sql) - Operational DB schema
- [contracts/databases/postgresql-ddl.sql](contracts/databases/postgresql-ddl.sql) - Reporting DB schema
- [contracts/csv/transaction-format.md](contracts/csv/transaction-format.md) - Input CSV specification
- [contracts/lineage/lineage-csv-format.md](contracts/lineage/lineage-csv-format.md) - Output CSV specification
- [quickstart.md](quickstart.md) - Setup and execution guide

### Design Decisions

**Data Model**:
- 5 entities in SQL Server (Transactions, LedgerEntries, AccountBalances, LineageEvents, PostingAudit)
- 2 entities in PostgreSQL (DailySnapshot, AccountRollup)
- Clear validation rules and state transitions defined
- Referential integrity enforced where appropriate

**Contracts**:
- Complete DDL for both databases with indexes and constraints
- Input CSV format with validation rules and error handling
- Output lineage CSV format compatible with OpenMetadata
- All contracts versioned (v1.0)

**Quickstart**:
- Step-by-step setup instructions for Amazon Linux 2023
- COBOL compiler options (Micro Focus or GnuCOBOL)
- ODBC configuration examples
- Complete demo workflow with expected results
- Troubleshooting section for common issues

### Agent Context Update

GitHub Copilot instructions updated with:
- Language: COBOL-85 or later (Micro Focus Visual COBOL / GnuCOBOL)
- Frameworks: unixODBC, ODBC drivers, Git
- Databases: Amazon RDS SQL Server, Amazon Aurora PostgreSQL, CSV files
- Project type: Single project (batch processing)

---

## Constitution Check Re-evaluation (Post-Design)

*Re-checking constitution compliance after Phase 1 design completion*

### ✅ I. Demo-Ready Code
**Status**: PASS (Confirmed)  
**Evidence**: Quickstart guide provides complete setup and demo workflow. 5-minute demo script included. All programs designed for clear demonstration of COBOL capabilities.

### ✅ II. COBOL Best Practices
**Status**: PASS (Confirmed)  
**Evidence**: Data model enforces proper structure. Copybooks defined for shared logic (LINEAGE-LOGGER, RECORD-DEFS, DB-CONFIG). Static lineage comments demonstrate documentation best practices.

### ✅ III. No Testing Required
**Status**: PASS (Confirmed)  
**Evidence**: Test runners are demo execution scripts, not formal test suites. No TDD requirements. Focus on working demonstration.

### ✅ IV. Banking Domain Focus
**Status**: PASS (Confirmed)  
**Evidence**: All 7 entities are banking domain concepts. Double-entry accounting implemented. Realistic transaction types and workflows.

### ✅ V. Simplicity and Clarity
**Status**: PASS (Confirmed)  
**Evidence**: Simple CSV input/output. Clear sequential execution order (5 test runners). Straightforward database schemas. No complex patterns or abstractions.

### ✅ Technical Standards: COBOL Standards
**Status**: PASS (Confirmed)  
**Evidence**: COBOL-85+ specified. Copybooks for reusable code. Embedded SQL for database access. Static lineage comments for documentation.

### ✅ Technical Standards: Data Management
**Status**: PASS (Confirmed)  
**Evidence**: Simple, well-documented schemas. Sample data provided in quickstart. Self-documenting via lineage and comments.

### ✅ Demo Requirements: Presentation Quality
**Status**: PASS (Confirmed)  
**Evidence**: Quickstart includes 5-minute demo script. All setup and troubleshooting documented. Sample data and expected results provided.

### ✅ Demo Requirements: Feature Scope
**Status**: PASS (Confirmed)  
**Evidence**: 5 complete COBOL programs defined. Each independently demonstrable. No stubs or placeholders in design.

### Post-Design Constitution Summary
**Overall Status**: ✅ **PASS** - All constitutional principles remain satisfied after design phase. No new violations introduced. Design maintains demo-focused, simple, banking-domain approach.

---

## Implementation Readiness

### Artifacts Generated
- ✅ Technical context filled (no NEEDS CLARIFICATION remaining)
- ✅ Constitution check passed (Phase 0 and post-Phase 1)
- ✅ Research completed with 8 key decisions documented
- ✅ Data model defined with 7 entities, validation rules, relationships
- ✅ Database contracts created (SQL Server DDL, PostgreSQL DDL)
- ✅ CSV format contracts created (input and output specifications)
- ✅ Quickstart guide created with setup and demo instructions
- ✅ Agent context updated for Copilot

### Ready for Phase 2
The `/speckit.tasks` command can now be executed to generate actionable, dependency-ordered tasks for implementation. All design artifacts are complete and ready to guide task breakdown.

---

**Plan Status**: ✅ **COMPLETE**  
**Branch**: `001-cobol-data-lineage`  
**Next Step**: Run `/speckit.tasks` to generate implementation tasks

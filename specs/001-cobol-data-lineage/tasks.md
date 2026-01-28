# Tasks: COBOL Data Lineage Banking Component

**Input**: Design documents from `/specs/001-cobol-data-lineage/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/, quickstart.md

**Tests**: Per constitution, formal tests are NOT required. Test runners are demonstration execution scripts.

**Organization**: Tasks are grouped by user story to enable independent implementation and demonstration of each feature.

## Format: `- [ ] [ID] [P?] [Story?] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

Based on plan.md structure:
- COBOL source: `cobol/src/`
- Copybooks: `cobol/copybooks/`
- Configuration: `config/`
- SQL scripts: `sql/sqlserver/`, `sql/postgres/`
- Data files: `data/in/`
- Test runners: `tests/`
- Documentation: `docs/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and directory structure

- [ ] T001 Create project directory structure per plan.md
- [ ] T002 [P] Create .version file with git commit SHA
- [ ] T003 [P] Create .gitkeep files for empty directories (lineage/out/, data/in/)
- [ ] T004 [P] Create sample transactions.csv in data/in/ with 5 transactions

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Database Schema Setup

- [ ] T005 Create SQL Server DDL script in sql/sqlserver/ddl.sql from contracts
- [ ] T006 Create PostgreSQL DDL script in sql/postgres/ddl.sql from contracts
- [ ] T007 Create ODBC configuration template in config/odbc.ini
- [ ] T008 Create ODBC driver configuration in config/odbcinst.ini

### COBOL Copybooks (Shared Infrastructure)

- [ ] T009 [P] Create DB-CONFIG.cpy copybook with DSN constants in cobol/copybooks/
- [ ] T010 [P] Create RECORD-DEFS.cpy copybook with common record layouts in cobol/copybooks/
- [ ] T011 Create LINEAGE-LOGGER.cpy copybook with ADD-LINEAGE-EVENT section in cobol/copybooks/

### Test Runner Framework

- [ ] T012 [P] Create 01_seed_sqlserver.sh test runner in tests/
- [ ] T013 [P] Create 02_post_ledger.sh test runner in tests/
- [ ] T014 [P] Create 03_balance.sh test runner in tests/
- [ ] T015 [P] Create 04_repl.sh test runner in tests/
- [ ] T016 [P] Create 05_export_lineage.sh test runner in tests/

### Documentation

- [ ] T017 Create docs/README.md with lineage strategy, program execution order, and troubleshooting

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Transaction Processing with Lineage Capture (Priority: P1) 🎯 MVP

**Goal**: Demonstrate complete data lineage from CSV ingestion through transaction posting to lineage export

**Independent Test**: Ingest sample CSV with 5 transactions, verify database insertion, post to ledger, and export lineage CSV showing complete flow

### CSV Ingestion (TX_INBOUND)

- [ ] T018 [US1] Create TX_INBOUND.cbl program in cobol/src/
- [ ] T019 [US1] Implement CSV file reading with OPEN INPUT and READ loop in TX_INBOUND.cbl
- [ ] T020 [US1] Implement CSV parsing with UNSTRING for delimited fields in TX_INBOUND.cbl
- [ ] T021 [US1] Implement data validation (ACC_ID, AMOUNT, CURRENCY, TX_TS_UTC, TX_TYPE) in TX_INBOUND.cbl
- [ ] T022 [US1] Implement ODBC connection to SQL Server (SQLSRV_CBLR DSN) in TX_INBOUND.cbl
- [ ] T023 [US1] Implement INSERT into dbo.Transactions with EXEC SQL in TX_INBOUND.cbl
- [ ] T024 [US1] Add static lineage comment block for CSV→Transactions transformation in TX_INBOUND.cbl
- [ ] T025 [US1] Add runtime lineage event logging (transform_kind=ingest) using LINEAGE-LOGGER.cpy in TX_INBOUND.cbl
- [ ] T026 [US1] Implement error handling for invalid rows (skip and log to stderr) in TX_INBOUND.cbl
- [ ] T027 [US1] Add COMMIT/ROLLBACK transaction handling in TX_INBOUND.cbl
- [ ] T028 [US1] Add stdout summary report (valid/invalid row counts) in TX_INBOUND.cbl

### Ledger Posting (POST_LEDGER)

- [ ] T029 [US1] Create POST_LEDGER.cbl program in cobol/src/
- [ ] T030 [US1] Implement ODBC connection to SQL Server in POST_LEDGER.cbl
- [ ] T031 [US1] Implement SELECT cursor over dbo.Transactions in POST_LEDGER.cbl
- [ ] T032 [US1] Implement double-entry logic: IF AMOUNT>=0 THEN CREDIT ELSE DEBIT in POST_LEDGER.cbl
- [ ] T033 [US1] Implement INSERT into dbo.LedgerEntries with conditional DEBIT/CREDIT in POST_LEDGER.cbl
- [ ] T034 [US1] Add static lineage comment block for Transactions→LedgerEntries transformation in POST_LEDGER.cbl
- [ ] T035 [US1] Add runtime lineage event logging (transform_kind=posting) in POST_LEDGER.cbl
- [ ] T036 [US1] Implement INSERT into dbo.PostingAudit with row counts in POST_LEDGER.cbl
- [ ] T037 [US1] Add COMMIT/ROLLBACK transaction handling in POST_LEDGER.cbl
- [ ] T038 [US1] Generate unique run_id (timestamp-based) in POST_LEDGER.cbl

### Lineage Export (LINEAGE_EXPORT)

- [ ] T039 [US1] Create LINEAGE_EXPORT.cbl program in cobol/src/
- [ ] T040 [US1] Implement ODBC connection to SQL Server in LINEAGE_EXPORT.cbl
- [ ] T041 [US1] Implement SELECT cursor over dbo.LineageEvents ORDER BY TS_UTC in LINEAGE_EXPORT.cbl
- [ ] T042 [US1] Implement CSV file output with OPEN OUTPUT to lineage/out/lineage.csv in LINEAGE_EXPORT.cbl
- [ ] T043 [US1] Write CSV header row with all 14 column names in LINEAGE_EXPORT.cbl
- [ ] T044 [US1] Implement CSV row formatting with proper quoting for fields containing commas in LINEAGE_EXPORT.cbl
- [ ] T045 [US1] Implement field quoting and escaping (double quotes) in LINEAGE_EXPORT.cbl
- [ ] T046 [US1] Close cursor and output file in LINEAGE_EXPORT.cbl
- [ ] T047 [US1] Add stdout summary report (lineage event count) in LINEAGE_EXPORT.cbl

**Checkpoint**: User Story 1 complete - Can demonstrate CSV→Transactions→LedgerEntries→Lineage CSV

---

## Phase 4: User Story 4 - Balance Calculation with Audit Trail (Priority: P2)

**Goal**: Calculate and maintain account balances with audit trail for reconciliation

**Independent Test**: Post ledger entries, run balance calculation, verify AccountBalances reflect aggregations and PostingAudit contains metrics

### Balance Aggregation (BALANCE_RECALC)

- [ ] T048 [P] [US4] Create BALANCE_RECALC.cbl program in cobol/src/
- [ ] T049 [US4] Implement ODBC connection to SQL Server in BALANCE_RECALC.cbl
- [ ] T050 [US4] Implement SELECT with GROUP BY ACC_ID, CURRENCY from dbo.LedgerEntries in BALANCE_RECALC.cbl
- [ ] T051 [US4] Implement balance calculation: SUM(CREDIT) - SUM(DEBIT) in BALANCE_RECALC.cbl
- [ ] T052 [US4] Implement MERGE (UPSERT) into dbo.AccountBalances in BALANCE_RECALC.cbl
- [ ] T053 [US4] Add static lineage comment block for LedgerEntries→AccountBalances aggregation in BALANCE_RECALC.cbl
- [ ] T054 [US4] Add runtime lineage event logging (transform_kind=aggregate) in BALANCE_RECALC.cbl
- [ ] T055 [US4] Implement INSERT into dbo.PostingAudit with row counts in BALANCE_RECALC.cbl
- [ ] T056 [US4] Add COMMIT/ROLLBACK transaction handling in BALANCE_RECALC.cbl
- [ ] T057 [US4] Add stdout summary report (balances updated count) in BALANCE_RECALC.cbl

**Checkpoint**: User Story 4 complete - Can demonstrate balance calculation and audit trail independently

---

## Phase 5: User Story 2 - Cross-Database Replication Tracking (Priority: P2)

**Goal**: Replicate data from SQL Server to PostgreSQL with complete lineage traceability

**Independent Test**: Run replication program, verify PostgreSQL daily_snapshots and account_rollups contain data with lineage events showing cross-database movement

### Cross-DB Replication (REPL_REPORTING)

- [ ] T058 [P] [US2] Create REPL_REPORTING.cbl program in cobol/src/
- [ ] T059 [US2] Implement dual ODBC connections (SQL Server SQLSRV_CBLR and PostgreSQL PG_CBLR) in REPL_REPORTING.cbl
- [ ] T060 [US2] Implement SELECT from SQL Server dbo.vw_DailyBalances view in REPL_REPORTING.cbl
- [ ] T061 [US2] Implement PostgreSQL INSERT with ON CONFLICT UPDATE (upsert) for public.daily_snapshots in REPL_REPORTING.cbl
- [ ] T062 [US2] Add static lineage comment block for SQL Server→PostgreSQL replication in REPL_REPORTING.cbl
- [ ] T063 [US2] Add runtime lineage event logging (transform_kind=replication) with engine-qualified identifiers (sqlserver→postgres) in REPL_REPORTING.cbl
- [ ] T064 [US2] Implement SELECT from SQL Server dbo.vw_CurrencyRollups view in REPL_REPORTING.cbl
- [ ] T065 [US2] Implement INSERT into public.account_rollups in REPL_REPORTING.cbl
- [ ] T066 [US2] Add runtime lineage event for currency rollups replication in REPL_REPORTING.cbl
- [ ] T067 [US2] Add COMMIT/ROLLBACK transaction handling for both databases in REPL_REPORTING.cbl
- [ ] T068 [US2] Add stdout summary report (rows replicated to PostgreSQL) in REPL_REPORTING.cbl

**Checkpoint**: User Story 2 complete - Can demonstrate cross-database replication with lineage tracking independently

---

## Phase 6: User Story 3 - Static Lineage Documentation Review (Priority: P3)

**Goal**: Provide machine-parseable lineage annotations in COBOL source code for code review

**Independent Test**: Open any COBOL program source file and verify static lineage comments exist and follow the defined grammar

### Static Lineage Annotations

- [ ] T069 [P] [US3] Review and ensure TX_INBOUND.cbl has complete static lineage comment block (PROGRAM, SRC, TGT, MAP, REF)
- [ ] T070 [P] [US3] Review and ensure POST_LEDGER.cbl has complete static lineage comment block with double-entry logic documented
- [ ] T071 [P] [US3] Review and ensure BALANCE_RECALC.cbl has complete static lineage comment block with aggregation logic documented
- [ ] T072 [P] [US3] Review and ensure REPL_REPORTING.cbl has complete static lineage comment blocks for both replications with engine-qualified identifiers
- [ ] T073 [US3] Add documentation section in docs/README.md explaining static lineage grammar and parsing approach
- [ ] T074 [US3] Create sample static lineage parser script (Python/shell) as proof of concept in docs/examples/

**Checkpoint**: User Story 3 complete - All programs have static lineage annotations matching runtime lineage

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final demo preparation

### Documentation & Demo Preparation

- [ ] T075 [P] Verify docs/README.md covers all aspects: lineage strategy, program order, troubleshooting
- [ ] T076 [P] Update quickstart.md with any implementation-specific details or learnings
- [ ] T077 [P] Create demo script (5-minute walkthrough) in docs/DEMO_SCRIPT.md
- [ ] T078 [P] Verify sample transactions.csv has realistic banking data (5+ rows, multiple currencies)

### Validation & Testing

- [ ] T079 Run full pipeline test: 01_seed → 02_post → 03_balance → 04_repl → 05_export
- [ ] T080 Verify SQL Server tables populated correctly (Transactions, LedgerEntries, AccountBalances, LineageEvents, PostingAudit)
- [ ] T081 Verify PostgreSQL tables populated correctly (daily_snapshots, account_rollups)
- [ ] T082 Verify lineage.csv contains all transform_kind types (ingest, posting, aggregate, replication)
- [ ] T083 Verify lineage.csv is chronologically ordered by ts_utc
- [ ] T084 Test edge case: Empty CSV file (header only)
- [ ] T085 Test edge case: Invalid CSV rows (bad currency, out-of-range amount)
- [ ] T086 Test edge case: Missing .version file (should default to "UNKNOWN")
- [ ] T087 Test idempotency: Re-run programs with same data, verify no errors

### Code Quality & Cleanup

- [ ] T088 [P] Review all COBOL programs for consistent naming conventions
- [ ] T089 [P] Ensure all programs have proper IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions
- [ ] T090 [P] Verify all EXEC SQL blocks have proper error checking (SQLCODE)
- [ ] T091 [P] Ensure all file I/O has proper OPEN/CLOSE and error handling
- [ ] T092 Code review: Verify static lineage comments match runtime lineage events
- [ ] T093 Code review: Verify all transformation logic is clearly commented
- [ ] T094 Remove any debug statements or temporary code

### Final Demo Validation

- [ ] T095 Run quickstart.md validation: Execute all setup steps on clean EC2 instance
- [ ] T096 Verify all test runners execute successfully in sequence
- [ ] T097 Verify demo can be completed in under 10 minutes (setup + execution)
- [ ] T098 Test demo with fresh database (truncate all tables, re-run pipeline)
- [ ] T099 Document any AWS-specific configuration needed (security groups, IAM roles)
- [ ] T100 Create final presentation materials highlighting lineage flow

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 (Phase 3 - P1)**: Depends on Foundational (Phase 2) - No dependencies on other stories
- **User Story 4 (Phase 4 - P2)**: Depends on Foundational (Phase 2) and User Story 1 ledger posting (POST_LEDGER) - Needs LedgerEntries to exist
- **User Story 2 (Phase 5 - P2)**: Depends on Foundational (Phase 2) and User Story 4 balance calculation (BALANCE_RECALC) - Needs AccountBalances to exist
- **User Story 3 (Phase 6 - P3)**: Can start anytime after programs exist - Mostly review and documentation
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

```
Phase 1: Setup → Phase 2: Foundational
                        ↓
                Phase 3: US1 (P1) - Transaction Processing & Lineage
                        ↓
                Phase 4: US4 (P2) - Balance Calculation
                        ↓
                Phase 5: US2 (P2) - Cross-DB Replication
                        ↓
                Phase 6: US3 (P3) - Static Lineage (can happen anytime after programs exist)
                        ↓
                Phase 7: Polish
```

**Critical Path**: Setup → Foundational → US1 → US4 → US2 → Polish

**MVP Scope**: Setup + Foundational + US1 = Working transaction processing with lineage export

### Within Each User Story

**User Story 1 (Transaction Processing)**:
1. TX_INBOUND first (ingestion before posting)
2. POST_LEDGER second (needs Transactions to exist)
3. LINEAGE_EXPORT third (needs LineageEvents to exist)

**User Story 4 (Balance Calculation)**:
1. BALANCE_RECALC (needs LedgerEntries from US1's POST_LEDGER)

**User Story 2 (Cross-DB Replication)**:
1. REPL_REPORTING (needs AccountBalances from US4's BALANCE_RECALC)

**User Story 3 (Static Lineage)**:
1. Review tasks can run in parallel (T069-T072 all marked [P])
2. Documentation can happen anytime

### Parallel Opportunities

**Phase 1 (Setup)**: T002, T003, T004 can run in parallel

**Phase 2 (Foundational)**:
- Database DDLs (T005, T006) can run in parallel
- ODBC configs (T007, T008) can run in parallel
- Copybooks (T009, T010) can run in parallel
- Test runners (T012-T016) can all run in parallel
- Documentation (T017) can run in parallel with other Phase 2 tasks

**Phase 3 (US1)**: 
- Within TX_INBOUND: Most tasks sequential (building on previous)
- Within POST_LEDGER: Most tasks sequential
- Within LINEAGE_EXPORT: Most tasks sequential
- BUT: If multiple developers, one can work on TX_INBOUND while another works on copybooks

**Phase 4 (US4)**: 
- BALANCE_RECALC tasks mostly sequential

**Phase 5 (US2)**: 
- REPL_REPORTING tasks mostly sequential

**Phase 6 (US3)**: 
- T069-T072 can all run in parallel (reviews of different programs)
- T073-T074 can run in parallel

**Phase 7 (Polish)**:
- Documentation tasks (T075-T078) can all run in parallel
- Code quality tasks (T088-T091) can all run in parallel
- Validation tasks (T079-T087) must run sequentially
- Demo tasks (T095-T100) mostly sequential

---

## Parallel Example: Foundational Phase

```bash
# These can all run in parallel by different developers:
Developer 1: T005 (SQL Server DDL)
Developer 2: T006 (PostgreSQL DDL)
Developer 3: T009 (DB-CONFIG.cpy)
Developer 4: T010 (RECORD-DEFS.cpy)
Developer 5: T012-T016 (All test runners)
Developer 6: T017 (README.md)

# T011 (LINEAGE-LOGGER.cpy) should wait for T009, T010 to inform design
```

## Parallel Example: User Story 3 (Static Lineage Reviews)

```bash
# These can all run in parallel:
Reviewer 1: T069 (TX_INBOUND static lineage)
Reviewer 2: T070 (POST_LEDGER static lineage)
Reviewer 3: T071 (BALANCE_RECALC static lineage)
Reviewer 4: T072 (REPL_REPORTING static lineage)
```

---

## Implementation Strategy

### Recommended Approach: MVP First

1. **Week 1**: Complete Phase 1 (Setup) + Phase 2 (Foundational)
   - Establish project structure
   - Create database schemas
   - Build copybook infrastructure
   - Set up test runners

2. **Week 2**: Complete Phase 3 (User Story 1 - P1)
   - Deliver working MVP: CSV → Transactions → LedgerEntries → Lineage CSV
   - Demonstrate core value: end-to-end lineage tracking through COBOL

3. **Week 3**: Complete Phase 4 (User Story 4 - P2)
   - Add balance calculation and audit trail
   - Demonstrate banking domain accuracy

4. **Week 4**: Complete Phase 5 (User Story 2 - P2)
   - Add cross-database replication
   - Demonstrate enterprise-grade lineage (SQL Server → PostgreSQL)

5. **Week 5**: Complete Phase 6 (User Story 3 - P3) + Phase 7 (Polish)
   - Add static lineage documentation
   - Finalize demo and documentation
   - Validate all acceptance criteria

### Alternative: Parallel Development

If 3+ developers available:
- Team A: User Story 1 (P1) - Critical path
- Team B: User Story 4 (P2) - Starts after US1 POST_LEDGER complete
- Team C: Copybooks + Test Runners + Documentation (parallel to everyone)
- Team A (after US1): User Story 2 (P2) - Starts after US4 complete
- Team B (after US4): User Story 3 (P3) - Review and static lineage
- All Teams: Phase 7 (Polish) - Final validation and demo prep

---

## Task Count Summary

- **Setup**: 4 tasks
- **Foundational**: 13 tasks (includes copybooks, test runners, docs)
- **User Story 1 (P1)**: 30 tasks (TX_INBOUND, POST_LEDGER, LINEAGE_EXPORT)
- **User Story 4 (P2)**: 10 tasks (BALANCE_RECALC)
- **User Story 2 (P2)**: 11 tasks (REPL_REPORTING)
- **User Story 3 (P3)**: 6 tasks (Static lineage reviews)
- **Polish**: 26 tasks (docs, validation, demo prep)

**Total**: 100 tasks

**MVP Scope** (Setup + Foundational + US1): 47 tasks

**Parallel Opportunities**: 25+ tasks can run in parallel when staffed appropriately

---

## Independent Test Criteria

### User Story 1 (P1) - Transaction Processing
✅ **Test**: Place transactions.csv in data/in/, run TX_INBOUND → POST_LEDGER → LINEAGE_EXPORT
✅ **Verify**: lineage.csv shows CSV→Transactions→LedgerEntries flow with transform_kind values

### User Story 4 (P2) - Balance Calculation
✅ **Test**: Run BALANCE_RECALC after posting ledger entries
✅ **Verify**: AccountBalances table reflects correct aggregations, PostingAudit has metrics

### User Story 2 (P2) - Cross-DB Replication
✅ **Test**: Run REPL_REPORTING after balance calculation
✅ **Verify**: PostgreSQL daily_snapshots and account_rollups populated, lineage shows sqlserver→postgres

### User Story 3 (P3) - Static Lineage
✅ **Test**: Open any COBOL program source file
✅ **Verify**: Static lineage comment blocks exist, follow grammar, match runtime lineage

---

**Tasks Status**: ✅ **READY FOR IMPLEMENTATION**  
**Branch**: `001-cobol-data-lineage`  
**Generated**: 2026-01-28

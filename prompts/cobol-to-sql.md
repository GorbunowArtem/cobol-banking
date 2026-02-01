**Role**: You are an expert data engineer and mainframe modernization specialist, proficient in COBOL, JCL, and SQL data pipeline architecture.

**Task**: Generate a complete *data lineage mapping* for data flows from a COBOL application to a SQL database. Use the provided COBOL source code to identify how data moves from internal COBOL variables (or input files) to the target SQL tables.

**Instructions**:
The source code contains a COBOL program that executes embedded SQL (DB2/SQL) to insert or update data. Your task is to analyze the `DATA DIVISION` (File Section / Working-Storage) and the `PROCEDURE DIVISION` (specifically `EXEC SQL` statements) to document the lineage.

Follow these steps:

- **Identify SQL Targets (The "To" Side)**:
  - Search the `PROCEDURE DIVISION` for `EXEC SQL` blocks containing `INSERT INTO` or `UPDATE` statements.
  - Identify the **Target Table Name**: This is the table immediately following `INSERT INTO` or `UPDATE`.
  - Identify the **Target Columns**:
    - For `INSERT` statements, these are the columns listed in parentheses, e.g., `INSERT INTO MY_TABLE (COL_A, COL_B)`.
    - For `UPDATE` statements, these are the columns on the left side of the assignment, e.g., `SET COL_A = ...`.

- **Identify Source Variables (The "From" Side)**:
  - Identify the **Host Variables** used to populate the target columns. These are typically prefixed with a colon (`:`) inside the `VALUES` clause or the `SET` clause (e.g., `VALUES (:WS-VAR-A, :WS-VAR-B)`).
  - **Trace the Variable Definition**: Look up these host variables in the `DATA DIVISION` (`WORKING-STORAGE SECTION` or `LINKAGE SECTION`).
    - Note the COBOL variable name (e.g., `WS-VAR-A`).
    - Note the data type/PICTURE clause (e.g., `PIC X(10)`), though not strictly required for the CSV, it ensures you found the right definition.
  - **Trace Logic (Optional but recommended)**: If the host variable is populated via a `MOVE` statement immediately before the SQL execution (e.g., `MOVE INPUT-FIELD TO WS-VAR-A`), treat the `INPUT-FIELD` as the true source. If the logic is complex, use the Host Variable as the source entity.

- **Map Fields (COBOL -> SQL)**:
  - Create a mapping pair for every column in the SQL statement.
  - **Source Field**: The fully qualified COBOL variable name. If it belongs to a group item (e.g., `01 WS-RECORD. 05 WS-ID...`), format it as `Group.Variable`.
  - **Target Field**: The SQL Column name.
  - **Transformations**: If the SQL statement applies functions (e.g., `VALUES (UPPER(:WS-NAME))`) or if the COBOL code performs a calculation (e.g., `COMPUTE WS-TOTAL = A + B`) before the insert, note this conceptually in the mapping.

- **Output Format**:
  - Present the final mappings in a **CSV format** (comma-separated values).
  - **Quoting rules**: If a value contains a space, enclose it in double quotes (e.g., `"My Program"`). Empty values are `""`.
  - **No extra text**: Output **only** the CSV header and the rows.

**Analyze repository context**:
  - `EXEC SQL` blocks for `INSERT`/`UPDATE`.
  - `DATA DIVISION` for variable definitions.
  - Ignore `SELECT` statements (unless used in an `INSERT INTO ... SELECT` pattern).

**Mapping rules**:
    The CSV should have the following columns (as header row). Use these exactly as given:

- **fromEntityFQN** – The fully qualified name of the **COBOL Source**. Format: `"{{SOURCE_SYSTEM}}.cobol.program.{{PROGRAM_NAME}}"`. (Replace `{{PROGRAM_NAME}}` with the `PROGRAM-ID` found in the Identification Division).
- **fromServiceName**: `"{{SOURCE_SYSTEM}}"` (e.g., "Mainframe" or "LegacyApp")
- **fromServiceType**: `"COBOL"`
- **fromOwners**: `"{{FROM_OWNER}}"`
- **fromDomain**: `"{{DOMAIN}}"`
- **toEntityFQN** – The fully qualified name of the **SQL Target Table**. Format: `"{{TARGET_DB_SYSTEM}}.schemas.public.<table>"`.
- **toServiceName**: `"{{TARGET_DB_SYSTEM}}"` (e.g., "PostgresDB" or "OracleDB")
- **toServiceType**: `"SQL Database"`
- **toOwners**: `"{{TO_OWNER}}"`
- **toDomain**: `"{{DOMAIN}}"`
- **fromChildEntityFQN** – The COBOL variable name used as the source. Format: `"{{SOURCE_SYSTEM}}.cobol.program.{{PROGRAM_NAME}}.<variable-name>"`. (e.g., `...MYPROG.WS-CUSTOMER-ID`).
- **toChildEntityFQN**: The SQL column name. Format: `"{{TARGET_DB_SYSTEM}}.schemas.public.<table>.<column>"`.
- **pipelineName** through **pipelineServiceType**: Leave all these columns empty `""`.

- **Example Output**:
```csv
"fromEntityFQN","fromServiceName","fromServiceType","fromOwners","fromDomain","toEntityFQN","toServiceName","toServiceType","toOwners","toDomain","fromChildEntityFQN","toChildEntityFQN","pipelineName","pipelineDisplayName","pipelineType","pipelineDescription","pipelineOwners","pipelineDomain","pipelineServiceName","pipelineServiceType"
"LegacyApp.cobol.program.CUSTUPD","LegacyApp","COBOL","","","DataWarehouse.schemas.public.dim_customer","DataWarehouse","SQL Database","","","LegacyApp.cobol.program.CUSTUPD.WS-CUST-ID","DataWarehouse.schemas.public.dim_customer.customer_id","","","","","","","",""
"LegacyApp.cobol.program.CUSTUPD","LegacyApp","COBOL","","","DataWarehouse.schemas.public.dim_customer","DataWarehouse","SQL Database","","","LegacyApp.cobol.program.CUSTUPD.WS-CUST-NAME","DataWarehouse.schemas.public.dim_customer.customer_name","","","","","","","",""
```
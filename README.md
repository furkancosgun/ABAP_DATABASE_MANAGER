# ABAP DATABASE MANAGER

**A custom SQL query application for SAP ABAP, enabling management of both internal and external databases.**

## Project Overview

`zabap_p_database_manager` is an SQL query application written in SAP ABAP that allows users to connect to and interact with databases within SAP or external databases. This flexibility provides a seamless interface for database management, similar to SQL Server Management Studio, directly from the ABAP environment.

## Purpose

The project facilitates database management by allowing users to:
- Run SQL queries on both the default SAP database and external databases.
- Retrieve, add, update, and delete data across connected databases.

## Functionalities

- **Flexible Database Connection**: Connect to either the SAP system's default database or an external database using the provided connection interface.
- **Executing SQL Queries**: Run custom SQL queries on the connected database.
- **Data Retrieval**: Display query results in an ALV (ABAP List Viewer) format for easy analysis.
- **Data Modification**: Insert, update, or delete data within the connected database.
- **Error Handling**: User-friendly messages for missing queries, connection issues, and query results.

## Usage

1. **Setup**: Transfer the project files into your local ABAP development environment.
2. **Run the Application**: Execute the report `zabap_p_database_manager` and specify either the default SAP database or an external connection name.
3. **Execute SQL Commands**: Use the interface to connect and run SQL queries on the chosen database.

## Requirements

- **SAP ABAP System**: Access to an SAP environment, with permissions for executing SQL queries on both internal and external databases.

## Contributing

We welcome contributions to enhance the project! Feel free to submit a pull request or add issues for suggestions, bug reports, or new features.
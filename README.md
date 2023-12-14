# SQL Interpreter

## Overview

Hello! We created an interpreter for the Structured Query Language (SQL) using functional goodness of the Haskell. Our project achieves a wide coverage of the available SQL commands which encompass the vast majority of common use cases. We hope this project will:

- Expand accessibility to the SQL language to facilitate the query process for all users, especially beginner users: Compared to R (tidyverse) and Python (pandas), SQL has a more intuitive syntax. This project plans to leverage the intuitiveness of SQL syntax in implementing an easy-to-understand language for database queries.
- Provide a query optimization library with property-based testing checks: The project will also provide a simple library that, given the abstract syntax tree of our language, can perform certain query optimization such as filtering and avoiding selecting unnecessary columns / rows to improve simple optimization. The optimization library is expected to be tested using QuickCheck.
- Allow local compilation and execution of queries: the project aims to complete the compilation and execution process locally. That is, instead of using online notebook or uploading scripts to database, this project aims to parse CSV files, parse and compile SQL-like scripts, perform query optimization, and return the result of queries entirely offline. While this may result in the limitation of the size of the table, this compiler will ensure the user's to be free from any database bugs or internet connection problem.

## Module Structure

We divide the project into the following three categories of functionality:

### Parser/Printer

_Objective_: Decompose the tables (.csv files) and queries (.txt files) into internal data structures. There is a significant amount of error handling that ensures compliance with the SQL syntax as defined here:

_Files_:

- Parser.hs
- SQLParser.hs
- TableParser.hs
- SQLPrinter.hs
- TablePrinter.hs

### Optimization

_Objective_: Run multiple optimization passes on the query's Abstract Syntax Tree before table inspection. The first optimization pass checks for redundant Select subqueries nested within the FROM expression. The second optmization pass checks

_Files_:

- Optimization.hs

### Interpretation

_Objective_: Execute commands on the tables within scope. We interpret a wide range of SQL commands which include nested expressions utilizing eleven operations and four data types.

_Files_:

- Interpretation.hs
- SQLSyntax.hs
- TableSyntax.hs

### Testing

_Objective_: Helper functions and data structures used throughout the project for both property-based testing and unit testing. We implemented two versions of a generator for the Select command to guarantee a valid result.

_Files_:

- GenSQL.hs
- GenVSQL.hs
- GenTable.hs
- Utils.hs

## Example Usage

The interpreter contains the following commands

- `:load <filepath>`: Upload a sql file to the interpreter. The filepath
  should be valid and in proper sql syntax, where each query is separated by a
  semicolon
- `:upload <filepath> <tablename>` : Upload a csv file to the table. Both
  the filepath and the table name should be valid. tablename should match the
  table name already exists in the stack
- `:quit`: The interpreter will exit gracefully
- `:queries`: The interpreter will show all queries that are currently in
  the queue
- `:run <number of command to run>`: Run the queries currently in the stack.
  if the number exists the number of queries exists in the stack, the
  interpreter will exhaust the queries and make no further action.
- `:back <number of command to go back>`: Retrieve the interpreter to previous state (the number of queries executed)
- `:cstack`: Clear the stack of the interpreter
- `:stack`: Show the stack of the interpreter (tables and alias)
- `:redirect <filepath>`: This command will redirect the output of the interpreter to a specific file. If the path to the file is not valid, the interpreter will fail to redirect. The default output location is main
- `:output <tablename>`: This command will output the table name to the output location, specified in earlier command

### Upload a Table

```bash
ghci> commandLine
SQL-Interp>:load test/test-suite/setup/setup.sql
Current Queries: CREATE TABLE Customers (customer_id INTEGER PRIMARY KEY, first_name VARCHAR(255), last_name VARCHAR(255), age INTEGER, country VARCHAR(255));
SQL-Interp>:run 3
customer_id,first_name,last_name,age,country

order_id,item,amount,customer_id

shipping_id,status,customer

SQL-Interp>:upload test/test-suite/setup/Orders.csv Orders
SQL-Interp>:stack
... Ignore output
SQL-Interp>:upload test/test-suite/setup/Customers.csv Customers
SQL-Interp>gi^?^?
SQL-Interp>:stack
--------Customers--------
customer_id,first_name,last_name,age,country
1,John,Doe,31,USA
2,Robert,Luna,22,USA
3,David,Robinson,22,UK
4,John,Reinhardt,25,UK
5,Betty,Doe,28,UAE
--------Customers--------
--------Orders--------
order_id,item,amount,customer_id
1,Keyboard,400,4
2,Mouse,300,4
3,Monitor,12000,3
4,Keyboard,400,1
5,Mousepad,250,2
--------Orders--------
--------Shippings--------
shipping_id,status,customer

--------Shippings--------
++++++++ALIAS++++++++
```

### Execute a Query

```bash
SQL-Interp>:load test/test-suite/test1/test1.sql
Current Queries: SELECT order_id, item, amount, first_name, last_name, age, country
 FROM (Orders AS o) JOIN (Customers AS c) ON o.customer_id=c.customer_id;
SQL-Interp>:run 1
age,amount,country,first_name,item,last_name,order_id
25,400,UK,John,Keyboard,Reinhardt,1
25,300,UK,John,Mouse,Reinhardt,2
22,12000,UK,David,Monitor,Robinson,3
31,400,USA,John,Keyboard,Doe,4
22,250,USA,Robert,Mousepad,Luna,5
SQL-Interp>:stack
--------Customers--------
customer_id,first_name,last_name,age,country
1,John,Doe,31,USA
2,Robert,Luna,22,USA
3,David,Robinson,22,UK
4,John,Reinhardt,25,UK
5,Betty,Doe,28,UAE
--------Customers--------
--------Orders--------
order_id,item,amount,customer_id
1,Keyboard,400,4
2,Mouse,300,4
3,Monitor,12000,3
4,Keyboard,400,1
5,Mousepad,250,2
--------Orders--------
--------Shippings--------
shipping_id,status,customer

--------Shippings--------
--------_Table3--------
age,amount,country,first_name,item,last_name,order_id
25,400,UK,John,Keyboard,Reinhardt,1
25,300,UK,John,Mouse,Reinhardt,2
22,12000,UK,David,Monitor,Robinson,3
31,400,USA,John,Keyboard,Doe,4
22,250,USA,Robert,Mousepad,Luna,5
--------_Table3--------
++++++++ALIAS++++++++
c -> Customers
o -> Orders
SQL-Interp>:redirect test/test-suite/output/csv1.csv
Redirect successful to test/test-suite/output/csv1.csv
SQL-Interp>:output _Table3
```

## Next Steps

We thoroughly enjoyed working on this project and are proud of the progress we made in implementing a SQL interpreter this semester. If we had more time, we would spend more time on the following:

- Executing more sophisticated optimization passes on the Abstract Syntax Tree. Currently, the optimization passes are completed before table inspection meaning that there is no notion of improving command sequencing based on the length or fields of the tables in scope.
- Implementing more sophisticated join methods. We use a naive approach to joining two tables that is approximately $O(n * m)$ where $n, m$ are the number of rows in each table. There exist other join algorithm alternatives that could provide a signficiant performance speed boost.
- Allowing for parallel and/or non-local tables or query submissions. Our project assumes the existence of .csv files present on the local machine and accepts queries directly. However, much of the usage of SQL occurs in parallel for large datasets hosted on a cloud service (ex. Spark).

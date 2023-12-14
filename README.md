# SQL Interpreter

## Overview
Hello! We created an interpreter for the Structured Query Language (SQL) using functional goodness of the Haskell. Our project achieves a wide coverage of the available SQL commands which encompass the vast majority of common use cases. We hope this project will:

* Expand accessibility to the SQL language to facilitate the query process for all users, especially beginner users: Compared to R (tidyverse) and Python (pandas), SQL has a more intuitive syntax. This project plans to leverage the intuitiveness of SQL syntax in implementing an easy-to-understand language for database queries.
* Provide a query optimization library with property-based testing checks: The project will also provide a simple library that, given the abstract syntax tree of our language, can perform certain query optimization such as filtering and avoiding selecting unnecessary columns / rows to improve simple optimization. The optimization library is expected to be tested using QuickCheck.
* Allow local compilation and execution of queries: the project aims to complete the compilation and execution process locally. That is, instead of using online notebook or uploading scripts to database, this project aims to parse CSV files, parse and compile SQL-like scripts, perform query optimization, and return the result of queries entirely offline. While this may result in the limitation of the size of the table, this compiler will ensure the user's to be free from any database bugs or internet connection problem.

## Module Structure
We divide the project into the following three categories of functionality:

### Parser/Printer
*Objective*: Decompose the tables (.csv files) and queries (.txt files) into internal data structures. There is a significant amount of error handling that ensures compliance with the SQL syntax as defined here: 

*Files*:
* Parser.hs
* SQLParser.hs
* TableParser.hs
* SQLPrinter.hs
* TablePrinter.hs

### Optimization
*Objective*: Run multiple optimization passes on the query's Abstract Syntax Tree before table inspection. The first optimization pass checks for redundant Select subqueries nested within the FROM expression. The second optmization pass checks 

*Files*:
* Optimization.hs

### Interpretation
*Objective*: Execute commands on the tables within scope. We interpret a wide range of SQL commands which include nested expressions utilizing eleven operations and four data types.

*Files*:
* Interpretation.hs
* SQLSyntax.hs
* TableSyntax.hs

### Testing
*Objective*: Helper functions and data structures used throughout the project for both property-based testing and unit testing. We implemented two versions of a generator for the Select command to guarantee a valid result.

*Files*:
* GenSQL.hs
* GenVSQL.hs
* GenTable.hs
* Utils.hs

## Example Usage

### Upload a Table

```bash
ghci> commandLine
SQL-Interp> 
ghci> 
ghci> 
ghci> 
ghci> 
```

### Execute a Query

```bash
ghci> commandLine
SQL-Interp> 
ghci> 
ghci> 
ghci> 
ghci> 
```

## Presentation
[Link](https://penno365-my.sharepoint.com/:p:/g/personal/wcurrie_upenn_edu/Eb6u8Ahuen1DvHP0hT6zYEIBPaddYJKWNf2TWgthMEvPnQ?e=m9YYIW) to our presentation on the project.

## Next Steps

We thoroughly enjoyed working on this project and are proud of the progress we made in implementing a SQL interpreter this semester. If we had more time, we would spend more time on the following:

 * Executing more sophisticated optimization passes on the Abstract Syntax Tree. Currently, the optimization passes are completed before table inspection meaning that there is no notion of improving command sequencing based on the length or fields of the tables in scope.
 * Implementing more sophisticated join methods. We use a naive approach to joining two tables that is approximately $O(n * m)$ where $n, m$ are the number of rows in each table. There exist other join algorithm alternatives that could provide a signficiant performance speed boost.
 * Allowing for parallel and/or non-local tables or query submissions. Our project assumes the existence of .csv files present on the local machine and accepts queries directly. However, much of the usage of SQL occurs in parallel for large datasets hosted on a cloud service (ex. Spark).
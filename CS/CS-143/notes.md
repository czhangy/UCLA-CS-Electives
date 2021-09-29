# COM SCI 143 - Fall '21 - Cho

[TOC]

## Lecture 1: What is a Database Management System?

- Objectives:
  - Learn how to use database systems
    - Learn data model
    - Learn SQL (Structured Query Language)
    - Learn Relational Database Management Systems (RDBMS)
    - Learn basics of other non-relational DBMS (NoSQL)
  - By the end of the quarter you will know
    - How to store data in DBMS
    - How to retrieve and interact with data in RDBMS using SQL
      - In particular, you learn how to use MySQL
    - How DBMS works
- Tentative Course Topics
  - Relational model
  - Relational algebra
  - SQL
  - Entity-relationship model
  - Relational design theory
  - Key data structures and algorithms for DBMS
  - Query execution
  - Transactions
  - NoSQL database
  - Non-relational data processing
- MySQL, Docker, Unix
  - In our class, students are required to learn and use
    - Docker
      - x86 Windows or Max
    - Unix command line
    - MariaDB
- Database Management System (DBMS)
  - What is a DBMS?
    - A system that manages massive amounts of data and provides convenient, efficient, persistent, and safe access to such data to many users simultaneously
      - Massive, convenient, efficient, persistent, safe, many users
- Database Architecture
  - Disk - "main memory"
    - OS - doesn't know how to efficiently cache data
      - DBMS engine - knows how to store data on disk to gain excellent performance, can use the underlying hardware directly, without going through the OS (raw I/O)
        - API - (JDBC/ODBC/PHP-MySQL)
          - Application
          - Command line interface
- Popular DBMS Software
  - Relational
    - Open source: MySQL, PostgreSQL, etc.
    - Closed source: Oracle, Microsoft SQL server, IBM DB2, etc.
  - Non-relational
    - MongoDB, Spark, etc.
  - Cloud
    - Amazon RDS, Azure SQL Database, Google Cloud Spanner, Oracle Database Cloud, etc.
- Database is Everywhere
  - All businesses manage various types of data
    - Online banking, Amazon, Walmart, phone backend systems, etc.
  - SQL is *the* language to interact with business-related data
    - Primary focus of the class



## Lecture 2: Relational Model

- Relational Model

  - What is a data model? Why do we need it?

    - A data model is a way we model/conceptualize/represent data
    - We need a concrete representation to manage data in a computer
    - Many different ways to model data
      - Graph model, tree model, object model, etc.

  - Other types of models

    - Graph models - network model
      - Constructed of nodes, edges, and labels
      - Ex: airline flights
    - Tree model - hierarchal model
      - Constructed of nodes, edges, and labels arranged in a tree
      - Ex: company hierarchy

  - Relational model

    - The most popular model used today for database systems

    - All data is represented as a set of "tables"

      - Also called a set of "relations"
      - Based on relation theory in mathematics

    - Ex: school information

      - `Student(sid, name, age, GPA, address, ...)`

        - | `sid` | `name` | `addr`       | `age` | `GPA` |
          | ----- | ------ | ------------ | ----- | ----- |
          | 301   | John   | 183 Westwood | 19    | 2.1   |
          | 303   | Elaine | 301 Wilshire | 17    | 3.9   |
          | 401   | James  | 183 Westwood | 17    | 3.5   |

      - `Class(dept, cnum, sec, title, instructor, ...)`

      - `Enroll(sid, dept, cnum, sec)`

  - Example: JSON (JavaScript Object Notation)

    - Syntax to represent objects in JavaScript

      - `[{ "x": 3, "y": "Good" }, { "x": "4, "y": "Bad }]`

    - One of the most popular data-exchange formats over Internet

      - As JavaScript gained popularity, JSON's popularity grew
        - Natural to use with JS, and web applications commonly use it
      - Simple and easy to learn
      - Other popular formats include XML, CSV, etc.

    - Basic JSON Syntax

      - Supports basic data types like numbers and strings, as well as arrays and "objects"

      - Double quotes for string: `"Best"`, `"UCLA"`, `"Worst"`, `"USC"`

      - Square brackets for array: `[1, 2, 3, "four", 5]`

      - Objects: `(attribute, name)` pairs

        - Use curly braces: `{ "sid": 301, "name":  "James Dean" }`

      - Things can be nested

        - ```json
          { 
             	"sid": 301,
             	"name": { "first": "James", "last":  "Dean" },
             	"classes": [ "CS143", "CS144" ]
          }
          ```

          

    - Data Model of JSON?

      - What is JSON's data model?
        - Tree? Graph? Relational?
        - It's the tree model => each object has a node with various values branching off of it

  - History of Relational Model

    - By far, the most significant invention in the history of DBMS
      - E.F. Codd, 1970
      - Completely revolutionized the field
        - Prior to his work, DBMS had no mathematical foundations
        - His work allowed for the development of algorithms and theory
        - Both theoretically relevant and practically efficient
      - Turing Award, 1981
    - Extremely simple
      - Another piece of evidence that the power of a *simple* yet *widely-applicable* idea in CS
    - Initial research prototypes
      - IBM System R => IBM DB2
      - Berkeley Postgres => PostgreSQL
    - IBM and Oracle: first commercial RDBMS vendor
      - Still dominates the market together with Microsoft

  - Relational Model Terminology

    - Each relation has a set of attributes (columns)
    - Each relation contains a set of tuples (rows)
      - Each tuple represents one entity that you want to capture
    - Every attribute has a domain (type)
      - Only atomic types
    - Data representation is very similar to an Excel spreadsheet
      - Excel not built for large datasets and lacks language with which data can be processed quickly
    - Schema: the structure of relations in a database
      - Relation name, attribute name, domain (optional)
      - Ex: `Student(sid, name, addr, age, GPA)`
    - Instance (data)
      - Actual content of relation
        - Schema is to a variable type as instance is to a value
    - Keys: a set of attributes that uniquely identifies a tuple in a relation
      - `Student(sid, name, address, GPA, age)`
        - `sid` could be a key
      - `Course(dept, cnum, sec, unit, instructor, title)`
        - `dept`, `cnum`, and `sec` together may be a key
        - `dept`, `sec`, and `title` together may be a key
      - Underline key attributes in schema
      - One schema may have multiple keys
        - One key is generally used as a primary key to identify the tuple
      - When do we need keys? How can they be used?
        - When we want to access a particular tuple, keys can be used to gain access to that tuple
        - When we want to join tables together to form associations

  - Name Scope

    - Name of a relation is unique across relations
    - Name of an attribute is unique in a table
      - Same attribute name in different tables is ok

  - Set Semantics

    - No duplicate tuples are allowed in relational model
      - Duplicate tuples are allowed in SQL for practical reasons (more on this later)
      - Can a relation with no duplicates have no keys?
        - No, every table must have a key, since you can identify the entire relational model as having no key
    - Tuple order doesn't matter
    - Attribute order doesn't matter
      - In SQL, attribute order does matter, but not in a pure relational model

  - `Null` Value

    - Common interpretation

      - Do not know/do not want to say/not applicable

    - Ex: `Student(id, major, name, GPA)`

      - What would a `Student`'s GPA value be before the first quarter?

    - Complications from `Null`

      - | `sid` | `name` | `addr`       | `age`  | `GPA` |
        | ----- | ------ | ------------ | ------ | ----- |
        | 301   | John   | 183 Westwood | 21     | 2.1   |
        | 303   | James  | 301 Wilshire | `NULL` | 3.9   |
        | 401   | Susan  | 183 Westwood | 17     | 3.5   |

        - Q1: students with `age >= 20`
          - `{ John }`
        - Q2: students with `age < 20`
          - `{ Susan }`
        - Q3: students with `age >= 20` or `age < 20`
          - This should be everyone
          - Strictly boolean: `{ John, Susan }`

      - When should we return tuples with `Null` values?

        - Brings up strange behavior

    - `Null` and SQL 3-Valued Logic

      - Every condition is evaluated as `True`, `False`, or `Unknown`
      - Concrete rules to deal with `Null` and `Unknown` values
      - `Null`s and SQL 3-valued logic adds significant complexity to DBMS implementation and execution

- Five Steps in Database Construction

  - Domain Analysis - talk to the experts and figure out what to store/what data we need to represent
    - Use of ER (Entity-Relationship) model to document it
  - Database Design - convert ER model to tables
    - Relational design/normalization theory tells us how to do this
  - Table Creation - create the tables in the DBMS
    - DDL (Data Definition Language)
  - Load - load the real data into the DBMS
    - A lot of data initially => use of a bulk-load command
  - Query and Update - manage database accesses
    - DML (Data Manipulation Language)

- Structured Query Language (SQL)

  - The standard language for interacting with RDBMS
  - Many versions of the SQL standard exists
    - As things go on, the standard gets more and more complicated
  - In our lectures, we mostly use SQL92 standard
    - Individual DBMS products may use a slightly different syntax, but it will be the same for the most part
  - SQL has many components
    - Data Definition Language (DDL): schema definition, constraints, etc.
      - How to define the schema/data tables, etc.
    - Data Manipulation Language (DML): query, modification, etc.
    - Transaction, Authorization, etc.
  - We'll cover the schema definition part in the rest of today's lecture
  - Basic SQL Data Types (commonly used subset)
    - String
      - `Char(n)` - padded fixed length
        - Padding character is system dependent (`Space` for Oracle, auto-removed for MySQL)
      - `Varchar(n)` - variable length
        - `n` is the maximum length
    - Number
      - `Integer` - 32-bit
      - `Decimal(5,2)` - `999.99`
        - Result of SQL's original business-orientation
        - Allows specification of number of significant digits and the number of digits behind the decimal
      - `Real`, `Double` - 32-bit, 64-bit
        - Floating-point number representations
    - Datetime
      - `Date` - `'2010-01-15'`
      - `Time` - `'13:50:00'`
      - `Timestamp` - `'2010-01-15 13:15:00'`
        - On MySQL, `Datetime` is referred to as `Timestamp`
  - SQL Table Creation
    - `Course(dept, cnum, sec, unit, instructor, title)`
    - Table creation command: `CREATE TABLE <schema_definition>;`
      - `CREATE TABLE Course(dept char(2), cnum int, sec int, unit int, instructor varchar(50), title varchar(100));`
      - Set primary key: `CREATE TABLE Course(dept char(2), cnum int, sec int, unit int, instructor varchar(50), title varchar(100), primary key(dept, cnum, sec));`
        - Any attributes part of the primary key cannot be `Null` in MySQL
      - Set unique keys that aren't primary: `CREATE TABLE Course(dept char(2), cnum int, sec int, unit int, instructor varchar(50), title varchar(100), primary key(dept, cnum, sec), unique(dept, title, sec), unique(dept, instructor, sec));`
      - Set default value: `unit int default 4`
    - Table deletion command: `DROP TABLE <schema_name>;`
      - `DROP TABLE Course;`
    - Table modification command: `ALTER TABLE`
    - Reserved keywords (`CREATE`, `DROP`, etc.) are case-insensitive
    - Defined keywords may or may not be case-sensitive
      - MySQL's defined keywords are case-sensitive
  - Bulk Loading Data into Table
    - No SQL standard for bulk data loading
    - MySQL (and Oracle)
      - `LOAD DATA LOCAL INFILE <data_file> INTO TABLE <table_name>`

- What We Learned

  - Data model
  - Schema and data instance
  - Relational model
    - Relation, attribute, tuple, domain
    - Key
    - Null value
    - Set semantics
  - 5 steps for database construction
    - Domain analysis, database design, table creation, load, query and manipulation
  - SQL table creation and bulk data loading



## Lecture 3: Relational Algebra

- Database Query Language
  - What is a "query"?
    - OED: a question, especially one addressed to an official or organization
    - Database jargon for a question
    - Question to get an answer from a database
  - Some queries are easy to pose, and some are not
    - Some might be efficient, some might not be
    - Find a query that's easiest for the user and easiest for the DQL
  - Relational Query Languages
    - Formal: relational algebra, relational calculus, datalog
    - Practical: SQL, Quel, QBE
    - Both input to and output from a query are relations: makes "piping" possible
      - Relation => Query => Relation
    - Set semantics: no duplicate tuples, duplicates are automatically eliminated
      - Multiset semantics for SQL for performance reasons (more on this later)


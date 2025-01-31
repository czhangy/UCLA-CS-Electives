# **COM** SCI 143 - Fall '21 - Cho

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
    - We don't have to work on the actual performance of our implementations
      - Done automatically by the language most of the time
    - Relational algebra operators automatically remove duplicates from their outputs
    - When a query is hard to write, think of its complement
    
  - Select Operator `σ_C (R)`

    - Filters out rows in a relation
    - `C`: filtering condition as a boolean expression
    - `R` can be either a relation or a result from another operator

  - Project Operator `π_A (R)`

    - Filters out columns in a relation
    - `A`: the set of attributes to keep

  - Cross Product (Cartesian Product) Operator `R x S`

    - Concatenate every tuple from `R` with every tuple from `S`
      - Concatenated horizontally
    - Create one output per every pair of input tuples
    - If column names conflict, prefix with the table name
    - Definition: `R x S = { t | t = (r, s) for r ∈ R and s ∈ S }`

  - Natural Join Operator `⋈`

    - Join two tables "naturally"
    - Notation: `R ⋈ S`
    - Concatenates tuples horizontally
    - Enforce equality condition on all common attributes
    - Only one copy of the common attributes are kept in the result

  - Rename Operator `ρ_S (R)`

    - `ρ_S (R)`: Rename `R` to `S`
    - `ρ_{S(A1, A2)} (R)`: Rename `R` to `S(A1, A2)` including attribute names

  - Union Operator `∪`

    - `R ∪ S`: Union of tuples from `R` and `S`
    - The schemas of `R` and `S` should be the same
    - No duplicate tuples in the result

  - Set Difference Operator `−`

    - `R − S`: Tuples in `R` that don't exist in `S`
    - The schemas of `R` and `S` should be the same

  - Intersect Operator `∩`

    - `R ∩ S`: Tuples that exist in both `R` and `S`
    - The schemas of `R` and `S` should be the same
    - `R ∩ S = R − (R − S)`

  - Example queries:

    - `Student(sid, name, addr, age, GPA)`

      `Class(dept, cnum, sec, unit, title, instructor)`

      `Enroll(sid, dept, cnum, sec)`

    - Retrieve all entries in the `Student` table

      - `Student`

    - Retrieve all students that are over 18 years old

      - $$
        \sigma_{age\ <\ 18}(Student)
        $$

    - Retrieve all students that are over 18 years old and have a GPA greater than 3.7

      - $$
        \sigma_{gpa\ >\ 3.7} (\sigma_{age\ <\ 18}(Student))
        $$

      - $$
        \sigma_{gpa\ >\ 3.7\ \and\ age\ <\ 18}(Student)
        $$

    - ID and GPA of all students

      - $$
        \Pi_{sid,\ gpa}(Student)
        $$

    - All departments offering a class

      - $$
        \Pi_{dept} (Class)
        $$

    - ID and GPA of students that are less than 18 years old

      - $$
        \Pi_{sid,\ gpa} (\sigma_{age\ <\ 18} (Student))
        $$

        

    - Names of students who take CS classes

      - 
        $$
        \Pi_{name} (\sigma_{Student.sid\ =\ Enroll.sid} (Student \times \Pi_{sid} (\sigma_{dept\ =\ 'CS'} (Enroll))))
        $$
        
      - $$
        \Pi_{name} (\sigma_{Student.sid\ =\ Enroll.sid} (Student \times \sigma_{dept\ =\ 'CS'} (Enroll)))
        $$
      
        
      
      - $$
        \Pi_{name} (\sigma_{Student.sid\ =\ Enroll.sid\ \and\ dept\ =\ 'CS'} (Student \times Enroll))
        $$
      
        
      
      - $$
        \Pi_{name} (Student \bowtie \sigma_{dept\ =\ 'CS'} (Enroll))
        $$
      
        
      
    - Names of students who take classes offered by "Dick Muntz"
    
      - $$
        \Pi_{name} (Student \bowtie Enroll \bowtie σ_{instructor\ =\ 'Dick\ Muntz'} (Class))
        $$
    
        
    
    - Names of students pairs who live at the same address
    
      - $$
        \Pi_{S1.name,\ S2.name} (\sigma_{S1.addr\ =\ S2.addr\ \and\ S1.name\ >\ S2.name} (ρ_{S1} (Student) \times ρ_{S2} (Student)))
        $$
    
        
    
    - All students and instructors' names
    
      - $$
        \rho_{Person(name)} (π_{name} (Student)) \cup \rho_{Person(name)} (π_{instructor} (Class))
        $$
    
        
    
    - Courses (`dept`, `cnum`, `sec`) that no one takes
    
      - $$
        \Pi_{dept,\ cnum,\ sec} (Class) - \Pi_{dept,\ cnum,\ sec} (Enroll)
        $$
    
        
    
    - Titles of courses that no one takes
    
      - $$
        \Pi_{title} ((\Pi_{dept,\ cnum,\ sec} (Class) − \Pi_{dept,\ cnum,\ sec} (Enroll)) \bowtie Class)
        $$
    
        
    
    - Instructor names who teach both CS and EE courses
    
      - $$
        \Pi_{instructor} (\sigma_{dept\ =\ 'CS'} (Class)) \cap \Pi_{instructor} (\sigma_{dept\ =\ 'EE'} (Class))
        $$
    
        
    
      - $$
        \Pi_{instructor} (\sigma_{dept\ =\ 'CS'} (Class)) \bowtie \Pi_{instructor} (\sigma_{dept\ =\ 'EE'} (Class))
        $$
    
    - IDs of students who did not take any CS class
    
      - $$
        \Pi_{sid}(Student)-\Pi_{sid}(\sigma_{dept\ =\ 'CS'}(Enroll))
        $$
    
  - Questions:
  
    - Is it ever useful to compose two projection operators next to each other?
      - No, the first projection operator would retrieve only the columns it was told to project, therefore the next projection operator's column(s) would already have been filtered out
    - Is it ever useful to compose two selection operators next to each other?
      - Yes, see the third example above
    - Concatenating two unrelated tables (cross product) looks odd; why use it?
      - See seventh example above
    - If `|R| = r` and `|S| = s`, what is `|R x S|`?
      - `r x s`
  
  - Core Relational Operators
  
    - `σ`, `π`, `x`, `⋈`, `ρ`, `∪`, `−`, `∩`
    - Which ones are "core" and which ones can be expressed with others?
      - Core: `σ`, `π`, `x`, `ρ`, `∪`, `−`
      - Not core:
        - `⋈` - can be expressed with `x`
        - `∩` - can be expressed with `−`
    - Prove that `−` is core:
      - First, we classify operators as either monotonic or not monotonic
        - Monotonic operators produce either the same amount or more output when more input is added
      - Everything is monotonic, except for `−`
      - It's impossible to create a non-monotonic operator from a set of monotonic operators, therefore `−` must be core
    - Prove that `∪` is core:
      - `x` and `∪` are the only operators that take in 2 inputs
      - `∪` is the only one that stacks schema on top of each other vertically, `x` does so horizontally
  
- Summary

  - Relational algebra: formal query language for relational model
    - Theoretical foundation behind SQL
  - Both inputs and outputs are relations: enables "piping"
    - Relation => Query => Relation
  - Set semantics: duplicates are automatically eliminated
  - Operators learned: `σ`, `π`, `x`, `⋈`, `ρ`, `∪`, `−`, `∩`
  - Suggestion: if a query is difficult to write, think of its complement!



## Lecture 4: Basic SQL

- SQL (Structured Query Language)

  - *The* query language for RDBMS
  - SQL has many aspects
    - DDL, DML, transactions, etc.
  - In this lecture, we're learning the DML part of SQL
    - How to query and modify the existing database
  - SQL and DBMS
    - SQL is a high-level description of what a user wants
    - Given a SQL query, DBMS figures out how best to execute it automatically
      - Beauty and success of DBMS
      - Users don't need to worry about efficiency usually
  - We say two queries are logically equivalent if they return the same exact result for any set of tables

- Basic SQL `SELECT` statement

  - ```sql
    SELECT A_1, ... , A_n
    FROM R_1, ... , R_m
    WHERE C
    ```

  - Approximately:

    - $$
      \Pi_{A_1,...,A_n}(\sigma_C(R_1\times\ ...\ \times R_m))
      $$

  - `SELECT *`: all attributes

  - Note:

    - `SELECT` is "projection", not "selection"
    - SQL does not remove duplicates: main difference between SQL and relational algebra
      - Multiset/bag semantics for SQL, set semantics for relational algebra
      - Use `SELECT DISTINCT` to remove duplicates

- Set Operators

  - SQL Set operators: `UNION`, `INTERSECT`, `EXCEPT`
    - Can be applied to relations or to the result of `SELECT` statements
  - Schemas of input relations should be the same
    - In practice, just having the compatible types is find
  - Set operators follow set semantics and remove duplicates
    - Most people don't know multiset semantics of set operators
    - No efficiency penalty for duplicate elimination for set operation
    - To keep duplicates, use `UNION ALL`, `INTERSECT ALL`, `EXCEPT ALL`
  - MySQL supports only `UNION`, but not `INTERSECT` or `EXCEPT`
    - A major pain since `EXCEPT` is a core operator
    - People often use a subquery to simulate `EXCEPT`
      - Use `NOT IN` operator in MySQL, which we will learn soon
  - MariaDB supports `INTERSECT` and `EXCEPT` (starting from v10.3)
    - Our container uses MariaDB

- Subqueries

  - `SELECT` statement may appear inside another `SELECT` statement
    - Nested `SELECT` statements
    
  - Interpretation of subquery
    - The result from the inner `SELECT` statement is treated like a regular relation
    - Scalar-valued subquery: if the result is a one-attribute, one-tuple relation, the result can be used like a constant
    
  - Un-nesting subquery
    - Does the addition of subqueries to MySQL make it more expressive than relational algebra?
    - A large body of theory and algorithms exist on how to "un-nest" a subquery to non-subquery SQL
      - We can rewrite subqueries to non-subqueries as long as there is no negation (`NOT`)
      - With negation, we need `EXCEPT`
    - Another demonstration of the success of the relational model
      - Simple theoretical model makes it possible to create important theorems and algorithms
    
  - Set Membership Operator
    - `IN`, `NOT IN`
      - `(a IN R)` is TRUE if `a` appears in `R`
    
  - Set Comparison Operator
    - `(a > ALL R)`, `(a <= SOME R)`, etc.: compare `a` against tuples in `R`
      - `a > ALL R` is TRUE if `a` is larger than all tuples in `R`
    - Q: Is `= SOME` equivalent to `IN`? Yes
    
  - Correlated Subquery
    - When the subquery references a table defined in the outer clause, it's a correlated subquery
    - `EXISTS()` is true when the contents contain at least one tuple
    
  - Subquery in `FROM`

    - ```sql
      SELECT name
      FROM (SELECT name, age FROM Student) S
      WHERE age > 17;
      ```

    - A new name must be given to the subquery in this case

- Common Table Expression (SQL99)

  - ```sql
    WITH (alias) AS (subquery)
    SELECT ... FROM (alias) ...
    ```

    - Very convenient for using the same subquery multiple times

  - Example:

    - ```sql
      WITH S AS (SELECT name, age FROM Student)
      SELECT name FROM S WHERE age > 17;
      ```

- Example Database: School Information

  - `Student(sid, name, addr, age, GPA)`

    `Class(dept, cnum, sec, unit, title, instructor)`

    `Enroll(sid, dept, cnum, sec)`

  - Q1: Titles and instructors of all CS classes

    - ```sql
      SELECT title, instructor
      FROM Class
      WHERE dept = 'CS';
      ```

  - Q2: Names and GPAs of all students who take CS classes

    - ```sql
      SELECT DISTINCT name, GPA
      FROM Enroll AS E, Student AS S
      WHERE dept = 'CS' AND E.sid = S.sid;
      ```

      - `S` and `E` are called tuple variables => they bind to every pair from `Student` and `Enroll`
      - Attributes can also be renamed

  - Q3: All student names and GPAs who live on Wilshire

    - ```sql
      SELECT name, GPA
      FROM Student
      WHERE addr LIKE '%Wilshire%';
      ```

      - `%` - matches 0 or more characters
      - `_` - matches one character
      - `%Wilshire%` - any string containing `Wilshire`
      - Q: What does `'___%'` mean?
        - Any string of length 3+
      - Common string functions exist: `UPPER()`, `LOWER()`, `CONCAT()`, etc.

  - Q4: Students' and instructors' names

    - ```sql
      (SELECT name
      FROM Student)
      UNION
      (SELECT Instructor name
      FROM Class)
      ```

  - Q5: IDs of students who do not take any CS classes

    - ```sql
      (SELECT sid
      FROM Student)
      EXCEPT
      (SELECT sid
      FROM Enroll
      WHERE dept = 'CS');
      ```

  - Q6: IDs of students who live with student 301

    - ```sql
      SELECT sid
      FROM Student
      WHERE addr = (SELECT addr
      			  FROM Student
      			  WHERE sid = 301);
      ```

      - This is not a feature of relational algebra

      - ```sql
        SELECT S2.sid
        FROM Student AS s1, Student AS S2
        WHERE S1.sid = 301 AND S1.addr = S2.addr;
        ```

  - Q7: Student names who take CS classes

    - ```sql
      SELECT name
      FROM Student
      WHERE sid IN (SELECT sid
      			  FROM Enroll
      		      WHERE dept = 'CS');
      ```

    - ```sql
      SELECT DISTINCT name
      FROM Student AS S, Enroll AS E
      WHERE S.sid = E.sid AND E.dept = 'CS';
      ```

  - Q8: Student names who take no CS classes

    - ```sql
      SELECT name
      FROM Student
      WHERE sid NOT IN (SELECT sid
      			  FROM Enroll
      		      WHERE dept = 'CS');
      ```

  - Q9: Student IDs who have higher GPAs than all students of age 18 or less

    - ```sql
      SELECT sid
      FROM Student
      WHERE GPA > ALL (SELECT GPA
      				 FROM Student
      				 WHERE age <= 18);
      ```

  - Q10: Student IDs who have higher GPA than at least one student of age 18 or less

    - ```sql
      SELECT sid
      FROM Student
      WHERE GPA > SOME (SELECT GPA
      				 FROM Student
      				 WHERE age <= 18);
      ```

  - Q11: Student names who take any class

    - ```sql
      SELECT name
      FROM Student AS S
      WHERE EXISTS(SELECT *
                   FROM Enroll AS E
                   WHERE E.sid = S.sid);
      ```

- What We Learned: Basic `SELECT` Query
  - `SELECT ... FROM ... WHERE`
    - Multiset semantics: duplicates are preserved unless `DISTINCT` is used
  - Set operators
  - Subqueries
    - Scalar-valued subquery
    - Set membership
    - Set comparison
    - Correlated subquery
  - Common table expression



## Lecture 5: Advanced SQL

- What's in this Lecture:

  - Aggregate Functions

    - Allows "aggregating" results from multiple tuples to produce a single output tuple

    - `AVG`, `SUM`, `COUNT`, `MIN`, `MAX` on single attribute

      - `COUNT(*)`: counts the number of matching tuples

    - `GROUP BY` and `SELECT` attributes

      - Is the following query meaningful?

        - ```sql
          SELECT sid, age, AVG(GPA)
          FROM Student
          GROUP BY age;
          ```

        - With `GROUP BY`, `SELECT` can have only aggregate functions or attributes that have a single value in each group

    - `HAVING` Clause

      - Check aggregate conditions
        - Example: Q6
      - Appear after `GROUP BY`

  - Window Functions

    - Introduced in SQL 2003
    - Syntax: `FUNCTION(attr) OVER()`
      - Use the same aggregate `FUNCTION(attr)`, but append `OVER()`
      - Example: `MAX(GPA) OVER()`
    - Interpretation
      - Generate one output tuple per input tuple, but `FUNCTION(attr)` is computed over all input tuples
    - Read Section 5.5 in the textbook for more information on window functions
      - `ORDER BY`, `RANK()`, `NTILE()`, window range, etc.

  - `CASE` Expression

    - Limited version of "if-then-else"

      - Returns different values depending on conditions

    - Syntax:

      - ```sql
        CASE
        	WHEN <condition> THEN <expr>
        	WHEN <condition> THEN <expr>
        	ELSE <expr>
        END
        ```

    - Can be used anywhere a column name can be referenced

      - `SELECT`, `WHERE`, `GROUP BY`, etc.

  - `ORDER BY` and `FETCH FIRST`

    - `ORDER BY`

      - SQL is based on multiset semantics

        - Duplicates are allowed
        - Tuple order is ignored

      - Still, for presentation purposes, it may be useful to order the result tuples by certain attribute(s)

        - Example: Order student tuples by GPA

          - ```sql
            SELECT sid, GPA
            FROM Student
            ORDER BY GPA DESC, sid ASC
            ```

        - Default is `ASC` (ascending) if omitted

        - `ORDER BY` doesn't change SQL semantics, it's purely for presentation

    - `FETCH FIRST`

      - Clause added in SQL 2008
      - `[ OFFSET <num> ROWS] FETCH FIRST <count> ROWS ONLY`
        - Skip the first `<num>` tuples and return the subsequent `<count>` rows
        - Unfortunately, this was standardized too late, many variations already existed
          - MySQL: `LIMIT <count> OFFSET <num>`
      - Changes multiset semantics

  - Data Modification in SQL

    - Insert tuple `(301, 'CS', 201, 1)` to `Enroll` table

      - ```sql
        INSERT INTO Enroll VALUES (301, 'CS', 201, 1);
        ```

    - Insert multiple tuples

      - ```sql
        INSERT INTO Enroll VALUES (301, 'CS', 201, 1), (302, 'CS', 143, 1);
        ```

    - Populate `Honors` table with students of `GPA > 3.7`

      - ```sql
        INSERT INTO Honors (SELECT *
                           FROM Student
                           WHERE GPA > 3.7);
        ```

    - Delete all students who are not taking classes

      - ```sql
        DELETE FROM Student
        WHERE sid NOT IN (SELECT sid
                         FROM Enroll);
        ```

      - Syntax: `DELETE FROM <relation> WHERE <condition>;`

    - Increase all CS course numbers by 100

      - ```sql
        UPDATE Class
        SET cnum = cnum + 100
        WHERE dept = 'CS';
        ```

      - Syntax:

        - ```sql
          UPDATE <relation>
          SET A1 = V1, ... , An = Vn
          WHERE <condition>;
          ```

- General SQL `SELECT`

  - ```sql
    SELECT <attributes>, <aggregates>
    FROM <relations>
    WHERE <conditions>
    GROUP BY <attributes>
    HAVING <aggregate_condition>
    ORDER BY <attributes>
    FETCH FIRST <n> ROWS ONLY
    ```

  - `SELECT` appears first, but is the last clause to be "interpreted"

- Example queries:

  - Q1: Average GPA of all students

    - Key challenge: we've been dealing with collecting information from one input tuple per output, we haven't learned how to combine information from multiple tuples into a single tuple yet

    - ```sql
      SELECT AVG(GPA)
      FROM Student;
      ```

      - Use of the aggregate function `AVG()`

  - Q2: Number of students taking CS classes

    - ```sql
      SELECT COUNT(DISTINCT sid)
      FROM Enroll
      WHERE dept = 'CS';
      ```

      - Example where duplicates can make our answer incorrect => must ensure `sid`s are distinct

  - Q3: Average GPA of students who take CS classes

    - ```sql
      SELECT AVG(GPA)
      FROM Student S, Enroll E
      WHERE S.sid = E.sid AND dept = 'CS';
      ```

      - Wrong => duplicate instances of students who are taking multiple CS courses
        - Cannot be fixed in same manner as Q2, since `GPA` is not a unique field

    - ```sql
      SELECT AVG(GPA)
      FROM Student
      WHERE sid IN
      (SELECT sid
      FROM Enroll
      WHERE dept = 'CS');
      ```

  - Q4: Average GPA for each age group

    - ```sql
      SELECT age, AVG(GPA)
      FROM Student
      GROUP BY age;
      ```

  - Q5: Number of classes each student takes

    - ```sql
      SELECT sid, COUNT(*)
      FROM Enroll
      GROUP BY sid;
      ```

      - What about students who take no classes (dangling tuples)?

  - Q6: Students who take 2+ classes

    - ```sql
      SELECT sid
      FROM Enroll
      WHERE COUNT(*) >= 2
      GROUP BY sid;
      ```

      - Wrong => the `WHERE` applies before the `GROUP BY`
        - In general, the aggregate functions shouldn't appear in the `WHERE` clause

    - ```sql
      SELECT sid
      FROM Enroll
      GROUP BY sid
      HAVING COUNT(*) >= 2;
      ```

  - Q7: Per each student, return their name, GPA, and the overall GPA average

    - ```sql
      SELECT name, GPA, AVG(GPA)
      FROM Student;
      ```

      - Wrong => the use of `AVG` without any other aggregate function expressions results in the input tuples collapsing into a single tuple

    - ```sql
      SELECT name, GPA, AVG(GPA) OVER()
      FROM Student;
      ```

  - Q8: Per each student, return their name, GPA, and the average GPA of their age group

    - Apply `AVG(GPA)` only within their "group"/"partition", not over the entire input tuples

      - Use `PARTITION BY` => similar to aggregate functions' `GROUP BY`

    - ```sql
      SELECT name, GPA, AVG(GPA) OVER(PARTITION BY age)
      FROM Student;
      ```

  - Q9: Average GPA within child/adult group

    - ```sql
      SELECT AVG(APG)
      FROM Student
      GROUP BY CASE
      	WHEN (age < 18) THEN 'child'
      	ELSE 'adult'
      END;
      ```

    - What if we want to show `child` and `adult` as part of the output tuples?

      - ```sql
        WITH S AS (SELECT CASE WHEN (age < 18) THEN 'child'
                   		       ELSE 'adult'
                   	      END age-group, GPA
                   FROM Student)
        SELECT age-group, AVG(GPA)
        FROM S
        GROUP BY age-group
        ```

  - Q10: Top 3 students ordered by their GPA

    - Sometimes, we just want a few rows from the result; is there a way to limit the result size?

    - ```sql
      SELECT *
      FROM Students
      ORDER BY GPA DESC
      FETCH FIRST 3 ROWS ONLY
      ```

      

## Lecture 6: Advanced SQL II

- SQL: More Tricky Details

  - `NULL` values

    - Dealing with `NULL`

      - Q: What will be returned from the following query if `GPA` is `NULL`?

        - ```sql
          SELECT name
          FROM Student
          WHERE GPA * 100 / 4 > 90;
          ```

        - When the `NULL` value is an input into one of the arithmetic operators, the result is `NULL`

        - We use three-valued logic to evaluate the `>` operator: value must be `True`, `False`, or `Unknown`

    - SQL is based on three-valued logic

      - SQL returns a tuple if the result from the condition is `True`

        - `False` or `Unknown` tuples will not be returned

      - Truth Table of Three-valued Logic

        - Assume `GPA` is `NULL` and `age` is `17`

          - Q: `GPA > 3.7 AND age > 18` - what is the result of this condition?

            - | AND       | True  | False | Unknown |
              | --------- | ----- | ----- | ------- |
              | **True**  | True  | False | Unknown |
              | **False** | False | False | False   |

          - Q: `GPA > 3.7 OR age > 18` - what is the result of this condition?

            - | AND       | True | False | Unknown |
              | --------- | ---- | ----- | ------- |
              | **True**  | True | True  | True    |
              | **False** | True | False | Unknown |

        - `NOT Unknown != Known`

    - `NULL` and Aggregates

      - Aggregate functions ignore tuples containing `NULL` values

        - Except `COUNT(*)`, which counts a `NULL` valued tuple as a valid tuple
        - Note that `COUNT(attr)` does ignore a `NULL` valued `attr`

      - When an input to an aggregate function is empty (= no input tuples):

        - `COUNT()` returns 0
        - All other aggregate functions return `NULL`

      - | sid  | GPA  |
        | :--: | :--: |
        |  1   | 3.0  |
        |  2   | 3.6  |
        |  3   | 2.4  |
        |  4   | NULL |

      - Q: What should be the result for the following queries?

        - ```sql
          SELECT SUM(GPA)
          FROM Student;
          ```

          - `9.0`

        - ```sql
          SELECT AVG(GPA)
          FROM Student;
          ```

          - `3.0`

        - ```sql
          SELECT COUNT(GPA)
          FROM Student;
          ```

          - `3`

        - ```sql
          SELECT COUNT(*)
          FROM Student;
          ```

          - `4`

    - `NULL` and Set Operators

      - `NULL` is treated like other regular values for set operators
      - Q: What should be the result of `{ 2.4, 3.0, NULL } ∪ { 3.6, NULL }`?
        - `{ 2.4, 3.0, 3.6, NULL }`

    - Checking `NULL`

      - In the case we need to explicitly check whether an attribute value is `NULL`, we can use the `IS NULL` or `IS NOT NULL` operators
        - Note that `= NULL` or `<> NULL` do not work => the output is always `Unknown`
      - `COALESCE()` function
        - Return first non-`NULL` value in the list
        - Example: `COALESCE(phone, email, addr)`
          - Tries `phone` field first, then `email`, then `addr`

  - Outer join

    - An outer join preserves dangling tuples

    - ```sql
      Student LEFT OUTER JOIN Enroll
      ON Student.sid = Enroll.sid;
      ```

      - Only dangling tuples from `Student` are preserved

    - ```sql
      Student RIGHT OUTER JOIN Enroll
      ON Student.sid = Enroll.sid;
      ```

      - Only dangling tuples from `Enroll` are preserved

    - ```sql
      Student FULL OUTER JOIN Enroll
      ON Student.sid = Enroll.sid;
      ```

      - All dangling tuples are preserved

    - More on `JOIN`s

      - MySQL doesn't support `FULL OUTER JOIN`
        - Only `LEFT` and `RIGHT` `OUTER JOIN`s
      - `R (INNER) JOIN S ON R.A = S.A`
        - Standard cross product with join condition `R.A = S.A`
      - `R NATURAL JOIN S`
        - Natural join from relational algebra
        - Equality on shared attributes
        - Keep only one copy of shared attributes

  - SQL and Multiset Semantics

    - Multiset (= Bag)
      - A set with duplicate elements
      - Order of elements doesn't matter
      - `{a, a, b, c} = {a, b, a, c} != {a, b, c}`
    - SQL is based on multiset semantics
      - We already learned how duplicates are generated and kept in SQL
      - Use `DISTINCT` to eliminate duplicates in the result
      - Exception: set operators are based on set semantics
    - Multiset Semantics for Set Operators
      - To use bag semantics for set operators, use the `ALL` keyword
        - `UNION ALL`, `INTERSECT ALL`, `EXCEPT ALL`
      - Q: `{ a, a, b } ∪ { a, b, c }`?
        - `{ a, a, a, b, b, c }`
      - Q: `{ a, a, a, b, c } ∩ { a, a, b }`?
        - `{ a, a, b }`
      - Q: `{ a, a, b, b } - { a, b, b, c }`
        - `{ a }`
      - Under multiset semantics:
        - `R ∪ S = S ∪ R`? => Always true
        - `R ∩ S = S ∩ R`? = ? Always true
        - `R ∩ (S ∪ T) = (R ∩ S) ∪ (R ∩ T)`? => No longer true
        - Be mindful that some rules change when operating under multi set semantics

  - Expressive power of SQL and recursion

    - SQL is a very expressive language, but its expressive power is limited

      - SQL is not a "Turing-complete" language

    - For example, the closure of a set cannot be computed using SQL92

      - Example: all ancestors, all reachable nodes
      - Support for recursion is needed to compute a closure

    - SQL99 added support for recursion

      - ```sql
        WITH RECURSIVE Ancestor(child, ancestor)
        AS (SELECT *
            FROM Parent)
            UNION
            (SELECT P.child, A.ancestor
             FROM Parent P, Ancestor A
             WHERE P.parent = A.child))
        SELECT ancestor
        FROM Ancestor
        WHERE child = 'Susan';
        ```

        - Union the base case with the recursive case

  - Example queries:

    - Q1: Number of classes each student takes, returning 0-class students as well

      - ```sql
        SELECT sid, COUNT(*)
        FROM Enroll
        GROUP BY sid;
        ```

        - Ignores students taking no classes

      - ```sql
        SELECT sid, COUNT(*)
        FROM Student S, Enroll E
        WHERE S.sid = E.sid
        GROUP BY sid;
        ```

        - Here, students that aren't present in the `Enroll` table have no partner to pair with, therefore they are represented by a dangling tuple and will be dropped from the final table

    - Q2: Find all ancestors of Susan

    - ```sql
      (SELECT parent
       FROM Parent
       WHERE child = 'Susan')
      UNION
      (SELECT P2.parent
       FROM Parent P1, Parent P2
       WHERE P1.parent = P2.child AND P1.child = 'Susan');
      ```

      - Would need to manually list all ancestor relations



## Lecture 7: Entity-Relationship Model

- Entity-Relationship (E/R) Model
  - Q: How should we design tables in our database?
    - Tables are not "given"
    - "Good" tables may not be easy to come up with
  - E/R model: graphical, intuitive, and "informal" representation of information on database
    - Used to "capture" what we learn from domain experts/database users
    - Not directly implemented by DBMS
    - Tools exist to automatically convert E/R model into tables
  - Two main components
    - Entity sets and relationship sets
- Entity Set
  - Entity: "thing" or "object" in the real world
    - e.g. me, a book, UCLA
  - Entity set: a set of entities (objects), like a class in OOP
    - Rectangle in ER
    - Consists of name and attributes
  - Entities with attributes can be thought of as "tuples" (or records)
    - `(301, John, 13 Hilgard, 18, 3.3)`, `(303, James, 12 De Neve, 19, 2.5)`
  - Key: a set of attributes that uniquely identifies an entity in an entity set
    - Underline in E/R
    - All entity sets in E/R need a key
- Relationship Set
  - Relationship: "connection" between entities
  - Relationship set: a set of relations of the same kind
    - Diamond in ER
    - Relationships can be thought of as "edges" between entities
  - Relationships can have attributes
  - Not all entities have to participate in a relationship
- Cardinality of Relationships
  - Cardinality: how many times do entities participate in a relationship?
    - One-to-one
      - Each entity can only participate in a single relationship
    - One-to-many
      - One entity in one set can participate in multiple relationships 
    - Many-to-many
      - Entities in either set can participate in multiple relationships
  - Cardinality: Add arrow on the "one" side
  - Total participation
    - Every entity participates in the relationship at least once
    - Double line in E/R model
- Meaning of Cardinality
  - Q: What does it mean?
    - Many-to-one in `Teach`?
      - One faculty member may teach multiple classes
      - Each class can have at most one faculty member teaching it
    - One-to-one in `Teach`?
      - One faculty member can teach at most one class
      - Each class can have at most one faculty member teaching it
    - Double-line between `Classes` and `Teach`?
      - All classes should be taught by at least one faculty member
    - Double-line and arrow between `Teach` and `Faculty`
      - All classes should be taught by at least one faculty member
      - Each class can have at most one faculty member teaching it
    - Double lines at both sides of `Teach` vs. one-to-one of `Teach`, are they the same?
      - No, the first ensures that entities on both sides must participate in the relationship, the second ensures each entity only participates in a single relationship
- General Cardinality Notation
  - Label an edge with `a..b`
    - The entity participates in the relationship between `a` through `b` times
    - `*` means unlimited
    - Don't get confused: for one-to-many relationship `0..*` appears on the "one" side and `0..1` appears on the "many" side
- N-ary Relationship
  - We may need more than a binary relationship sometimes
  - Ex: `Students`, `TAs`, and `Classes`
    - All TA's help all students
    - Each student is assigned to a particular TA
- Roles
  - We can designate a "role" to each entity set that participates in a relationship set
    - Labels on edges of a relationship in E/R model
    - Useful if an entity set participates more than once in a relationship
- Superclass and Subclass
  - ISA relationship in E/R connects superclass and subclass
  - Notes:
    - Specialization: superclass => subclass
    - Generalization: subclass => superclass
    - Subclass inherits all attributes of its superclass
    - Subclass participates in the relationships of its superclass
    - Subclass may participate in its own relationship
    - Disjoint specialization vs. overlapping specialization
      - Either-or vs. multiple specialization
      - Single hollow arrow vs. multiple hollow arrows (towards the superclass)
- Weak Entity Set
  - Entity set that doesn't have enough attributes to uniquely identify an entity
  - Double rectangle in E/R model
  - Part of its key comes from one or more entity sets it is linked to
    - Owner entity set: entity set providing part of the key
    - Identifying relationship: relationship between a weak entity set and owner entity set
      - Double diamond in E/R model
    - Discriminator: attributes in a weak entity set that become part of the key
      - Dashed underline
- E/R Design Principles
  - Often it is not clear what choices to make
    - One gigantic entity set with many attributes vs. many smaller entity sets?
    - Attribute vs. Entity set?
  - General rule of thumb for good design: avoid redundancy
    - Saying the same thing more than once
    - Space waste and potential inconsistency
  - Things to consider for entity set vs. attribute
    - Do we need more attributes than keys?
    - Is it a one-to-one relationship?
      - Create multiple entity sets for many-to-many or many-to-one relationships
- E/R to Relation
  - Converting E/R diagram to tables is mostly straightforward
    - Automatic conversion tools exist
  - (Strong) entity set: one table with all attributes for each entity set
  - Relationship set: one table with keys from the linked entity sets and its own attributes
    - If attribute names conflict, prefix them with entity set name
    - Look at the relationship cardinality to determine the key



## Lecture 8: Normalization

- Redundancies in Tables

  - | `sid` | `name` | `addr`   | `dept` | `cnum` | `title`           | `unit` |
    | ----- | ------ | -------- | ------ | ------ | ----------------- | ------ |
    | 301   | James  | 11 West  | CS     | 143    | Database          | 04     |
    | 105   | Elaine | 84 East  | EE     | 284    | Signal Processing | 03     |
    | 301   | James  | 11 West  | ME     | 143    | Mechanics         | 05     |
    | 105   | Elaine | 84 East  | CS     | 143    | Database          | 04     |
    | 207   | Susan  | 12 North | EE     | 128    | Microelectronics  | 03     |

  - The same information is included multiple times

  - Redundancy leads to potential anomalies down the road

    - Update anomaly: information may be updated partially and inconsistently

      - What if a student changes their address? They must change each entry related to that student

      wInsertion anomaly: We may not include some information at all

      - What if a student doesn't take a class? They would have to use a placeholder `NULL`

    - Deletion anomaly: while deleting information, we may delete others

      - What if the only class that a student takes gets cancelled? That student would then be removed from the database

  - Is there a better table design? What table(s) will we use?

    - `Class(dept, cnum, title, unit)`
    - `Student(sid, name, addr)`
    - `Enroll(sid, dept, cnum)`

- Coming up with Better Tables

  - Any way to arrive at the better design more systematically?
    - Where is the redundancy from?

- Intuition behind Normalization Theory

  - Functional Dependency (FD)
    - Some attributes are "determined" by other attributes:
      - e.g. `sid -> (name, addr)` and `(dept, cnum) -> (title, unit)`
    - When there is a functional dependency we may have redundancy
      - e.g. `(105, Elaine, 84 East)` is stored redundantly, so is `(CS, 143, database, 04)`

- "Decomposing" `StudentClass` Table
  - `StudentClass(sid, name, addr, dept, cnum, title, unit)`
    - FD: `sid -> (name, addr)`, `(dept, cnum) -> (title, unit)`
    - `A(sid, name, addr)`
    - `B(sid, dept, cnum, title, unit)`
    - `C(dept, cnum, title, unit)`
    - `D(sid, dept, num)`
  - Basic idea of "normalization"
    - Whenever there is FD, the table may be "bad" due to redundancy
    - We use FDs to split (or "decompose") table and remove the redundancy
  - We learn the functional dependency and decomposition theory as the next topic

- Overview
  - Functional Dependency (FD)
    - Definition
    - Trivial functional dependency
    - Logical implication
    - Closure
    - FD and key
  - Decomposition
    - Lossless decomposition
  - Boyce-Codd Normal Form (BCNF)
    - Definition
    - BCNF decomposition algorithm
  - Most theoretical part of the class, pay attention!
- Functional Dependency (FD)
  - Definition
    - Notation: `u[X]` - values for the attributes of `X` if tuple `u`
      - Ex: `u = (sid: 100, name: James, addr: Wilshire)`
        - `u[sid, name] = (100, James)`
    - Functional dependency `X -> Y`
      - For any `u_1, u_2 ∈ R`, if `u_1[X]` = `u_2[X]`, then `u_1[Y] = u_2[Y]`
      - Informally, `X -> Y` means "no two tuples in `R` can have the same `X` values but different `Y` values"
    - Ex: `StudentClass(sid, name, addr, dept, cnum, title, unit)`
      - Q: `sid -> name`? Yes
      - Q: `dept, cnum -> title, unit`? Yes
      - Q: `dept, cnum -> sid`? No
      - Whether FD holds or not is dependent on real-world semantics
  - Trivial Functional Dependency
    - Trivial FD: `X -> Y` is a trivial functional dependency when `Y ⊆ X`
      - `X -> Y` is always true regardless of real-world semantics
    - Non-trivial FD: `X -> Y` when `Y ⊄ X`
    - Completely non-trivial FD: `X -> Y` when `X ∩ Y = ∅`
  - Logical Implication
    - `R(A, B, C, G, H, I)`
      `F = { A -> B, A -> C, CG -> H, CG -> I, B ->H }`
    - Q: Is `A -> H` true given `F`?
      - `F` logically implies `A -> H`
    - Canonical database: a method to check logical implication
  - Closure
    - Closure of functional dependency set `F`: `F+`
      - `F+`: the set of all FDs that are logically implied by `F`
    - Closure of attribute set `X`: `X+`
      - `X+`: the set of all attributes that are functionally determined by `X`
      - Ex: what is `{ sid, dept, cnum }+` given `{ sid -> name, (dept, cnum) -> (title, unit) }`?
        - `X+ = { sid, dept, cnum, name, title, unit }`
    - Closure `X+` Computation Algorithm
      - Start with `X+ = X`
      - Repeat until there is no change in `X+`:
        - If there is `Y -> Z` with `Y ⊂ X+`, then `X+ <- ∪ Z`
    - Attribute Closure Example
      - `R(A, B, C, G, H, I)`
        `F = { A -> B, A -> C, CG -> H, CG -> I, B -> H }`
      - Q: `{A}+`?
        - `{ A, B, C, H }`
      - Q: `{A, G}+`
        - `{ A, G, B, C, H, I }`
  - Functional Dependency and Key
    - `R(A, B, C, G, H, I)`
      `F = { A -> B, A -> C, CG -> H, CG -> I, B -> H }`
    - Q: Is `{ A, G }` a key of `R`? Is `{ A, B }` a key of `R`?
    - `X` is a key of `R` if and only if
      - `X -> all attributes of R` (i.e. `X+ = R`)
      - No subset of `X` satisfies the first condition (i.e. `X` is minimal)
  - Projecting Functional Dependency
    - `R(A, B, C, D)`
      `F = { A -> B, B -> A, A -> C }`
    - Q: What FDs hold for `R'(B, C, D)`, which is a projection of `R`?



## Lecture 9: BCNF Decomposition

- Decomposition

  - Our previous "decomposition" example:

    - `StudentClass(sid, name, addr, dept, cnum, title, unit)` => `A(sid, name, addr)`, `B(sid, dept, cnum, title, unit)`

  - Hopefully, we can "remove redundancy" through a sequence of decompositions using FDs

  - General Decomposition

    - $$
      R(A_1, ..., A_n)\rightarrow R_1(A_1,...,A_i), R_2(A_j,...,A_n)\\\{A_1,...,A_n\}=\{A_1,...,A_i\}\cup\{A_j,...A_n\}
      $$

  - Lossless Decomposition

    - Q: When we decompose `R` to `R_1` and `R_2`, what should we watch out for?
      - Do not lose any data
    - Lossless-Join Decomposition
      - Decomposition of `R` into `R_1` and `R_2` is lossless-join decomposition if and only if `R = R_1 ⋈ R_2`
      - After a lossless-join decomposition, we can always get back the original table `R` if needed
      - Q: When is a decomposition lossless-join?
        - Q: Decomposition of `S(cnum, sid, name)` into `S1(cnum, sid)` and `S2(cnum, name)` => is it lossless?
          - No, duplicates break things
          - Decomposition into `S1(cnum, sid)` and `S2(sid, name)`? => is it lossless
            - Yes!
        - The decomposition `R(X, Y, Z) -> R1(X, Y), R2(Y, Z)` is lossless-join if `Y -> X` or `Y -> Z`
          - Shared attribute(s) are the key of one of the decomposed tables
          - This condition can be checked using FDs

- Boyce-Codd Normal Form

  - FD, Key, and Redundancy
    - Q: `StudentClass(sid, name, addr, dept, cnum, title, unit)` => does the FD `sid -> (name, addr)` cause redundancy under `StudentClass`
      - Yes, if `sid` is ever repeated
    - Q: `Student(sid, name, addr)` => does the FD `sid -> (name, addr)` cause redundancy under `Student`?
      - No, there would be no reason to store multiple copies of `sid`
    - Q: Why does the same FD cause reduncancy in one case, but not in the other?
      - In `StudentClass`, `sid` isn't a key, but in `Student` it is
      - If the LHS of a FD is a key, then it cannot introduce redundancy and vice versa
  - Relation `R` is in BCNF with regard to the set of FDs `F` if and only if for every non-trivial FD `(X -> Y) ∈ F+`, `X` contains a key
    - Informally, "normal form" means "good table design"
    - BCNF ensures that there is no redundancy in the table due to FD
  - When a table `R` is not in BCNF, we know that there is redundancy in the table and the design is "bad"
    - When table `R` violates the BCNF condition, we have to redesign the table so that the new design is in BCNF => "BCNF decomposition algorithm"
    - Decompose `R` until all decomposed tables are in BCNF

- BCNF Example 1

  - `Class(dept, cnum, title, unit)`
    FD `(dept, cnum) -> (title, unit)`
  - Intuitively, is it a good table design? Any redundancy? Any better design?
  - Q: Is it in BCNF?
    - Yes, the only functional dependency has an LHS that contains a key for this table

- BCNF Example 2

  - `Employee(name, dept, manager)`
    `F = { name -> dept, dept -> manager }`
  - Is it in BCNF?
    - `{ name }+ = { name, dept, manager }`
    - `{ dept }+ = { dept, manager }`
      - Violates BCNF, redundancy is present
      - Intuitively, detects that redundancy is present when many employees work for a department

- BCNF Violation and Table Decomposition

  - Decompose tables until all tables are in BCNF

    - For each FD `X -> Y` that violates the BCNF condition, separate those attributes out into another table to remove redundancy
    - We also have to ensure that this decomposition is lossless

  - BCNF Decomposition Algorithm

    - ```
      For any R in the schema
      	If (non-trivial X -> Y holds on R AND X doesn't contain a key), then
      		1) Compute X+ (Closure of X)
      		2) Decompose R into R1(X+) and R2(X, Z)
      			// X becomes the common attribute
      			// Z consists of all attributes in R except X+
      Repeat until no more decomposition
      ```

- BCNF Decomposition Example 1

  - `ClassInstructor(dept, cnum, title, unit, instructor, office, fax)`
    `F = { instructor -> office, office -> tax, (dept, cnum) -> (title, unit), (dept, cnum) -> instructor }`
  - Is it in BCNF?
    - `{ inst }+ = { inst, office, fax }` => violates BCNF
      - Decompose into `R1(inst, office, fax)` and `R2(inst, dept, cnum, title, unit)`
    -  `{ office }+ = { office, fax }` => violates BCNF
      - Decompose into `R3(office, fax)` and `R4(inst, office)`
    - `{ (dept, cnum) }+ = { dept, cnum, title, unit, inst }` => fine
  - Decomposed: `R2(inst, dept, cnum, title, unit)`, `R3(office, fax)`, and `R4(inst, office)`

- BCNF Decomposition Example 2

  - `R(A, B, C, G, H, I)`
    `F = { A -> B, A -> C, CG -> H, CG -> I, B -> H }`

  - Is it in BCNF?

    - `{ A }+ = { A, B, C, H }`
      - Decompose into `R1(A, B, C, H)` and `R2(A, G, I)`

    - `{ B }+ = { B, H }`
      - Decompose into `R3(B, H)` and `R4(A, B, C)`
    - Be careful of logicially implied FDs

  - Decomposed: `R2(A, G, I)`, `R3(B, H)`, and `R4(A, B, C)`

- Revisiting BCNF Decomposition Algorithm
  - Is guaranteed to guide lossless decomposition
- Uniqueness of BCNF Decomposition
  - Q: Does the BCNF decomposition algorithm always lead to a unique set of relations?
    - No, the order by which you decompose based on FDs that violate BCNF may change the resultant tables
  - Each possible resultant set of relations is equally good under BCNF criteria
- Good Table Design in Practice
  - Normalization splits tables to reduce redundancy
    - However, splitting tables has negative performance implication
  - As a rule of thumb, start with normalized tables and merge them if performance isn't good enough
- What We Learned
  - Relational design theory
  - Functional dependency
    - Trivial functional dependency
    - Logical implication
    - Closure
  - Decomposition
    - Lossless-join decomposition
  - Boyce-Codd Normal Form (BCNF)
    - BCNF decomposition algorithm
  - There exist other definitions of "Normal forms"
    - Third normal form, fourth normal form, etc.
    - BCNF is most useful and widely used



## Lecture 10: Database Integrity

- What We Will Learn

  - How can we ensure that data in our database is "consistent"?
    - Referential integrity constraint
    - `CHECK` constraint

- Data integrity Enforcement in RDBMS

  - Domain
    - `GPA` is `REAL`
    - `NOT NULL`
  - Integrity constraints
    - If violated, the DBMS generates and error and aborts
    - e.g., key, referential integrity, `CHECK`

- Key Constraint

  - A set of attributes should be unique in a table

  - `Class(dept, cnum, sec, unit, instructor, title)`

    - The primary key is `(dept, cnum, sec)`
    - The tuple `(dept, sec, title)` should be unique

  - ```sql
    CREATE TABLE Class(
    	dept CHAR(2) NOT NULL, cnum INT NOT NULL, sec INT NOT NULL,
        unit INT, instructor VARCHAR(100), title VARCHAR(100),
        PRIMARY KEY(dept, cnum, sec),
        UNIQUE(dept, sec, title)
    );
    ```

  - One `PRIMARY KEY` per table, others should be `UNIQUE`

    - `PRIMARY KEY` and `UNIQUE` are enforced through index (more on this later)

- Referential Integrity (RI)

  - Examples

    - If `sid` appears in `Enroll`, it should also appear in `Student`
    - If `(dept, cnum, sec)` appears in `Enroll`, it should also appear in `Class`

  - Q: Is the reverse true?

    - No!

  - Terminology

    - `E.B` references `S.B`
      - `E.B`: foreign key (= referencing attribute)
      - `S.B`: referenced attribute
    - Referential integrity
      - Referencing attribute should always exist in the reference attribute
      - When foreign key is `NULL`, no referential integrity check is performed

  - SQL Referential Integrity Syntax

    - ```sql
      CREATE TABLE Enroll(
      	sid INT,
          dept CHAR(2),
          cnum INT,
          sec INT,
          FOREIGN KEY(sid) REFERENCES Student(sid)
          FOREIGN KEY(dept, cnum, sec) REFERENCES Class(dept, cnum, sec)
      );
      ```

    - Referenced attributes must be `PRIMARY KEY` or `UNIQUE`
  
- Violation of Referential Integrity

  - Assume `E.B` references `S.B`

  - Q: When can RI be violated?

    - `INSERT INTO E`
    - `DELETE FROM S`
    - `UPDATE E`
    - `UPDATE S`

  - RI violation from referencing table `E` is never allowed

    - DBMS rejects the statement

  - RI violation from referenced table `S` is not allowed by default, but we can instruct the DBMS to "fix" the violation automatically

    - Q: How can we fix `DELETE FROM S`?

      - Delete the corresponding tuple from `E`
      - Change the referencing attribute to `NULL`

    - Q: How can we fix `UPDATE S`?

      - Update the corresponding tuple in `E`
      - Change the referencing attribute to `NULL`

    - Specifying Automatic Fix of RI Violation

      - Syntax

        - ```sql
          CREATE TABLE E(
          	A INT, B INT,
              FOREIGN KEY(B) REFERENCES S(B)
              ON UPDATE { CASCADE | SET NULL | SET DEFAULT }
              ON DELETE { CASCADE | SET NULL | SET DEFAULT }
          );
          ```

          - `CASCADE` cascades changes in the referenced attribute to the referencing attribute

- More Comments on Referential Integrity

  - Referential integrity is the only SQL constraint that can "fix itself"
    - Other constraints simply reject the statement and generate an error
  - Some DBMS do not support all "fixing" actions
    - Oracle supports `ON DELETE` , but not `ON UPDATE`, for example
  - Q: Why should referenced attributed be unique?
    - Non-uniqueness introduces ambiguity

- Self-Referencing Table

  - ```sql
    CREATE TABLE R(
    	A INT PRIMARY KEY,
        B INT,
        FOREIGN KEY(B) REFERENCES R(A)
        ON DELETE CASCADE
    );
    ```

  - Def: a table that references itself

  - `CASCADE` may be problematic

    - Cyclic relationship in the table => may delete the whole table

- Circular Constraint

  - How can we create the two tables?

    - Whenever we try to establish references, the other table doesn't exist yet

  - How can we insert tuples?

    - Whenever we try to establish references, the referenced attribute doesn't exist yet

  - Creating tables: `ALTER TABLE`

    - ```sql
      CREATE TABLE Chicken(cid INT PRIMARY KEY, eid INT);
      CREATE TABLE Egg(eid INT PRIMARY KEY, cid INT REFERENCES Chicken);
      ALTER TABLE Chicken ADD FOREIGN KEY(eid) REFERENCES Egg(eid);
      ```

  - Inserting tuples: two options

    - Create the first chicken or egg that came from nowhere (= `NULL`)

    - Create a chicken (and egg) that came from itself

      - ```sql
        INSERT INTO Chicken VALUES (1, NULL);
        INSERT INTO Egg VALUES (1, 1);
        UPDATE CHICKEN SET eid = 1 WHERE eid IS NULL;
        ```

- `CHECK` Constraint

  - Example:`GPA` should be between `0.0` and `4.0`

    - ```sql
      CREATE TABLE Student(
      	sid INT,
          name VARCHAR(50),
          addr VARCHAR(50),
          GPA REAL,
          CHECK(GPA >= 0 AND GPA <= 4)
      );
      ```

  - `CHECK(<condition>)`

    - `<condition>` can be any condition that may appear in a `WHERE` clause
      - May include subqueries

  - Constraint is attached to a particular table

    - Constraint is checked when the attached table is updated, and the statement is rejected if the condition is violated

  - Examples

    - Q1: `cnum` should be `< 600` and `unit` should be `< 10`

      - ```sql
        CREATE TABLE Class(
        	dept CHAR(2),
            cnum INT,
            sec INT,
            unit INT,
            title VARCHAR(100),
            instructor VARCHAR(100),
            CHECK(cnum < 600 AND unit < 10)
        );
        ```

    - Q2: The units of all CS classes should be `> 3`

      - ```sql
        CREATE TABLE Class(
        	dept CHAR(2),
            cnum INT,
            sec INT,
            unit INT,
            title VARCHAR(100),
            instructor VARCHAR(100),
            CHECK(dept <> 'CS' OR unit > 3)
        );
        ```

        - `A -> B` is the same as `~A or B`

    - Q3: Students whose `GPA < 2` cannot take CS classes

      - ```sql
        CREATE TABLE Student(
        	sid INT,
            name VARCHAR(50),
            GPA REAL,
            ...
        );
        
        CREATE TABLE Enroll(
        	sid INT,
            dept CHAR(2),
            cnum INT,
            sec INT,
            CHECK(dept <> 'CS' OR sid IN (SELECT sid
                                          FROM Student
                                          WHERE GPA >= 2))
        );
        ```

        - Q: When will the constraint be checked?
          - Only when the `Enroll` table is updated

    - Q4: Can we express referential integrity constraint using the `CHECK` constraint?

      - For example, can we express `Enroll.sid` is in `Student.sid`?

        - ```sql
          CREATE TABLE Student(
          	sid INT,
          	name VARCHAR(50),
          	GPA REAL,
          	...
          );
          
          CREATE TABLE Enroll(
          	sid INT,
          	dept CHAR(2),
          	cnum INT,
          	sec INT,
          	CHECK(sid IN (SELECT sid FROM Student))
          );
          ```

          - Won't check the constraint both ways, like an RI would

- MySQL Support

  - Domain constraint
  - Key constraint
  - Under InnoDB engine, referential integrity constant
    - But not under MyISAM engine
    - Does not support single-column `REFERENCES` shorthand
    - Cannot omit column names even if names are the same
  - No `CHECK` constraint support in standard MySQL
    - MariaDB supports (limited) `CHECK` constraint
  - Warning: MySQL silently ignores constraints that it doesn't support
    - No warning or error messages
    - You may believe the constraint is there when it isn't
    - It is safer to user the most conservative syntax

- What We Learned

  - How to preserve database integrity
  - Key constraint: `PRIMARY KEY`, `UNIQUE`
  - Referential integrity constraint
    - `FOREIGN KEY`
    - Reference attributes should be unique
    - Violation from referenced table may be fixed by DBMS
      - `ON DELETE`/`UPDATE CASCADE`/`SET NULL`
    - `CHECK` constraint



## Lecture 11: MongoDB and MapReduce

- JSON (JavaScript Object Notation)

  - Syntax to represent objects in JavaScript

    - `[{ "x": 3, "y": "Good" }, { "x": 4, "y": "Bad" }]`

  - One of the most popular data-exchange formats over internet

    - As JavaScript gained popularity, JSON's popularity grew
    - Simple and easy to learn
    - Other popular formats include XML, CSV, etc.

  - Basic JSON Syntax

    - Supports basic data types like numbers and strings, as well as arrays and "objects"

    - Double quotes for string: `"Best"`, `"UCLA"`, `"Worst"`, `"USC"`

    - Square brackets for array: `[ 1, 2, 3, "four", 5 ]`

    - Objects: `(attribute, name)` pairs => use curly braces

      - `{ "sid": 301, "name": "James Dean" }`

    - Things can be nested

      - ```json
        { "sid": 301,
          "name": { "first": "James", "last": "Dean" },
          "classes": [ "CS143", "CS144" ]}
        ```

- RDBMS for JavaScript Object Persistence

  - JavaScript applications need a "persistence layer" to store and retrieve JavaScript objects
  - Traditionally (until mid-2010) this was done with RDBMS
    - RDBMS as a massive, safe, efficient, multi-user storage engine
  - Q: How can we store JavaScript objects in RDBs?
  - "Impedance mismatch": Two choices
    - Store object's JSON as a string in a column
    - "Normalize" the object into a set of relations
    - Pros and cons of each approach?
  - Q: Can we just create a "native database" for JSON?

- MongoDB

  - Database for JSON objects

    - Perfect as a simple persistence layer for JavaScript objects
    - "NoSQL database"

  - Data is stored as a collection of documents

    - Document: (almost) JSON object
    - Collection: group of "similar" documents

  - Analogy

    - Document in MongoDB ~ row in RDB
    - Collection in MongoDB ~ table in RDB

  - MongoDB document

    - ```json
      {
          "_id": ObjectId(8df38ad8902c),
          "title": "MongoDB",
          "description": "MongoDB is NoSQL database",
          "tags": [ "mongodb", "database", "NoSQL" ],
          "likes": 100,
          "comments": [
              { "user": "lover", "comment": "Perfect!" },
              { "user": "hater", "comment": "Worst!" }
          ]
      }
      ```

      - `_id` field: primary key
        - May be of any type other than array
        - If not provided, automatically added with a unique `ObjectID` value
      - Stored as BSON (Binary representation of JSON)
        - Supports more data types than JSON
        - Does not require double quotes for field names

  - MongoDB philosophy

    - Adopts JavaScript's "laissez faire" philosophy
      - Don't be too strict! Be accommodating! Handle user requests in a "reasonable" way
    - Schema-less: no predefined schema
      - Give me anything, I will store it anywhere you want
      - One collection will store documents of *any* kind with no complaint
      - No need to "plan ahead"
        - A "database" is created when a first collection is created
        - A "collection" is created when a first document is inserted
      - Both a blessing and a curse

  - Basic MongoDB Commands

    - `mongo`:" start MongoDB shell
    - `use <dbName>`: use the database
    - `show dbs`: show list of databases
    - `show collections`: show list of collections
    - `db.colName.drop()`: delete `colName` collection
    - `db.dropDatabase`: delete current database
    - CRUD operations
      - `insertOne()`, `insertMany()`
      - `findOne()`, `find()`
      - `updateOne()`, `updateMany()`
      - `deleteOne()`, `deleteMany()`
    - Insertion: `insertX(doc(s))`
      - `db.books.insertOne({ title: "MongoDB", likes: 100 })`
      - `db.books.insertMany([ { title: "a" }, { title: "b"} ])`
    - Retrieval: `findX(condition)`
      - `db.books.findOne({ likes: 100 })`
      - `db.books.find({ $and: [ { likes: { $gte: 10 } }, { likes: { $lt: 20 } } ] })`
        - Other Boolean/comparison operators: `$or`, `$not`, `$gt`, `$ne`, etc.
    - Update: `updateX(condition, update_operation)`
      - `db.books.updateOne({ title: "MongoDB" }, { $set: { title: "MongoDB II" } })`
      - `db.books.updateMany({ title: "MongoDB" }, { $inc: { likes: 1 } })`
        - Other update operators: `$mul` (multiply), `$unset` (remove field), etc.
    - Deletion: `deleteX(condition)`
      - `db.books.deleteOne({ title: "MongoDB" })`
      - `db.books.deleteMany({ likes: { $lt: 100 } })`
  
  - MongoDB Aggregates
  
    - MongoDB supports complex queries through "aggregates"
    
    - MongoDB aggregates are very much like SQL's `SELECT` queries
    
      - Stages - SQL's `SELECT` clause
      - Pipeline - SQLs `SELECT` statement 
      
    - Example
    
      - ```json
        { id: 1, cust_id: "a", status: "A", amount: 50 }
        { id: 2, cust_id: "a", status: "A", amount: 100 }
        { id: 3, cust_id: "c", status: "D", amount: 25 }
        { id: 4, cust_id: "d", status: "C", amount: 125 }
        { id: 5, cust_id: "d", status: "A", amount: 25 }
        ```
    
      - ```mongodb
        db.orders.aggregate([
        	{ $match: { status: "A" }},
        	{ $group: {
        		_id: "$cust_id",
        		total: { $sum: "$amount" },
        		count: { $sum: 1 }
        	}},
        	{ $sort: { total: -1 }}
        ])
        ```
    
        - Equivalent to SQL's `SELECT`
          - Just `$match` is fine, for example
          - In `$group` stage, `_id` is "group by attributes"
    
    - Common Aggregate Stages
    
      - `$match` ~ `WHERE`
      - `$group` ~ `GROUP BY`
      - `$sort` ~ `ORDER BY`
      - `$limit` ~ `FETCH FIRST`
      - `$project` ~ `SELECT`
      - `$unwind`: replicate document per every element in the array
        - `{ $unwind: "y" }: { "x": 1, "y": [ 1, 2 ] } -> { "x": 1, "y": 1 }, { "x": 1, "y": 2 }`
      - `$lookup`: "look up and join" another document based on the attribute value
        - `{ $lookup: { from: <collection to join>, localField: <local join attr>, foreignField: <remote join attr>, as: <output field name> } }`
        - Matching documents are returned as an array in `<output field name>`
    
  - MongoDB vs. RDB
  
    - MongoDB
      - MongoDB document
        - Preserves structure
          - Nested objects
        - Potential redundancy
        - Restructuring or combining data is complex and inefficient
      - MongoDB: "laissez faire"
        - No explicit DB/collection creation
        - No schema => anything is fine
    - RDB
      - RDB tuple
        - "Flattens" data
          - Set of flat rows
        - Removes redundancy
        - Data can be easily "combined" using relational operators
      - RDB: "straightjacket"
        - Declare everything before use
        - Reject if not compliant
  
  - More on MongoDB
  
    - We learned just the basics
    - MongoDB has many more features
      - Transactions, replication, autosharding, etc.
    - Read MongoDB documentation and online tutorials to learn more
  
- MapReduce

  - Distributed Analytics using Cluster

    - Often, our data is non-relational (e.g. flat file) and huge
      - Billions of query logs, billions of web pages, etc.
    - Q: Can we perform analytics on large data quickly using thousands of machines?

  - Example 1: Search Log Analysis

    - Log of billions of queries => count the frequency of each query

      - Input query log:

        - ```
          cat, time, userid1, ip1, referrer1
          dog, time, userid2, ip2, referrer2
          ...
          ```

      - Output query frequency

        - ```
          cat 200000
          dog 120000
          ```

    - Q: How can we perform this task?

      - Step 1: "Transform" each line of the query log into `(query, 1)`
      - Step 2: Collect all tuples with the same query and aggregate them
      - Q: How can we parallelize the two steps?
        - Q: In step 1, can the transformation of each line be done independently of each other?
          - Yes! This step can be safely parallelized
        - Q: How do we parallelize the second "aggregation" step?
          - Move the tuples with the same query to the same machine
          - Perform aggregation on multiple machines in parallel



## Lecture 12: MapReduce and Disks

- MapReduce

  - Example 2: Web Indexing

    - 1 billion pages => build "inverted index"

      - Input documents:

        - ```
          1: cat chases dog
          2: dog loves cat
          ...
          ```

      - Output index:

        - ```
          cat 1, 2, 5, 10, 20
          dog 1, 2, 3, 8, 9
          ```

    - Q: How can we do this?

      - Step 1: "Transform" every document into `(word, doc_id)` tuples
      - Step 2: Collect all tuples with the same word and "aggregate" (or concatenate) the `doc_id`s
      - Q: How can we parallelize the two steps on multiple machines?
        - Q: How can the transformation of each document be done independently of each other?
          - Split input data into multiple independent chunks
          - Move each chunk to a separate machine
          - Perform "transformation" on multiple machines in parallel
        - Q: How can we parallelize the second "concatenation step"?
          - Move the tuples with the same word to the same machine
          - Perform aggregation on multiple machines in parallel

  - Generalization

    - "Mapping Step": Input data consists of multiple independent units
      - Examples
        - Query log: each line of the query log
        - Indexing: each web page
      - Partition input data into multiple "chunks" and distribute them to multiple machines
      - Transform/map input into `(key, value)` pairs
        - Query log: `query_log_line -> (query, 1)`
        - Indexing: `web_page -> (word1, page_id), (word2, page_id), ...`
    - "Reduction Step": Aggregate the types of matching keys
      - Examples:
        - Query log: `(query, 1), (query, 1), ... -> (query, count)`
        - Indexing: `(word, 1), (word, 3), ... -> (word, [1, 3, ...])`
      - Reshuffle tuples of the same key to the same machine
      - Collect and output the aggregation results

  - MapReduce Model

    - Programmer provides:
      - The map function: `unit_data -> (k, v), (k', v'), ...`
      - The reduce function: `(k, v1), (k, v2), ... -> (k, aggr(v1, v2, ...))`
    - MapReduce handles the rest
      - Automatic data partition, distribution, and collection
      - Failure and speed-disparity handling
    - Many systems exist supporting MapReduce model

  - Hadoop

    - First open-source implementation of MapReduce and GFS (Google File System)
      - Implemented in Java
    - User implements map and reduce functions

  - Spark

    - Open-source cluster computing infrastructure

    - Supports MapReduce and SQL

      - Supports data flow more general than simple MapReduce

    - Input data is converted into RDD (Resilient Distributed Dataset)

      - A collection of independent tuples
      - The tuples are automatically distributed and shuffled by Spark

    - Supports multiple programming languages

      - Scala, Java, Python, etc.
      - Scala and Java are much more performant than others

    - Example: Count Words

      - Count all words in a document

        - Dog loves cat but cat loves pig => `(cat, 2), (dog, 1), (pig, 1), (loves, 1), ...`

      - Spark Python code:

        - ```python
          lines = sc.textFile("input.txt")
          words = lines.flatMap(lambda line: line.split(""))
          word1s = words.map(lambda word: (word, 1))
          wordCounts = word1s.reduceByKey(lambda a, b: a + b)
          wordCounts.saveAsTextFile("output")
          ```

    - Key Spark Function

      - Transformation: Convert RDD tuple into RDD tuple(s)
        - `map()`: convert one input tuple into one output tuple
        - `flatMap()`: convert one input into multiple output tuples
        - `reduceByKey()`: specify how two input "values" should be aggregated
        - `filter()`: filter out tuples based on condition
      - Action: Perform "actions" on RDD
        - `saveAsTextFile()`: save RDD in a directory as text file(s)
        - `collect()`: create Python tuples from Spark RDD
        - `textFile()`: create RDD from text (each line becomes an RDD tuple)

  - What We Learned

    - Large-scale data analytics on distributed clusters
    - MapReduce model
    - Spark

- Disk

  - System architecture

    - CPU: where the computation occurs
    - Main memory: where the data accessed by the CPU resides
      - Transferred to CPU in words (1B - 64B) at a rate of ~100GB/s
    - Disk: where data is stored
    - Disk controller/system bus: facilitates the connection between disk and main memory
      - Transferred to main memory in blocks (512B - 50kB) at a rate of ~10GB/s
      - Much slower than the transfer between main memory and the CPU

  - Magnetic Disk vs. SSD

    - Magnetic disk
      - Stores data on a magnetic disk
      - Typical capacity: 1TB - 20TB
      - Structure of a Platter
        - Track, cylinder, sector (= block, page)
        - Data is transferred in the unit of blocks (not bytes) to amortize high access delay
    - Solid state drive (SSD)
      - Stores data in NAND flash memory
      - Typical capacity: 100GB - 10TB
      - Faster than magnetic disks, particularly random disk access
      - 5x more expensive and has limited write cycles (~2000)

  - Access Time

    - Q: How long does it take to read a page of disk to memory?
      - Reading a Page From Disk
        - Seek time
          - Time to move a disk head between tracks
          - Delay due to acceleration of the disk head
          - Average seek time: 10ms
        - Rotational delay
          - Depends on the speed at which the disk is spinning
          - Typical disk: 1000 rpm - 15000 rpm
          - Q: For 6000 rpm, what is the average rotational delay?
            - 1 full rotation => 10ms
            - 5ms
        - Transfer time
          - Q: How long will it take to read one sector on a disk with 6000 rpm and 10000 sectors/track
            - We need to do 1/10000th of a rotation
            - 0.01ms
      - `Access Time = Seek Time + Rotational Delay + Transfer Time`
    - Q: What needs to be done to read a page?

  - Transfer Rate

    - The rate at which we can transfer data from disk

      - Measured in bytes/sec

    - Q: 6000 rpm, 10000 sectors/track, 1kB/sector => what is the transfer rate of this disk?

      - Burst transfer rate vs. Sustained transfer rate

        - Burst transfer rate is the maximum rate that can be achieved

          - $$
            \frac{\text{RPM}}{60}\times\frac{\text{sectors}}{\text{track}}\times\frac{\text{bytes}}{\text{sector}}
            $$

        - Sustained transfer rate is the transfer rate over a long period of time

      - `1KB x 10000 = 10MB` every 10ms

      - 1000MB/sec

  - Random I/O

    - For magnetic disks:

      - Random I/O is VERY expensive compared to sequential I/O

    - For SSD disks:

      - Random I/O is still expensive, but not as much as for magnetic disks

    - Avoid random I/O to minimize delay

    - |                   | **Magnetic** | **SSD**      |
      | ----------------- | ------------ | ------------ |
      | **Random I/O**    | ~100 IO/sec  | ~100K IO/sec |
      | **Transfer rate** | ~100MB/sec   | ~10GB/sec    |

  - Buffers and Buffer Pool

    - Temporary main memory "cache" for disk blocks
      - Avoid future reads
      - Hide disk latency
      - Most DBMSs let users change buffer pool size

  - Abstraction by OS

    - Sequential blocks => no need to worry about head, cylinder, sector
      - Access to non-adjacent blocks => random I/O
      - Access to adjacent blocks => sequential I/O

  - Things to Remember

    - Platter, track, cylinder, block (sector)
    - `Access Time = Seek Time + Rotational Delay + Transfer Time`
    - Random I/O vs. Sequential I/O



## Lecture 13: Files and Index

- Files

  - Main Problem: how do we store tables into disks?

    - Q: Assume we have a 512B block and 80B tuples => how to store?
    - Spanned vs. Unspanned
      - Unspanned => Tuples must be contained in a single block, wasting extra space
      - Spanned => tuples may span over multiple blocks, maximizing space usage
      - Q: Maximum space waste for unspanned?
        - In our 512B/80B example, we're wasting 32B per block
        - If our tuples are 1 byte more than 50% of the block size, we would end up wasting 50% - 1B of the entire block => upper limit

      - Unspanned is more common

  - Variable-Length Tuples

    - Ex: `VARCHAR(100)`
    - How do we store them?
      - Reserved Space
        - Reserve the maximum space for each tuple
        - Q: Any problem?
          - May see a lot of wasted space, just like in an unspanned system
          - No upper limit on this wasted space

      - Variable-Length Space
        - Pack tuples tightly
        - Q: How do we know the end of a tuple?
          - Inclusion of a unique sequence of bytes that marks the end of a tuple
          - Store the length of the tuple

        - Q: What to do for update/delete?

          - Delete and insert at the end => results in fragmentation
            - We may need to reshuffle these fragments to resolve the issue

          - Insert and move everything behind it back => inefficient

        - Q: How can we "point to" a tuple?

          - We need the block number and the beginning location of the tuple
            - May be compromised by periodic updates to tuples => need to update the pointer

      - Slotted Page
        - 3 main ideas in data management for problem solving: sorting, hashing, adding a layer of abstraction
        - At the beginning of each block, we have an array of pointers that point to the location of the tuples

  - Long Tuples

    - Assume the following:

      - ```sql
        ProductReview(
        	pid INT,
            reviewer VARCHAR(50),
            date DATE,
            rating INT,
            comments VARCHAR(4000)
        )
        ```

      - Block size 512B

    - Q: How should we store it?

      - Splitting tuples
        - Most of the attributes are reasonably short, long attributes tend to be rare
          - Therefore, we treat long attributes as an exception

        - Long attributes are stored separately (often as a separate file)
          - These can then be dealt with separately

        - This way, queries to shorter attributes suffer no performance implications

  - Column-Oriented Storage

    - The storage methods discussed so far are row-oriented

    - For analytical queries, reading the entire row of a tuple may not be needed

      - Row-oriented storage forces us to read the entire row even if most columns are not needed for query processing => wasting disk I/Os

      - Example:

        - ```sql
          SELECT name FROM Students WHERE GPA > 3.7;
          ```

    - Idea is to store by column, not by row

    - Unneeded columns can be skipped for query processing

      - Better compression and caching behavior

    - Downsides:

      - Column values of matching rows must be "joined"
      - Insertion/update of a row is more expensive (multiple I/Os per row)

  - Sequential File

    - Tuples are ordered by certain attribute(s) (search key)

    - Example:

      - | name   | addr          | GPA  |
        | ------ | ------------- | ---- |
        | Elaine | 1 Le Conte    | 3.7  |
        | James  | 3 Mississippi | 2.9  |
        | John   | 12 Wilshire   | 1.8  |
        | Peter  | 4 Olympic     | 3.9  |
        | Susan  | 7 Pico        | 1.0  |
        | Tony   | 12 Sunset     | 2.4  |

      - Ordered by `name`

    - Inserting a new tuple

      - 2 options:

        - Rearrange in an array
        - Maintain a linked list

      - Q: What happens if the block the tuple should be inserted into is full?

        - Overflow page - reserving free space to avoid overflow

        - `PCTFREE` in DBMS

          - ```sql
            CREATE TABLE R(a INT) PCTFREE 40;
            ```

        - Slows down performance by forcing random I/O, breaking physical sequentiality while maintaining logical sequentiality

  - Things to Remember

    - Spanned/unspanned tuples
    - Variable-length tuples (slotted page data structure)
    - Long tuples
    - Row-oriented vs. column-oriented storage
    - Sequential file and search key
      - Problems with insertion (overflow page)
      - `PCTFREE`

- Index

  - Basic Problem

    - ```sql
      SELECT * FROM Student WHERE sid = 30;
      ```

    - | sid  | name    | GPA  |
      | ---- | ------- | ---- |
      | 20   | Susan   | 3.5  |
      | 60   | James   | 1.7  |
      | 70   | Peter   | 2.6  |
      | 40   | Elaine  | 3.9  |
      | 30   | Christy | 2.9  |

    - Q: How can we answer the query?

      - Iterate until we find `sid = 30`, then return

  - Random-Order File

    - How do we find `sid = 30`?
      - If there is no ordering to the data, we have to just scan through all tuples
      - Table sequenced by `sid` => use binary search




## Lecture 14: Index and B+Tree

- Index

  - Basic Idea

    - Build an "index" on the table
      - An auxiliary structure to help us quickly locate a tuple given a "search key"

  - Dense, Primary Index

    - Primary index (= clustering index)

      - Underlying table is sequenced by a key
      - Index is built on the same key (= search key)

    - Dense index

      - One `(key, pointer)` index entry per every tuple

    - Search algorithm

      - Find the key from index and follow the pointer
      - Possibly use binary search

    - Q: Why dense index, isn't binary search on the file the same?

      - Example:

        - 100,000,000 tuples (900B/tuple)
        - 4B search key, 4B pointer
        - 4096B block, unspanned tuples

      - Q: How many blocks for a table (how big)?

        - $$
          \lfloor\frac{4096}{900}\rfloor=4\text{ tuples/block}
          $$

        - $$
          \lceil\frac{100,000,000}{4}\rceil=25,000,000\text{ blocks}
          $$

        - $$
          4\text{KB}\times 25,000,000=100\text{GB}
          $$

      - Q: How many blocks for index (how big)?

        - $$
          \lfloor\frac{4096}{8}\rfloor=512\text{ index entries/block}
          $$

        - $$
          \lceil\frac{100,000,000}{512}\rceil=195,313
          $$

        - $$
          4\text{KB}\times 195,313\approx4\text{KB}\times200,000=800\text{MB}
          $$

      - Binary search on the table (100GB) requires disk access, while binary search on the index (800MB) can be done in main memory

  - Sparse, Primary Index

    - Primary Index
      - Index is built on the same search key as the underlying sequential file

    - Sparse Index
      - `(key, pointer)` pair per every "block"
      - `(key, pointer)` pair points to the first tuple in the block
      - We cannot tell if a given key exists from just the index, we must look at the underlying blocks
      - Smaller => better for disk I/O

    - Q: How can we find `80`?
      - Find the greatest search key less than `80`, follow the pointer to the block and search it

  - Multi-Level Index

    - Nth level of index points to another level of indexes, etc. (think multi-level page tables)
    - Q: Why multi-level index?
      - If the Nth underlying level is too big for main memory to cache, we can make another level of indexes to make caching easier

    - Q: Does a dense, 2nd level index make sense?
      - No, it would be the same size as the underlying level
      - Any Nth level index where `N > 1` must be sparse

  - Secondary (non-clustering) Index

    - Secondary (non-clustering) index
      - When tuples in the table are not ordered by the index search key
        - Index on a non-search-key for sequential file
        - Unordered file

      - Q: What index?
        - Does a sparse index make sense? No, the underlying table is unordered
        - First level must always be dense
          - Second level and on must be sparse

  - Overflow Problem

    - Q: What happens if we want to insert a tuple to a block which would overflow the block?
      - An overflow block needs to be created
      - In addition, an overflow index needs to be created to point to the overflow block
      - Over time, more and more disk I/Os would be required

    - Performance problems after many insertions
      - After many insertions, there can be a long chain of overflow pages
      - A new index needs to be rebuilt at this point

  - Indexed Sequential Access Method (ISAM)

    - Pros
      - Simple
      - Sequential blocks

    - Disadvantage
      - Not suitable for updates
      - Becomes ugly (loses sequentiality and balance) over time

  - Index Creation in SQL

    - ```sql
      CREATE INDEX <index_name> ON <table> (<attr>, <attr>, ...)
      ```

    - Example:

      - ```sql
        CREATE INDEX sid_idx ON Student (sid)
        ```

      - Creates a B+tree on the attributes

      - Speeds up lookup on `sid`

  - Primary (Clustering) Index

    - MySQL:

      - Primary key becomes the clustering index

    - DB2:

      - ```sql
        CREATE INDEX idx ON Student (sid) CLUSTER
        ```

      - Tuples in the table are sequenced by `sid`

    - Oracle: Index-Organized Table (IOT)

      - ```sql
        CREATE TABLE T (
        	...
        ) ORGANIZATION INDEX
        ```

      - B+tree on primary key

      - Tuples are stored in the leaf nodes of a B+tree

    - Periodic reorganization may still be necessary to improve range scan performance


  - Important Terms
    - Search key (!= primary key)
      - Primary keys are unique, primary attributes used to identify a tuple
      - A search key is the attribute being searched for and depends on the query
    - Primary index vs. secondary index
      - Clustering index vs. non-clustering index
    - Dense index vs. sparse index
    - Multi-level index
    - Indexed Sequential Access Method (ISAM)

- B+Tree

  - Most popular index structure in RDBMS

  - Pros

    - Suitable for dynamic updates
    - Balanced
    - Minimum space usage guarantee

  - Cons

    - Non-sequential index blocks

  - `n`: the number of pointer spaces in a node

  - Balanced: all leaf nodes are at the same level

  - Leaf Node

    - All pointers (except the last one) point to tuples
    - At least half of the pointer spaces are used

  - Non-Leaf Node

    - Points to the nodes one level below
      - No direct pointers to tuples

    - At least half of the pointer spaces used
      - Except the root, where at least 2 pointer spaces are used

  - Space Usage Guarantee

    - B+Tree nodes have at least:
      - Leaf (non-root): `⌈(n + 1) / 2⌉` pointers, `⌈(n + 1) / 2⌉ - 1` keys
      - Non-leaf (non-root): `⌈n / 2⌉` pointers, `⌈n / 2⌉ - 1` keys
      - Root: 2 pointers, 1 key

  - Search on B+Tree

    - Find a greater key and follow the link on the left

      - ```
        function find(value V)
        	/* Returns leaf node C and index i such that C.Pi points to first record
        	with search key value V */
        	Set C = root node
        	while (C is not a leaf node) begin
        		Let i = smallest number such that V <= C.Ki
        		if there is no such number i then begin
        			Let Pm = last non-null pointer in the node
        			Set C = C.Pm
                end else if (V = C.Ki) then
                	Set C = C.Pi+1
                else C = C.Pi // V < C.Ki
            end
            // C is a leaf node
            Let i be the least value such that Ki = V
            if there is such a value i then
            	return (C, i)
            else
            	return null // No record with key value V exists
        ```

  - B+Tree Insertion

    - No overflow
      - Run the traversal algorithm to find the space the tuple should be inserted in, and insert if there's space



## Lecture 15: B+Tree and Hash Index

- B+Tree

  - B+Tree Insertion

    - Leaf Overflow
      - The leaf node is split into 2 so that there is enough space
      - Keys are split between the nodes
      - The first key of the new node is copied to the parent, along with a pointer to the newly allocated node
      - We can stop as long as there is no overflow
      - Q: After split, are the leaf nodes always half full?
        - Yes, we only split when one node has more nodes than it can accommodate, therefore, since we assign half the keys to each node, they will always be half full

    - Non-Leaf Overflow
      - Starts with leaf overflow, but the parent node cannot fit the extra pointer/key
      - Split the non-leaf node in two, moving the middle key up to its parent
        - If the number of keys is even, either of the middle keys can be used

      - We can stop as long as there is no overflow
      - Q: After the split, are the non-leaf nodes at least half full?
        - Yes, same reason as above

    - New Root
      - Overflow traces all the way back up the tree
      - A new node is allocated, as there is no parent to move the middle node up to
      - The root is guaranteed to point to at least 2 nodes

- Summary
  - Leaf node overflow
    - The first key of the new node is copied to the parent
  - Non-leaf node overflow
    - The middle key is moved to the parent
  - Detailed algorithm is found in Figure 11.17

  - B+Tree Deletion

    - No Underflow
      - Traverse until the search key of interest is found
      - Check underflow conditions based on the minimum space guarantee

    - Leaf Underflow
      - Coalesce with neighbor
        - After deletion, the minimum space guarantee is not met
        - We can try to merge the node with its neighbor first to resolve the underflow
        - Check left and then right neighbor to see if there is available space to merge
        - If there's space, we need to update pointers in the merged node and delete the key from the parent
        - The parent must also be checked for underflow

      - Redistribute with neighbor
        - After deletion, the minimum space guarantee is not met
        - We try to borrow keys from the neighbor if merge fails
        - Redistribute the keys in the underflowed node from its siblings
        - Both nodes should end up roughly half full
          - Guaranteed, since we know the merge failed because both neighbors are too full

        - The key in the parent must now be updated, as the first key of one of the leaf nodes has changed
        - The parent node's value is simply being updated, so it cannot underflow

    - Non-Leaf Underflow
      - Coalesce with neighbor
        - Deletion of parent from leaf underflow merge results in non-leaf node underflowing
        - Merge with neighbor by pulling down the splitting key from the parent
        - Delete the pointer to the merged node

      - Redistribute with neighbor
        - Occurs when neighbors are too full to merge
        - Temporarily, make the node overflow by pulling down the splitting key and moving everything to the left
        - Apply the overflow handling algorithm (the algorithm used for B+tree insertion) to the overflowed node
          - Pick the new middle key in the node and move it to the parent
          - Move everything to the right of the middle key to the empty node that was merged from

    - Tree Depth Reduction
      - Merging of non-leaf nodes results in an empty root node
      - Delete the empty nodes, reducing the depth of the tree by 1

    - Important Points
      - Remember:
        - For leaf node merging, we delete the mid-key from the parent
        - For non-leaf node merging/redistribution, we pull down the mid-key from their parent

      - Exact algorithm: Figure 11.21

  - Where does `n` come from?

    - `n` determined by:

      - Size of a node
      - Size of a search key
      - Size of an index pointer

    - Q: What is `n` if the node is 1024B, the key is 10B, and the pointer is 8B?

      - $$
        8n+10(n-1)\le 1024\\n\le57.44\\n=57
        $$

  - Range Search on B+Tree

    - ```sql
      SELECT *
      FROM Student
      WHERE sid > 60;
      ```

    - B+Tree can also handle this, finding the limit and corresponding leaf nodes

- Hash Index

  - What is a Hash Table?
    - Hash Table
      - Hash function: `h(k): key -> [0...n]`
        - Should be very random and uniformly distributive

      - Array for keys: `T[0...n]`
      - Given a key `k`, store it in `T[h(k)]`

  - Hashing for DBMS (Static Hashing)
    - Think about individual disk blocks as buckets for the hash table
    - Overflow and Chaining
      - When the table size is small, we can experience all the benefits of hashing
      - When a disk block overflows, we need to create an overflow block, which creates a need to search through overflow chains to locate tuples
      - We lose the guarantee of a single lookup
      - Main problem: as data grows in size, overflow blocks become unavoidable




## Lecture 16: Hash Index and Joins

- Hash Index
  - Extendable Hashing
    - Basic Idea
      - Use `i` of `b` bits from hash output, increasing `i` as needed
        - Dynamically increase `i` as we use more and more hash bits
      - Add a level of indirection: directory of pointers to hash buckets
        - In order to retrieve a tuple, we pass the search key into a hash function, which sends us to a directory, where we find a pointer that points to the proper hash bucket
    - Insertion
      - Starts with `i = 0`
      - If no hash bucket overflow:
        - Insert the tuple into the hash bucket
      - If a hash bucket overflows:
        - If the hash bucket `i == directory i`, then:
          - Double the directory size by copying existing pointers from the directory
          - Increase `directory i` value by `1`
        - Split the overflowing hash bucket
          - Move tuples in the bucket to the new bucket based on their hash values
          - Update directory pointer
          - Increase the hash bucket `i` value by `1`
    - Deletion
      - Merge Condition
        - Hash bucket merge condition
          - Bucket `i`'s are the same
          - First `(i - 1)` bits of the hash key values are the same
        - Directory shrink condition
          - All bucket `i`'s are smaller than the `directory i`
        - Comes with performance implications
    - Questions on Extendable Hashing
      - Can we provide minimum space guarantee? No, space waste is possible
        - Can be mitigated to some degree with a good hash function
  - Summary
    - Static Hashing
      - Overflow and chaining
    - Extendable hashing
      - Can handle growing files
        - No periodic reorganizations
      - Indirection
        - Up to 2 disk accesses to access a key
      - Directory doubles in size
        - Not too bad if the data is not too large

- Hashing vs. Tree

  - Can an extendable-hash index support the following query efficiently?

    - ```sql
      SELECT *
      FROM R
      WHERE R.A > 5;
      ```

    - No, extendable hashing can only support point queries well, not range-based queries

  - Which one is better for the following query, B+tree or extendable hashing?

    - ```sql
      SELECT *
      FROM R
      WHERE R.A = 5;
      ```

    - Hash is supposedly faster, but this benefit is unlikely to be relevant => dataset would have to be really big

- Joins

  - Motivation

    - Q: How do we process:

      - ```sql
        SELECT *
        FROM Student
        WHERE sid > 30;
        ```

    - Q: How do we process:

      - ```sql
        SELECT *
        FROM Student S, Enroll E
        WHERE S.sid = E.sid;
        ```

  - 4 Join Algorithms: `R ⋈ S`

    - Nested-Loop Join (NLJ)

      - ```pseudocode
        for each r ∈ R:
        	for each s ∈ S:
        		if r.A = s.A, then output (r, s)
        ```

      - Poor performance on large tables

    - Index Join (IJ)

      - ```pseudocode
        create an index for S.A
        for each r ∈ R:
        	X := lookup index on S.A with r.A value
        	for each s ∈ X:
        		output (r, s)
        ```
  
      - Avoids scanning the entire table for each query
  
    - Sort-Merge Join (SMJ)

      - ```pseudocode
        sort R and S by A
        i = 1, j = 1
        while (i <= |R| and j <= |S|):
        	if (R[i].A = S[j].A) then:
        		output (R[i], S[j])
        		i++
        		j++
        	else if (R[i].A > S[j].A) then j++
        	else if (R[i].A < S[j].A) then i++
        ```

    - Hash Join (HJ)

      - Hash function: `h(v) => [1, k]`

      - Q: Given `(r ∈ R)` and `(s ∈ S)`, can `r` and `s` join if `h(r.A) != h(s.A)`?

        - No, if they could be joined, the hash output would be the same

      - Main idea

        - Partition tuples in `R` and `S` based on hash values on join attributes
        - Perform "joins" only between partitions of the same hash values
  
      - ```pseudocode
        // Hashing stage (bucketizing): hash tuples into buckets
        hash R tuples into G1, ... , Gk buckets
        hash S tuples into H1, ... , Hk buckets
        // Join stage: join tuples in matching buckets
        for i = 1 to k:
        	match tuples in Gi, Hi buckets
        ```
  
  - Comparison of Join Algorithms
  
    - Q: Which algorithm is better?
      - Q: What does "better" mean?
  
    - Ultimate bottom line: which algorithm is the "fastest"?
      - Q: How does the system know which algorithm runs fast? Run all join algorithms and pick the fastest one?

    - Cost Model
      - A model to estimate the performance of a join algorithm
        - Multiple cost models are possible depending on their sophistication
  
      - Our cost model: # of disk blocks that are read/written during join
        - Not perfect: ignores random vs. sequential I/O difference, CPU cost, etc.
        - Simple to analyze
        - "Good enough" to pick the best join algorithm
          - Cost of join is dominated by disk I/O
          - Most join algorithms have a similar disk access pattern
  
        - Our cost model ignores the last I/O for writing the final result
          - This cost is the same for all algorithms




## Lecture 17: Cost Model

- Cost Model

  - Running Example

    - Join two tables: `R ⋈ S`
    - `|R| = 1000` and `|S| = 10,000`
    - `b_R = 100 blocks` and `b_S = 1000 blocks` (`10 tuples/block`)
    - `M` = main memory cache, 22 disk blocks 

  - Sort-Merge Join (SMJ)

    - ```pseudocode
      sort R and S by A
      i = 1, j = 1
      while (i <= |R| and j <= |S|):
      	if (R[i].A = S[j].A) then:
      		output (R[i], S[j])
      		i++
      		j++
      	else if (R[i].A > S[j].A) then j++
      	else if (R[i].A < S[j].A) then i++
      ```

    - Take blocks into main memory cache from each table one at a time to compare

    - An extra block must be used to stage output tuples

    - When one block runs out, pull the next one into main memory

    - 1 disk I/O per block

      - Each block of `R` and `S` is read into main memory once

    - Cost of Join Stage of Sort-Merge Join

      - Q: Ignoring the final write of output, how many disk blocks are read during join?

        - `1000 + 100 = 1100 disk I/Os`

      - Q: We only used 3 memory blocks. Can we use the rest to make things better?

        - No, regardless of how many memory blocks we use, we must read/write the exact same number of disk block reads

      - In general:

        - $$
          b_R+b_S
          $$

  - Nested-Loop Join (NLJ)

    - ```pseudocode
      for each r ∈ R:
      	for each s ∈ S:
      		if r.A = s.A, then output (r, s)
      ```

    - Scan `S` table once for every tuple of `R`

    - One block for the current `R` block, one block for the current `S` block, and one block for staging output tuples

    - `100 + 1000 x 1000 = 1000000 disk I/Os`

    - Q: Can we do better?

      - Block Nested Loop Join
        - Scan `S` table once for every block of `R`
        - `100 + 100 x 1000 = 100100 disk I/Os`

      - We can still do better
        - Scan `S` table once for every 20 blocks of `R`
        - Make full use of main memory caching
        - `100 + 5 x 1000 = 5100 disk I/Os`

    - Q: What if we read `S` first?

      - `1000 + 50 x 100 = 6000 disk I/Os`
      - Generally, putting the smaller table on the left will make the join more efficient

    - In general:

      - $$
        b_R+\lceil\frac{b_R}{M-2}\rceil\times b_S
        $$

    - Summary

      - Always use block nested loop join (not the naïve algorithm)
      - Read as many blocks as possible for the left table in one iteration
      - Use the smaller table on the left (i.e., outer loop)

  - Hash Join (HJ)

    - Step (1): Hashing stage: `h(v) -> [1, k]`

    - Step (2): Join stage

    - Bucketizing Stage

      - Read `R` table and hash them into `k` buckets

        - One buffer for each bucket
        - One block used to read from `R`
        - Q: Given `M = 22`, what is the maximum `k`? => 21
        - Q: How many disk I/Os to bucketize `R`?
          - We read each block from `R` once to bucketize it
          - Each buffer tuple for each bucket is written out once
          - How many tuples will be sent to each bucket? => `100/21 ~ 5` on average
          - `105` disk writes
          - Approximates to `b_R`
          - `100 + 100 = 200`

      - Read `S` table and hash them into `k` buckets

        - Same process as with `R`
        - Approximates to `1000 + 1000 = 2000`

      - In general:

        - $$
          2b_R+2b_S=2(b_R+b_S)
          $$

    - Join Stage

      - Join tuples in `Gi` with those in `Hi`

      - Pull all blocks from `Gi` into main memory, then one block of `Hi` and produce the output

        - 7 blocks used
        - Each block in `Gi` is read once
          - Same with each block in `Hi`

      - What if `R` is large, and `Gi > M - 1 `?

        - Usually doesn't happen

        - We bucketize again with the hope that the resulting sub-buckets will fit into main memory

        - Recursive Partitioning

          - Use a new hash function `h'(v) -> [1, k]` to recursively partition `Gi` and `Hi` to even smaller partitions (until one of them fit in main memory)

          - \# of bucketizing steps needed for `R`:

            - $$
              \lceil\log_{M-1}\frac{b_R}{M-2}\rceil
              $$

            - In each bucketing step, the number of disk I/Os performed is:

              - $$
                2(b_R+b_S)
                $$

      - In general:

        - $$
          b_R+b_S
          $$

    - Overall:

      - $$
        \lceil\log_{M-1}\frac{b_R}{M-2}\rceil\times 2(b_R+b_S)+b_R+b_S
        $$




## Lecture 18: Cost Model and Query Optimization

- Cost Model

  - Index Join (IJ)

    - ```pseudocode
      create an index for S.A
      for each r ∈ R:
      	X := lookup index on S.A with r.A value
      	for each s ∈ X:
      		output (r, s)
      ```

    - Allocate a block for `R` tuples, a block for the index nodes, a block for `S` tuples, and a block for staging

    - Pull in tuples from `R`, look up the tuple in the index, then read the tuple from `S`

    - Cost is equivalent to the cost of scanning `R` plus the cost of the index look up plus the cost of reading `S`

    - IJ Example 1

      - 15 blocks for index
        - 1 root, 14 leaves
      - On average, 1 matching `S` tuple per `R` tuple
      - Q: How many disk I/Os? How should we use the memory?
        - Assuming a secondary index, there will be 1 disk I/O for each tuple in `R`
          - If there were 10 matching `S` tuples per `R` tuple:
            - In a secondary index, there could be up to 10 disk I/Os per `R` tuple
            - In a primary index, there would be 1-2 disk I/Os, as the tuples in `S` would be ordered by search key
        - We should use memory to cache more of the index blocks
          - `R` tuples are read exactly once, there is no need to cache them
          - `S` tuples are essentially being randomly accessed
          - We must perform index retrieval on the same index many times throughout the join, so they should be cached in the beginning of the join
      - Cost for `R` scan: 100 => each block of `R` is read once
      - Cost for index lookup: 15 => we retrieve and cache each index block once
      - Cost for read matching `S` tuple: 1000 => we do one disk I/O for retrieving each tuple in `R`

    - IJ Example 2

      - 40 blocks for index

        - 1 root, 39 leaves

      - On average, 10 matching `S` tuples per `R` tuple

      - Q: How many disk I/Os? How should we use the memory?

        - Caching `R` and `S` is not helpful, but we can no longer cache the entirety of the index
        - Each lookup must be used for the root node, but this is not necessarily true for each leaf node
        - We will cache the root node and as many leaf nodes as possible

      - Cost for `R` scan: 100 => each block of `R` is read once

      - Cost for index lookup:

        - Depending on which blocks are cached, each lookup may take either 0 or 1 disk I/Os

        - Take probabilities:

          - $$
            \frac{18}{39}\times0+\frac{21}{39}\times1=\frac{21}{39}\times1000
            $$

      - Cost for reading matching `S` tuple: 10,000 => 10 disk I/Os per tuple in `R`

    - In general:

      - $$
        b_R+|R|(C_I+C_S)
        $$

  - SMJ: Cost of Sorting

    - Q: How many disk I/Os during the sort stage?

      - Q: How can we sort `R`? => Merge-sort

        - Read as many disk blocks from `R` as possible into main memory

        - Apply a sorting algorithm on that subset of `R`

        - Generate a partition of sorted data

        - Results in 5 sorted runs for the global example

          - $$
            \lceil\frac{100}{22}\rceil=5
            $$

        - Q: How many blocks can we sort in each batch? => 22

          - Do we need to allocate one block for output? => No, use the input blocks

        - 200 disk I/Os => each block of `R` is read and written once

        - Q: What do we do with these sorted runs?

          - Maintain a pointer for each sorted run, incrementing the pointer for the value that is the smallest
          - Allocate a block in main memory for each sorted run and read blocks in one at a time
          - Place the smallest values into a staging block, which can then be written to main memory
          - 200 disk I/Os => each block of `R` is read and written once again

      - 400 total disk I/Os for sorting `R`

      - Q: What if the number of sorted runs is larger than the number of available blocks in main memory?

        - Q: How can we sort `S`?

          - $$
            \lceil\frac{1000}{22}\rceil=46
            $$

          - Too many sorted runs to fit into main memory (`M = 22`)

          - One main memory block is needed to stage the result, therefore 21 sorted runs can be merged at a time

            - Take the first 21, merge them and produce the output => repeat for all 46 sorted runs
            - Q: What do we do with the produced sorted runs?
              - Just repeat the same process with the new sorted runs, as we now have 3 sorted runs, which can fit into main memory

          - Each merging step takes `2b` disk I/Os

          - 6000 total disk I/Os to sort `S`

      - In general:

        - 1 initial sorting

        - Subsequent merging stages:

          - $$
            \lceil\log_{M-1}(\frac{b_R}{M})\rceil
            $$

        - `2b` disk I/Os per sorting/merging stage

        - $$
          2b_R(\lceil\log_{M-1}(\frac{b_R}{M})\rceil+1)
          $$

    - For both the sort and merge steps:

      - $$
        2b_R(\lceil\log_{M-1}(\frac{b_R}{M})\rceil+1)+2b_S(\lceil\log_{M-1}(\frac{b_S}{M})\rceil+1)+(b_R+b_S)
        $$

  - Cost of Join Algorithms

    - |         | Cost (M = 22, b_R = 100, b_S = 1000) |
      | ------- | :----------------------------------: |
      | **NLJ** |                 5100                 |
      | **SMJ** | 7500 (if unsorted), 1100 (if sorted) |
      | **HJ**  |                 3300                 |
      | **IJ**  |             1115 - 10640             |

    - Formulas (`b_R < b_S`)

      - NLJ:

        - $$
          b_R+\lceil\frac{b_R}{M-2}\rceil b_S
          $$

      - SMJ:

        - $$
          2b_R(\lceil\log_{M-1}(\frac{b_R}{M})\rceil+1)+2b_S(\lceil\log_{M-1}(\frac{b_S}{M})\rceil+1)+(b_R+b_S)
          $$

      - HJ:

        - $$
          2(b_R+b_S)\lceil\log_{M-1}\frac{b_R}{M-2}\rceil+(b_R+b_S)
          $$

      - IJ

        - $$
          b_R+|R|(C+J)
          $$

        - `C`: index lookup cost

        - `J`: # of matching `S` tuples per `R` tuple

  - Summary of Joins

    - NLJ is fine for "small" relations (relative to memory size)
    - HJ is usually the best for equi-join
      - If tables have not been sorted and with no index
      - Consider SMJ if tables have been sorted
      - Consider IJ if index exists

    - To pick the best, DBMS needs to maintain data statistics

  - Statistics Collection for DBMS

    - "Cost-based optimizer"

      - DBMS uses statistics on tables/indexes to pick the best query execution plan
      - Keeping correct stats is very important
        - Without correct stats, the DBMS may do stupid things

    - Oracle

      - ```sql
        ANALYZE TABLE <table> COMPUTE STATISTICS
        ```

      - ```sql
        ANALYZE TABLE <table> ESTIMATE STATISTICS
        ```

        - Cheaper than `COMPUTE`

    - DB2

      - ```sql
        RUN ON TABLE <userid>.<table> AND INDEXES ALL
        ```

    - MySQL doesn't have a cost-based optimizer

      - Rule-based optimizer: use simple heuristics only without looking at the actual data

- Query Optimization

  - `R(A, B)`, `S(B, C)`, `T(C, D)`

  - ```sql
    SELECT *
    FROM R, S, T
    WHERE R.B = S.B AND S.C = T.C AND R.A = 10 AND T.D < 30
    ```

  - Q: How can we process the above query?

    - In general  for `n` way joins:

      - $$
        \frac{(2(n-1))!}{(n-1)!}\text{ ways}
        $$

    - In reality, picking the very best is too difficult

    - DBMS tries to avoid "obvious mistakes" using a number of heuristics to examine only those plans that are likely to be good

      - Put the smallest table on the left
      - "Left-deep" tree
      - Push selection as deep as possible
      - ...

    - For 90% of queries, DBMS picks a good query execution plan

      - To optimize the remaining 10%, companies pay big money to databse consultants

  - Looking at Query Plan

    - Many systems allow users to look at query plan

      - No SQL standard
      - Different systems use different syntax

    - Examples:

      - MySQL, PostgreSQL:

        - ```sql
          EXPLAIN SELECT...
          ```

      - Oracle:

        - ```sql
          EXPLAIN PLAN FOR SELECT...
          ```

      - MS SQL Server:

        - ```sql
          SET SHOWPLAN_TEXT ON
          ```




## Lecture 19: Transactions

- Motivation

  - Crash recovery

    - Example: Transfer $1M from Susan to Jane

    - ```sql
      S1: UPDATE Account SET balance = balance - 1000000 WHERE owner = 'Susan'
      S2: UPDATE Account SET balance = balance + 1000000 WHERE owner = 'Jane'
      ```

    - Imagine the system crashes after `S1` but before `S2`: what now?

    - Q: How can DBMS guarantee that these "bad" scenarios will never happen?

- Transaction

  - A sequence of SQL statements that are executed as "one unit"

  - Two key commands related to transaction:

    - After a sequence of SQL commands, the user can issue either `COMMIT` or `ROLLBACK`
    - `COMMIT`
      - "I am done, commit everything I've done"
      - All changes made by the transaction must be stored permanently
    - `ROLLBACK`
      - "I changed my mind. Ignore what I just did"
      - Undo all changes made by the transaction

  - Creating a Transaction

    - All SQL commands until `COMMIT`/`ROLLBACK` become one transaction

  - ACID Property of Transaction

    - DBMS guarantees ACID property on all transactions
      - Atomicity: "all or nothing"
        - Either all or none of the operations in a transaction is executed
        - If the system crashes in the middle of a transactions, all changes are undone
      - Consistency
        - If the database was a in a "consistent" state before the transaction, it is still in a consistent state after the transaction
      - Isolation
        - Even if multiple transactions run concurrently, the final result is the same as if each transaction ran in isolation in a sequential order
      - Durability
        - All changes made by committed transactions will remain even after a system crash

  - Autocommit Mode

    - Sometimes, it is too inconvenient to declare transactions explicitly

    - Autocommit mode

      - When on, every SQL statement automatically becomes one transaction
      - When off, DBMS behaves as usual, all SQL commands through `COMMIT`/`ROLLBACK` become one transaction

    - Setting Autocommit Mode

      - Oracle:

        - ```sql
          SET AUTOCOMMIT ON/OFF
          ```

        - Default is off

      - MySQL:

        - ```mysql
          SET AUTOCOMMIT = {0 | 1}
          ```

        - Default is on

        - InnoDB only

      - MS SQL Server:

        - ```sql
          SET IMPLICIT_TRANSACTIONS OFF/ON
          ```

        - Default is off

        - `IMPLICIT TRANSACTION ON` means `AUTOCOMMIT OFF`

      - DB2:

        - ```sql
          UPDATE COMMAND OPTIONS USING c ON/OFF
          ```

        - Default is on

      - JDBC

        - ```jdbc
          connection.setAutoCommit(true/false)
          ```

        - Default is on

      - In Oracle, MySQL, and MS SQL Server, `BEGIN TRANSACTION` command temporarily disables autocommit mode until `COMMIT` or `ROLLBACK`

- SQL Isolation Levels

  - By default, RDBMS guarantees ACID for transactions

  - Some scenarios may not need ACID and may want to allow minor "bad scenarios" to gain more "concurrency"

  - By specifying "SQL Isolation Level", app developers can specify what types of "bad scenarios" can be allowed for their apps

    - Dirty read, non-repeatable read, and phantom

  - |                      | Dirty Read | Non-Repeatable Read | Phantom |
    | -------------------- | :--------: | :-----------------: | :-----: |
    | **Read uncommitted** |     Y      |          Y          |    Y    |
    | **Read committed**   |     N      |          Y          |    Y    |
    | **Repeatable read**  |     N      |          N          |    Y    |
    | **Serializable**     |     N      |          N          |    N    |

  - Dirty Read

    - Reading a value from an uncommitted transaction

    - Example:

      - ```sql
        T1: UPDATE Employee SET salary = salary + 100;
        T2: SELECT salary FROM Employee WHERE name = 'Amy';
        ```

      - Q: Under ACID, once `T1` updates Amy's salary, can `T2` read Amy's salary?

        - No, we have no idea if `T1` will be rolled back
        - Under ACID, `T2` has to wait for `T1` to be committed

      - If we just want an estimate of Amy's salary, we may be fine performing a dirty read

        - Among 4 SQL isolation levels, `READ UNCOMMITTED` allows dirty reads

  - Non-repeatable Read

    - When `Ti` reads the same tuple multiple times, `Ti` may get a different value

    - Example:

      - ```sql
        T1: UPDATE Employee SET salary = salary + 100 WHERE name = 'John';
        T2: (S1) SELECT salary FROM Employee WHERE name = 'John';
        	...
        	(S2) SELECT salary FROM Employee WHERE name = 'John';
        ```

      - Q: Under ACID, can `T2` get different values for `S1` and `S2`?

        - No, because of atomicity and isolation

    - `READ UNCOMMITTED` and `READ COMMITTED` allow non-repeatable read

  - Phantom

    - When new tuples are inserted, statements may or may not see (part of) them

      - Preventing phantom can be very costly
      - Exclusive lock on the entire table or a range of tuples

    - Example:

      - ```sql
        T1: INSERT INTO Employee VALUES (Beverly, 1000), (Zack, 1000);
        T2: SELECT SUM(salary) FROM Employee;
        ```

      - Q: Under ACID, what may `T2` return?

        - 5000 if `T1` is executed after `T2` or 7000 if `T1` is executed before `T2`

      - Imagine `T2` starts executing and `T1` begins in the middle of execution

        - `T1` then inserts a tuple in a section that `T2` has already summed and commits
        - Neither tuple is read multiple times, therefore no non-repeatable read occurred
        - By the time `T2` reads `Zack`, it is a committed value, so no dirty read occurred
        - This results in a sum of 6000

    - Phantoms are allowed, except in the `SERIALIZABLE` isolation level

  - Access Mode

    - A transaction can be declared to be read only, when it has `SELECT` statements only (no `INSERT`, `DELETE`, `UPDATE`)
    - DBMS may use this information to optimize for more concurrency

  - Declaring SQL Isolation Level

    - ```sql
      SET TRANSACTION [READ ONLY] ISOLATION LEVEL <level>
      ```

      - Example:

        - ```sql
          SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
          ```

    - More precisely:

      - ```sql
        SET TRANSACTION [access_mode] ISOLATION LEVEL <level>
        ```

        - `access_mode`: `READ ONLY`/`READ WRITE` (default: `READ WRITE`)
        - `level`:
          - `READ UNCOMMITTED`
          - `READ COMMITTED` (default in Oracle, MS SQL Server)
          - `REPEATABLE READ` (default in MySQL, IBM DB2)
          - `SERIALIZABLE`
        - `READ UNCOMMITED` is allowed only for `READ ONLY` access mode

    - Isolation level needs to be set before every transaction

  - Mixing Isolation Levels

    - Example:

      - John's initial salary is 1000

      - ```sql
        T1: UPDATE Employee SET salary = salary + 100; ROLLBACK;
        T2: SELECT salary FROM Employee WHERE name = 'John';
        ```

      - Q: `T1` and `T2` are `SERIALIZABLE`, what may `T2` return? => 1000

      - Q: `T1` is `SERIALIZABLE` and `T2` is `READ UNCOMMITTED`, what may `T2` return => 1000 or 1100

        - ACID is only guaranteed for `T1`, not `T2`

    - Isolation level is in the eye of the beholding operation

      - Global ACID is guaranteed when all transactions are `SERIALIZABLE`

- Logging

  - Guaranteeing ACID

    - ```sql
      T1: UPDATE Student SET GPA = 3.0 WHERE sid = 30;
      ```

      - To perform this update, we read the disk block containing the target tuple into main memory from disk, update it in main memory, then write it back to disk => guarantees durability

    - DBMS doesn't immediately write the update disk block back to disk for performance reasons

      - Q: What happens if the system crashes before the block is written back?
        - Durability is violated
      - Q: Is there a way to maintain durability without incurring the performance implications of writing tuples back to disk after every transaction?
        - Log changed values, send the log to disk

  - Rolling Back to Earlier State

    - ```pseudocode
      T: read(A), write(A), read(B), write(B)
      ```

    - Q: What if we execute up to `read(B)` and decide to `ROLLBACK`? How can we go back to the old value of `A`?

      - Log all old values of updated tuples

  - Partial Execution

    - ```pseudocode
      T: read(A), write(A), read(B), write(B)
      ```

    - Q: What if the system executes up to `write(A)` and the system crashes? What should the system do when it reboots? How does the system know whether `T` did not finish?

      - The system needs to log what has been executed so that it may either finish `T` or rollback, ensuring atomicity

  - Intuition

    - In a separate log file, save the following log records before `Ti` takes any action:

      - | Log record                      | When                                                         |
        | ------------------------------- | ------------------------------------------------------------ |
        | `<Ti, start>`                   | Before transaction `Ti` starts                               |
        | `<Ti, commit/abort>`            | Before transaction `Ti` is committed/aborted                 |
        | `<Ti, X, old-value, new-value>` | Before a statement in `Ti` changes value of `X` from `old-value` to `new-value` |

    - These records are used during `ROLLBACK` or during crash recovery

  - Rules for Log-Based Recovery

    - DBMS generates a log record before start and end and modification by `Ti`
    - Before `Ti` is committed, all log records until `Ti`'s commit must be flushed to disk
    - Before any modified tuple is written back to disk, all log records through the tuple modification must be flushed to disk first
      - Example: the log record `<Ti, A, 5, 10>` should be written to the disk before the tuple `A` is updated to `10` in disk
    - During `ROLLBACK`, DBMS reverts to old value of tuples using log records
    - During crash recovery, DBMS:
      - Re-executes all actions in the log file from beginning to end
      - Rolls back all actions from non-committed transactions in the reverse order

  - Summary

    - DBMS uses a log file to ensure ACID for transactions
      - Helps rolling back partially executed transactions
      - Helps recovery after crash
    - Before modifying any data, DBMS generates a log record
    - Before commit, DBMS flushes records to disk to ensure durability
    - During recovery, records in the log file are replayed to put the system in the proper state

  

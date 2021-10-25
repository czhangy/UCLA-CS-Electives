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

- BCNF Deomposition Example 2

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



## Lecture 10:

- 


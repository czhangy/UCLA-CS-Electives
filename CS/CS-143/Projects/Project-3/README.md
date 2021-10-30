# Data Conversion

## Goal

**Load JSON data loaded from the Nobel laureates API into MySQL and develop a PHP page to query this data.**

## Requirements

- Download the JSON data file:

  - ```bash
    mkdir -p /home/cs143/data
    cd /home/cs143/data
    wget http://oak.cs.ucla.edu/classes/cs143/project3/nobel-laureates.json
    ```

- Design a relational schema to store the following JSON properties:

  - ```json
    "laureates": {
        "id", "givenName", "familyName", "gender",
        "birth": {
            "date",
            "place": {
                "city", "country"
            }
        },
        "orgName",
        "founded": {
            "date",
            "place": {
                "city", "country"
            }
        },
        "nobelPrizes": [{
            "awardYear", "category", "sortOrder",
            "affiliations": [{
                "name", "city", "country"
            }]
        }]
    }
    ```

  - Any other properties should be discarded

  - If other language options are given, just store the English ones

  - Design process:

    - List relations and keys, ignoring attribute types for now
    - Identify any non-trivial FDs that hold on each relation
    - Redesign relations until they are in BCNF, or justify to yourself why they shouldn't be
    - Reduce any other redundancies or inefficiencies

- Write a data transformation program

  - Use either JavaScript, Python, or PHP to implement the converter
    - Use the skeleton code provided
  - The program should take the input JSON data from `/home/cs143/data/nobel-laureates.json` and produce a set of SQL load files in the current directory
    - These load files should be named according to the corresponding relation of the schema, using `.del` as an extension
  - The entire convert process should be done by `convert.sh`
  - Duplicates should be eliminated in the script, not during the bulk-loading process

- Load the data into MySQL

  - Issue a set of `CREATE TABLE` commands for all the relations in the schema
  - Use `LOAD DATA LOCAL INFILE` to bulk load the data from the generated `.del` files
  - Create a script called `load.sql` that drops all existing Project 3-related tables, recreates them according to the schema, and populates the tables using the SQL load files in the current directory

- Write SQL queries

  - Write SQL script files named `q1.sql` through `q5.sql` that answer the following queries properly:
    - What is the `id` of Marie Curie? (`6`)
    - What country is the affiliation `CERN` located in? (`Switzerland`)
    - Find the family names associated with 5+ Nobel prizes (`Smith`, `Wilson`)
    - How many different locations does the affiliation `University of California` have? (`6`)
    - In how many years was a Nobel prize awarded to an organization in at least one category? (`26`)

- Implement a web service

  - Takes the Nobel laureate's ID and returns the JSON data associated with them
    - Should be pure JSON data, not enclosed in HTML tags
    - Preserve the `en` property name whenever possible
  - The schema of the returned "JSON" should be as close as possible to the original data
  - Modify the skeleton code in `laureate.php`



## Deliverables

- Coding
  - [ ] Download the JSON data
  - [ ] Design the relational schema
  - [ ] Write the data transformation program
  - [ ] Load the data into MySQL, creating necessary scripts
  - [ ] Write the SQL queries and debug
  - [ ] Implement the PHP page
- Submission
  - [ ] Download and use `p3_package` to zip all required files
  - [ ] Download and use `p3_test` to test the submission
  - [ ] Submit to Gradescope



## Related Links

- **[Introduction to MySQL](https://oak.cs.ucla.edu/refs/mysql/cs143.html)**
- [**JSON Tutorial**](https://www.tutorialspoint.com/json/index.htm)
- **[JSON Validator](https://jsonlint.com/)**
- **[MySQL `LOAD DATA` Documentation](https://dev.mysql.com/doc/refman/8.0/en/load-data.html)**
- **[PHP JSON Parsing Tutorial](https://www.tutorialrepublic.com/php-tutorial/php-json-parsing.php)**


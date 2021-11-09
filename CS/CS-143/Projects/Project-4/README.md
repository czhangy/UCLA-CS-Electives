# NoSQL – MongoDB

## Goal

**Load JSON data loaded from the Nobel laureates API into MongoDB and develop a PHP page to query this data.**

## Requirements

- Download and run the new MongoDB container using:

  - ```bash
    docker run -it -v {your_shared_dir}:/home/cs143/shared -p 8889:80 --name mongo-apache junghoo/mongo-apache
    ```

- Download the JSON data file:

  - ```bash
    mkdir -p /home/cs143/data
    $ cd ~/data/
    $ wget http://oak.cs.ucla.edu/classes/cs143/project3/nobel-laureates.json
    ```

- Write a program to load the data from the JSON to MongoDB

  - The program should be written in PHP, JavaScript, or Python

    - Use the skeleton code provided in `convert.zip`
    - Conversion should be fully executed by the command `./convert.sh`

  - Every laureate in the data should be loaded as a separate document

  - Data should be taken from `/home/cs143/data/nobel-laureates.json`

  - The data should be loaded using the command-line tool `mongoimport`

    - ```bash
      mongoimport --drop --db=nobel --collection=laureates laureates.import
      ```

  - Once the import file `laureates.import` has been created, test the database by issuing `find()` commands

    - Note that the `id` field is a string

- Implement a web service

  - Modify the skeleton code in `laureate.php`
  - Takes the Nobel laureate's ID and returns the JSON data associated with them
    - Must include all attributes in the original dataset and no extra attribute that was not present, such as `_id`

- Write aggregate queries

  - Write SQL script files named `q1.js` through `q5.js` that answer the following queries properly:

    - What is the `id` of Marie Curie? (`{ "id": "6" }`)
    - What country is the affiliation `CERN` located in? (`{ "country": "Switzerland" }`)
    - Find the family names associated with 5+ Nobel prizes (`{ "familyName": "Smith" }`, `{ "familyName": "Wilson" }`)
    - How many different locations does the affiliation `University of California` have? (`{ "locations": "6"}`)
    - In how many years was a Nobel prize awarded to an organization in at least one category? (`{ "years": "26" }`)

  - All queries should be able to be run by:

    - ```bash
      mongo nobel --quiet < q?.js
      ```

      

## Tasks

- [x] Coding
  - [x] Download and run the `mongo-apache` container
  - [x] Download the JSON data
  - [x] Write a program that loads the data to MongoDB
  - [x] Implement a PHP web service that connects to MongoDB
  - [x] Write aggregate queries and debug
- [x] Submission
  - [x] Download and use `p4_package` to zip all required files
  - [x]  Download and use `p4_test` to test the submission
  - [x] Submit to Gradescope

## Related Links

- **[`find()` Documentation](https://docs.mongodb.com/v4.4/tutorial/query-documents/)**
- **[JSON Validator](https://jsonlint.com/)**
- **[MongoDB Aggregate Framework](https://studio3t.com/knowledge-base/articles/mongodb-aggregation-framework/)**
- **[MongoDB Aggregate References](https://docs.mongodb.com/v4.4/reference/aggregation/)**
- **[MongoDB PHP Driver](https://oak.cs.ucla.edu/classes/cs143/project4/php-mongodb.html)**
- **[MongoDB Tutorial](https://www.tutorialspoint.com/mongodb/index.htm)**
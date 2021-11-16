# Distributed Computing with Apache Spark

## Goal

**Use the Apache Spark engine to identify the most frequent book pairs reviewed by the same users on GoodReads, using the Map-Reduce framework.**

## Requirements

- Download and run the new MongoDB container using:

  - ```bash
    docker run -it -p 4040:4040 -v {host_shared_dir}:/home/cs143/shared --name spark junghoo/spark
    ```

- Download the book review dataset:

  - ```bash
    mkdir -p /home/cs143/data
    cd ~/data/
    wget http://oak.cs.ucla.edu/classes/cs143/project5/goodreads.user.books
    ```

- Write a program to output the pairs of book IDs that appear frequently together in the users’ review lists

  - This code should be written into the file `bookPairs.py`

  - This code should take the file at `/home/cs143/data/goodreads.user.books` as input

  - The code may find the functions `map()`, `flatMap()`, `reduceByKey()`, and `filter()` helpful

  - The output must consist of lines of the following format:

    - ```
      ((bookid_1, bookid_2), frequency)
      ```

    - The above means that `bookid_1` and `bookid_2` appear together in the `frequency` number of users' review lists

    - This output must be generated for every book pair that appears in more than 20 users' review lists

    - This output should be produced in the directory `./output`

    - The order of the book IDs does not matter, so there should be no duplicates

    - The order of the lines in the output does not matter

  - No third-party Python libraries may be used

  - The code should be executed by the following command:

    - ```bash
      spark-submit bookPairs.py
      ```

- Debug the program using subsets of the large dataset

  - The first `k` lines of a file can be taken using the following command:

    - ```bash
      head -k goodreads.user.books > goodreads.k
      ```

  - An output generated using `k = 1000` should be the following:

    - | Book Pairs  | Count |
      | ----------- | ----- |
      | (536, 1387) | 22    |


  - An output generated using `k = 3000` should be the following:

    - | Book Pairs    | Count |
      | ------------- | ----- |
      | (613, 939)    | 21    |
      | (1000, 66)    | 27    |
      | (1000, 1116)  | 32    |
      | (1000, 1117)  | 33    |
      | (1116, 66)    | 28    |
      | (1116, 1117)  | 28    |
      | (1117, 66)    | 27    |
      | (1386, 536)   | 53    |
      | (1386, 1387)  | 58    |
      | (1387, 536)   | 57    |
      | (1471, 1473)  | 21    |
      | (1525, 1526)  | 34    |
      | (1604, 1605)  | 22    |
      | (12710, 1525) | 22    |
      | (12710, 1526) | 22    |

## Tasks

- [ ] Coding
  - [x] Download and run the `spark` container
  - [x] Download the GoodReads dataset
  - [ ] Write a program that outputs frequently paired books
  - [ ] Debug using subsets of the large dataset
- [ ] Submission
  - [ ] Download and use `p5_package` to zip all required files
  - [ ]  Download and use `p5_test` to test the submission
  - [ ] Submit to Gradescope

## Related Links

- **[Spark Shell Quick Start](https://spark.apache.org/docs/latest/quick-start.html)**
- **[Spark Transformation Functions](http://spark.apache.org/docs/latest/rdd-programming-guide.html#transformations)**
# Data Manipulation With Unix Shell Commands

## Goal

**Learn how to use popular Unit text-based shell commands for basic data processing and analysis.**

## Requirements

- Download and run the new Unix container using:

  - ```bash
    docker run -it -v {your_shared_dir}:/home/cs143/shared --name unix junghoo/unix
    ```

- Learn Unix data manipulation commands

  - Download the adult dataset:

    - ```bash
      wget https://oak.cs.ucla.edu/classes/cs143/project6/adult.data
      ```

    - ~50K rows of data to be used as an example "large" dataset

    - Is a well-formatted CSV file

  - Get familiar with uses of the following commands: `cat`, `less`, `head`, `tail`, `shuf`, `wc`, `cut`, `grep`, `awk`, `sort`, `uniq`, `datamash`, `sed`, etc.

- Analyze Google's n-gram data

  - Download the n-gram dataset:

    - ```bash
      mkdir -p /home/cs143/data
      cd ~/data/
      wget http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-s.gz
      ```

    - Data in the file has the following format:

      - ```
        ngram TAB year TAB match_count TAB volume_count NEWLINE
        ```

  - Write queries using Unix commands

    - Each query should be written in a script named `q?.sh`, where `?` is the query number
    - Each query should take the file at `/home/cs143/data/googlebooks-eng-all-1gram-20120701-s.gz` as input
    - Find the 1-gram and the year in which the 1-gram’s match count in that year is at least 1,000 times as large as its volume count
      - For each of such `(1-gram, year)` pairs, print `1-gram TAB year`
      - The field separator doesn't need to be specified, as it is `TAB` by default

    - Find the earliest year in which there exists 1-gram that appeared in 10,000 or more volumes in that year
      - Consider that in the Unix command line, the order in which commands are used in the pipeline may have significant performance implications

    - For every 1-gram, sum up its match counts over all years
      - Return `1-gram TAB total-match-count` for each 1-gram whose total match count is 1,000,000 or more

    - For every year since 1900 (inclusive), find the most frequent 1-gram in the year (in terms of its match count)
      - Return `most-frequent-gram TAB year TAB gram’s-match-count` triple per each year

    - Remove all 1-grams that have a POS suffix, then for each remaining 1-gram, sum up its match count since 2000 (inclusive), and return the top-10 most frequent 1-grams
      - Assume that if any 1-gram contains the character `_`, it is suffixed with a POS tag
      - Return the `1-gram TAB total-match-count` pair


## Tasks

- [ ] Coding
  - [ ] Download and run the Unix container
  - [ ] Learn Unix commands
    - [ ] Download the adult dataset
    - [ ] Walk through all of the examples
  - [ ] Analyze Google's N-gram data
    - [ ] Download the n-gram dataset
    - [ ] Write queries to test on the dataset
    - [ ] Debug queries
- [ ] Submission
  - [ ] Download and use `p6_package` to zip all required files
  - [ ] Submit to Gradescope

## Related Links

- **[GNU `awk` User Guide](https://www.gnu.org/software/gawk/manual/gawk.html)**
- **[GNU `sed` User Guide](https://www.gnu.org/software/sed/manual/sed.html)**
- **[`grep` Command Examples](https://www.thegeekstuff.com/2009/03/15-practical-unix-grep-command-examples)**
- **[Shell Expansion](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_04.html)**
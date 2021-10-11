# Internet Movie Database

## Goal

**Use PHP and MySQL to create a web application that can be used to access the movie database implemented in Project 1.**

## Requirements

- `actor.php`
  - A page used to show an actor's name, gender, date of birth, date of death, and movies they starred in
  - The page must take in the actor's `id` as a parameter
  - Every movie title must serve as a hyperlink to that movie's page
- `movie.php`
  - A page used to show a movie's title, year of release, MPAA rating, production company, genre, cast, average user rating, and user reviews
  - The page must take in the movie's `id` as a parameter
  - Every actor name in the cast list must serve as a hyperlink to that actor's page
  - There must be a link to leave a review for the movie
- `search.php`
  - A page used to search for actors by name or movies by title
  - If no parameter is provided in the URL, the user must be able to use both search bars
  - If an `actor` parameter is provided, the page must list all actors whose first or last names contain the keywords the user entered
  - If an `movie` parameter is provided, the page must list all movies whose titles contain the keywords the user entered
  - All names/titles should act as hyperlinks to their respective pages
  - All search queries must be case insensitive
  - Spaces within the search query should be treated as `AND` operations
- `review.php`
  - A page used to leave a view for a given movie
  - The page must take in the movie's `id` as a parameter
  - If the request contains `mid`, `name`, `rating`, and `comment` parameters, the page should post a review to the `Review` table, using the current time as the `time` field
  - Upon the successful posting of a review, confirmation text should be present indicating the request was successful
- These pages must be contained within an intuitive UI
- The assumption can be made that the user is not malicious, meaning input sanitization is unnecessary
- The `class_db` database should be used for MySQL queries, using the `cs143` username and an empty password

## Deliverables

- Coding
  - [x] `actor.php`
    - [x] Page takes in the `id` of an actor as the parameter and displays the corresponding information properly
    - [x] Page shows relevant data from the `Actor` table
    - [x] Page contains a list of all movies the actor has starred in
    - [x] Movie titles hyperlink to the correct movie page
  - [ ] `movie.php`
    - [x] Page takes in the `id` of an movie as the parameter and displays the corresponding information properly
    - [x] Page shows relevant data from the `Movie` table
    - [ ] Page shows the correct genre of the movie, according to the `MovieGenre` table
    - [x] Page contains a cast list
    - [x] Actor names hyperlink to the correct actor page
    - [ ] Page contains the average user rating of the movie
    - [ ] Page contains a list of user reviews for the movie
    - [x] Page contains a link to the movie's review page
  - [x] `search.php`
    - [x] Search bars are displayed when no parameter is given
    - [x] The page takes in an `actor` parameter and displays a list of actors
    - [x] The page takes in a `movie` parameter and displays a list of movies
    - [x] The page only displays actors/movies whose names/titles contain the user-inputted strings
    - [x] The page supports multi-word search
    - [x] The page supports case-insensitive search
  - [ ] `review.php`
    - [ ] The page shows all fields necessary to populate the `Review` table
    - [ ] The user is able to submit a review for posting
    - [ ] A success message appears upon a successful post
- Submission
  - [ ] Install the `p2_test` testing script using `wget http://oak.cs.ucla.edu/classes/cs143/project2/p2_test`
  - [ ] Test the web application using `./p2_test`
  - [ ] Install the `p2_package` packaging script using `wget http://oak.cs.ucla.edu/classes/cs143/project2/p2_package`
  - [ ] Package all necessary files using `./p2_package`
  - [ ] Submit the `project2.zip` to Gradescope

## Relevant Links

- **[Demo Site (User: project, Pass: demo)](http://oak.cs.ucla.edu/classes/cs143/demos/project2/)**
- **[MySQL + PHP Tutorial](https://oak.cs.ucla.edu/refs/php/php_mysql_cs143.html)**
- **[MySQL Timestamp Documentation](https://dev.mysql.com/doc/refman/8.0/en/date-and-time-functions.html)**
- **[MySQLi Documentation](https://www.php.net/manual/en/book.mysqli.php)**
- **[PHP Tutorial](https://www.w3schools.com/php/php_intro.asp)**
- **[PHP User Input Tutorial](https://oak.cs.ucla.edu/refs/php/php_input.html)**
- **[Project 1 Spec](https://oak.cs.ucla.edu/classes/cs143/project1/)**


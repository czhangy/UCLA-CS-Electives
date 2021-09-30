# Setting Up Container and MySQL Database

## Goal

**Set up the project development environment and learn how to create and populate tables in MySQL.**

## Requirements

- Write a script `create.sql` to create the following tables:

  - `Movie(id, title, year, rating, company)`

    - | Name      | Type           | Description        |
      | --------- | -------------- | ------------------ |
      | <u>id</u> | `INT`          | Movie ID           |
      | title     | `VARCHAR(100)` | Movie title        |
      | year      | `INT`          | Release year       |
      | rating    | `VARCHAR(10)`  | MPAA rating        |
      | company   | `VARCHAR(50)`  | Production company |

  - `Actor(id, last, first, sex, dob, dod)`

    - | Name      | Type          | Description      |
      | --------- | ------------- | ---------------- |
      | <u>id</u> | `INT`         | Actor ID         |
      | last      | `VARCHAR(20)` | Last name        |
      | first     | `VARCHAR(20)` | First name       |
      | sex       | `VARCHAR(6)`  | Sex of the actor |
      | dob       | `DATE`        | Date of birth    |
      | dod       | `DATE`        | Date of death    |

  - `MovieGenre(mid, genre)`

    - | Name  | Type          | Description |
      | ----- | ------------- | ----------- |
      | mid   | `INT`         | Movie ID    |
      | genre | `VARCHAR(20)` | Movie genre |

  - `MovieActor(mid, aid, role)`

    - | Name | Type          | Description         |
      | ---- | ------------- | ------------------- |
      | mid  | `INT`         | Movie ID            |
      | aid  | `INT`         | Actor ID            |
      | role | `VARCHAR(50)` | Actor role in movie |

  - `Review(name, time, mid, rating, comment)`

    - | Name    | Type          | Description      |
      | ------- | ------------- | ---------------- |
      | name    | `VARCHAR(20)` | Reviewer name    |
      | time    | `DATETIME`    | Review time      |
      | mid     | `INT`         | Movie ID         |
      | rating  | `INT`         | Review rating    |
      | comment | `TEXT`        | Reviewer comment |

- Write a script `load.sql` to load the following files into their respective tables:
  - `Movie`
    - `movie.del`
  - `Actor`
    - `actor1.del`
    - `actor2.del`
    - `actor3.del`
  - `MovieGenre`
    - `moviegenre.del`
  - `MovieActor`
    - `movieactor1.del`
    - `movieactor2.del`
- Run both of the scripts using `mysql class_db < <script_name>` to create and populate the tables within `class_db`

## Deliverables

- Coding
  - [x] Write `create.sql`
  - [x] Write `load.sql`
  - [x] Run the scripts
- Submission
  - [x] Install the `p1_package` packaging script using `wget http://oak.cs.ucla.edu/classes/cs143/project1/p1_package`
  - [x] Package the `create.sql`, `load.sql`, and `README.md` files using `./p1_package`
  - [ ] Submit `project1.zip` to Gradescope


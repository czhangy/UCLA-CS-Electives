-- Drop related tables if they exist
DROP TABLE IF EXISTS Person;
DROP TABLE IF EXISTS Organization;
DROP TABLE IF EXISTS Place;
DROP TABLE IF EXISTS NobelPrize;
DROP TABLE IF EXISTS Born;
DROP TABLE IF EXISTS Founded;
DROP TABLE IF EXISTS Awarded;
DROP TABLE IF EXISTS Affiliated;

-- Create tables
CREATE TABLE Person (id INT PRIMARY KEY, givenName VARCHAR(100), familyName VARCHAR(100), gender VARCHAR(10));
CREATE TABLE Organization (id INT PRIMARY KEY, orgName VARCHAR(100));
CREATE TABLE Place (id INT PRIMARY KEY, city VARCHAR(100), country VARCHAR(100));
CREATE TABLE NobelPrize (awardYear INT, category VARCHAR(100), sortOrder INT, PRIMARY KEY(awardYear, category, sortOrder));
CREATE TABLE Born (person_id INT PRIMARY KEY, place_id INT, date VARCHAR(25));
CREATE TABLE Founded (org_id INT PRIMARY KEY, place_id INT, date VARCHAR(25));
CREATE TABLE Awarded (laureate_id INT, awardYear INT, category VARCHAR(100), sortOrder INT,
                     PRIMARY KEY(laureate_id, awardYear, category, sortOrder));
CREATE TABLE Affiliated (awardYear INT, category VARCHAR(100), sortOrder INT, place_id INT, name VARCHAR(100),
                        PRIMARY KEY(awardYear, category, sortOrder, place_id, name));

-- Load data into tables
LOAD DATA LOCAL INFILE 'person.del' INTO TABLE Person;
LOAD DATA LOCAL INFILE 'organization.del' INTO TABLE Organization;
LOAD DATA LOCAL INFILE 'place.del' INTO TABLE Place;
LOAD DATA LOCAL INFILE 'nobelprize.del' INTO TABLE NobelPrize;
LOAD DATA LOCAL INFILE 'born.del' INTO TABLE Born;
LOAD DATA LOCAL INFILE 'founded.del' INTO TABLE Founded;
LOAD DATA LOCAL INFILE 'awarded.del' INTO TABLE Awarded;
LOAD DATA LOCAL INFILE 'affiliated.del' INTO TABLE Affiliated;

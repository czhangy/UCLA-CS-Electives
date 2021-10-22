WITH YearlyCredits AS (SELECT stud_id, year, SUM(credits) AS credits
                       FROM Student AS S, Class AS C, Takes
                       WHERE S.id = stud_id AND C.id = class_id
                       GROUP BY stud_id, year),
     MaxCredits AS (SELECT stud_id, MAX(credits) OVER(PARTITION BY stud_id) AS credits
                    FROM YearlyCredits)
SELECT *
FROM MaxCredits NATURAL JOIN YearlyCredits;

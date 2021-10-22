WITH X AS (SELECT stud_id, COUNT(*) AS cnt 
           FROM Takes
           WHERE year = 2009
           GROUP BY stud_id),
     Y AS (SELECT stud_id, COUNT(*) AS cnt
           FROM Takes
           WHERE year = 2010
           GROUP BY stud_id)
SELECT X.stud_id
FROM X LEFT OUTER JOIN Y
ON X.stud_id = Y.stud_id
WHERE X.cnt > Y.cnt OR Y.cnt IS NULL;

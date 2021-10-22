WITH A AS (SELECT stud_id, name
           FROM Instructor, Advisor
           WHERE id = inst_id)
SELECT S.name, A.name
FROM Student S LEFT OUTER JOIN A
ON id = stud_id;

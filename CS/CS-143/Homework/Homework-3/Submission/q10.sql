WITH SchoolName AS (SELECT CASE dept WHEN 'Comp. Sci.' THEN 'Engineering'
                                     WHEN 'Elec. Eng.' THEN 'Engineering'
                                     ELSE 'L&S'
                           END AS school, dept
                    FROM Department),
     SchoolStuds AS (SELECT school, COUNT(*) AS num_studs
                     FROM SchoolName AS SN, Student as S
                     WHERE SN.dept = S.dept
                     GROUP BY school),
     SchoolInsts AS (SELECT school, COUNT(*) AS num_insts
                    FROM SchoolName AS S, Instructor AS I
                    WHERE S.dept = I.dept
                    GROUP BY school)
SELECT *
FROM SchoolStuds NATURAL JOIN SchoolInsts;

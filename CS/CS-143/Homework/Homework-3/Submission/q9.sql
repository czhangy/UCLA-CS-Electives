WITH SchoolName AS (SELECT CASE dept WHEN 'Comp. Sci.' THEN 'Engineering'
                                     WHEN 'Elec. Eng.' THEN 'Engineering'
                                     ELSE 'L&S'
                           END
                           school, dept
                    FROM Department)
SELECT dept, school
FROM SchoolName
GROUP BY dept;

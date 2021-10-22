WITH Credits AS (
                    SELECT stud_id, SUM(credits) AS credits
                    FROM Class, Takes
                    WHERE class_id = id
                    GROUP BY stud_id
                )
SELECT stud_id
FROM Credits
ORDER BY credits DESC
LIMIT 4;

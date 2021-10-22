WITH Credits AS (SELECT stud_id, SUM(credits) AS credits
                 FROM Takes, Class
                 WHERE Takes.class_id = Class.id
                 GROUP BY stud_id)
SELECT id, tot_cred - COALESCE(credits, 0) AS credit_discrepency
FROM Student, Credits
WHERE id = stud_id;

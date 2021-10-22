WITH RECURSIVE Req(class_id, prereq_id) AS (
                                            (SELECT *
                                             FROM Prereq)
                                            UNION
                                            (SELECT R.class_id, P.prereq_id
                                             FROM Req R, Prereq P
                                             WHERE R.prereq_id = P.class_id)
                                           )
SELECT prereq_id
FROM Req
WHERE class_id = 'BIO-399';

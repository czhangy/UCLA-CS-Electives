SELECT D.dept, MAX(credits) AS maximum_course_credit
FROM Department D, Class C
GROUP BY D.dept;
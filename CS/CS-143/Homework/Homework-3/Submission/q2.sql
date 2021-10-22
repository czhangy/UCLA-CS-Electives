SELECT DISTINCT D.dept, MAX(credits) OVER(PARTITION BY D.dept) AS maximum_course_credit
FROM Department D, Class C
WHERE D.dept = C.dept;

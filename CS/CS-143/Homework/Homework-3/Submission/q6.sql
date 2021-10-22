SELECT dept, AVG(credits) AS dept_average_course_credit
FROM Class
GROUP BY dept;

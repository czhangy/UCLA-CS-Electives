SELECT DISTINCT dept,
                AVG(credits) OVER(PARTITION BY dept) AS dept_avg_course_credit,
                AVG(credits) OVER() AS overall_avg_course_credit
FROM Class;

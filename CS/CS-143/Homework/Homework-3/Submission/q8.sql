SELECT DISTINCT dept, (AVG(salary) OVER() - AVG(salary) OVER(PARTITION BY dept)) AS diff_avg_salary
FROM Instructor;

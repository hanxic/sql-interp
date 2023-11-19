SELECT department, AVG(salary) as average_salary
FROM employees
GROUP BY department;
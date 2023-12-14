SELECT order_id, item, amount, first_name, last_name, age, country
FROM Orders AS o
  JOIN Customers AS c ON o.customer_id = c.customer_id;
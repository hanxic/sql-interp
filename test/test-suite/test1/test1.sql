SELECT o.order_id, o.item, o.amount, c.first_name, c.last_name, c.age, c.country
FROM Orders AS o
  JOIN Customers AS c ON o.customer_id = c.customer_id;
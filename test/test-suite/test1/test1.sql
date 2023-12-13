SELECT o.order_id, o.item, o.amount, c.first_name, c.last_name, c.age, c.country
FROM Orders o
  JOIN Customers c ON o.customer_id = c.customer_id
  JOIN Shippings s ON o.customer_id = s.customer
WHERE s.status = 'Delivered';
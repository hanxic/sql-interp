SELECT order_id, amount + 5, customer_id
FROM Orders
LIMIT
2
OFFSET 2;
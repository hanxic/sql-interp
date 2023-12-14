SELECT amount, COUNT(order_id)
FROM Orders
GROUP BY amount;
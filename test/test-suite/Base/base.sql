CREATE TABLE Customers
(
  customer_id INTEGER PRIMARY KEY,
  first_name VARCHAR(255),
  last_name VARCHAR(255),
  age INTEGER,
  country VARCHAR(255)
);

CREATE TABLE Orders
(
  order_id INTEGER PRIMARY KEY,
  item VARCHAR(255),
  amount INTEGER,
  customer_id INTEGER
);

CREATE TABLE Shippings
(
  shipping_id INTEGER PRIMARY KEY,
  status VARCHAR(255),
  customer INTEGER
);
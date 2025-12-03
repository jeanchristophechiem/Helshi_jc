# Данные
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)

# Формулы
n <- length(x)
a <- (n * sum(x*y) - sum(x)*sum(y)) /
  (n * sum(x^2) - (sum(x))^2)

b <- mean(y) - a * mean(x)

a; b  # коэффициенты

y_pred <- a * x + b

plot(x, y, pch = 19)
abline(a = b, b = a, col = "red", lwd = 2)

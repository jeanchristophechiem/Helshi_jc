# Create a normal distribution with mean = 10 and sd = 5
y <- rnorm(n = 1000, mean = 10, sd = 5)

# Create histogram with density overlay
hist(y, freq = FALSE, breaks = 30, 
     main = "Histogram with Density Overlay", 
     xlab = "Values", ylab = "Density",
     col = "lightblue", border = "white")

# Add density curve
lines(density(y), col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Histogram", "Density"), 
       fill = c("lightblue", NA), border = c("white", NA),
       lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2))


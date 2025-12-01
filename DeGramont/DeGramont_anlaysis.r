# Set working directory to the DeGramont folder
setwd("/Users/jean-christophechiem/Documents/Helshi_jc/DeGramont")

# Reset graphics parameters
graphics.off()
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Read data.csv from the current folder
data <- read.csv("data.csv")

# Display basic information about the dataset
cat("Dataset dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Column names:\n")
print(names(data))

# Display first few rows
cat("\nFirst 6 rows:\n")
print(head(data))

# Display summary statistics
cat("\nSummary statistics:\n")
print(summary(data))

# Install and load survival analysis packages
# Set repository to use binary packages for macOS
options(repos = c(CRAN = "https://cran.rstudio.com/"))

if (!require(survival, quietly = TRUE)) {
  install.packages("survival", type = "binary")
  library(survival)
}

# For now, let's use just the survival package for basic plotting
# We can add survminer later if needed

# Create survival object
surv_obj <- Surv(time = data$surv, event = data$survind)

# Fit Kaplan-Meier survival curve stratified by treatment
km_fit <- survfit(surv_obj ~ treat, data = data)

# Print survival summary by treatment
cat("\nSurvival Summary by Treatment:\n")
print(summary(km_fit))

# Log-rank test to compare survival curves
logrank_test <- survdiff(surv_obj ~ treat, data = data)
cat("\nLog-rank test comparing treatments:\n")
print(logrank_test)

# Extract and display key statistics from log-rank test
chisq_stat <- logrank_test$chisq
p_value <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
df <- length(logrank_test$n) - 1

cat("\n=== LOG-RANK TEST RESULTS ===\n")
cat("Chi-square statistic:", round(chisq_stat, 4), "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", round(p_value, 6), "\n")

if (p_value < 0.001) {
  cat("Significance: Highly significant (p < 0.001)\n")
} else if (p_value < 0.01) {
  cat("Significance: Very significant (p < 0.01)\n")
} else if (p_value < 0.05) {
  cat("Significance: Significant (p < 0.05)\n")
} else {
  cat("Significance: Not significant (p >= 0.05)\n")
}

cat("Interpretation: ")
if (p_value < 0.05) {
  cat("There IS a statistically significant difference between treatment groups.\n")
} else {
  cat("There is NO statistically significant difference between treatment groups.\n")
}
cat("==============================\n")

# Plot survival curves by treatment
plot(km_fit, 
     main = "Kaplan-Meier Survival Curves by Treatment",
     xlab = "Time",
     ylab = "Survival Probability",
     col = c("blue", "red"),
     lwd = 2,
     conf.int = FALSE)

# Add grid for better readability
grid()

# Get treatment group names
treatment_names <- names(km_fit$strata)
treatment_labels <- gsub("treat=", "", treatment_names)

# Add legend
legend("topright", 
       legend = treatment_labels,
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

# Add log-rank test p-value to the plot
p_value <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
if (p_value < 0.001) {
  p_text <- "Log-rank p < 0.001"
} else {
  p_text <- paste("Log-rank p =", round(p_value, 4))
}

# Add p-value text to bottom left of plot
text(x = 0, y = 0.1, labels = p_text, 
     pos = 4, cex = 1.2, font = 2, 
     col = ifelse(p_value < 0.05, "red", "black"))

# Add median survival lines if available
medians <- summary(km_fit)$table[, "median"]
if (any(!is.na(medians))) {
  abline(h = 0.5, col = "gray", lty = 2)
  text(0, 0.5, "50% survival", pos = 3, col = "gray")
}

# Save the plot as PNG file
png("survival_curves_by_treatment.png", width = 800, height = 600, res = 300)

# Reset par settings for the PNG device
par(mar = c(5, 4, 4, 2))

# Simple survival plot
plot(km_fit, 
     main = "Kaplan-Meier Survival Curves by Treatment",
     xlab = "Time",
     ylab = "Survival Probability",
     col = c("blue", "red"),
     lwd = 2,
     conf.int = FALSE)

grid()

legend("topright", 
       legend = treatment_labels,
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")

# Add p-value text
text(x = 0, y = 0.1, labels = p_text, 
     pos = 4, cex = 1.2, font = 2, 
     col = ifelse(p_value < 0.05, "red", "black"))

# Add median survival lines if available
if (any(!is.na(medians))) {
  abline(h = 0.5, col = "gray", lty = 2)
  text(0, 0.5, "50% survival", pos = 3, col = "gray")
}

dev.off()
cat("\nPlot saved as 'survival_curves_by_treatment.png' in current folder\n")

# Print numbers at risk in console instead
cat("\nNumbers at Risk:\n")
cat("Time points: 0, 25%, 50%, 75%, 100% of follow-up\n")
time_points <- c(0, quantile(km_fit$time, c(0.25, 0.5, 0.75)), max(km_fit$time))
time_points <- round(time_points)

group_names <- gsub("treat=", "", names(km_fit$strata))

for (i in 1:length(group_names)) {
  if (i == 1) {
    group_indices <- 1:km_fit$strata[1]
  } else {
    group_indices <- (km_fit$strata[1] + 1):sum(km_fit$strata[1:2])
  }
  
  group_times <- km_fit$time[group_indices]
  group_n_risk <- km_fit$n.risk[group_indices]
  
  risk_numbers <- sapply(time_points, function(t) {
    if (t == 0) return(max(group_n_risk))
    valid_indices <- which(group_times <= t)
    if (length(valid_indices) > 0) {
      return(group_n_risk[max(valid_indices)])
    } else {
      return(max(group_n_risk))
    }
  })
  
  cat(group_names[i], ":", paste(risk_numbers, collapse = "  "), "\n")
}

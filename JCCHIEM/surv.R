library(survival)
library(survminer)

# Read the CSV file and store it in the variable 'data'
data <- read.csv("data.csv")

# Check the first few rows to confirm
head(data)


# Assuming data$surv = survival time, data$survind = 1 for event, 0 for censored
surv_obj <- Surv(time = data$surv, event = data$survind)

fit <- survfit(surv_obj ~ treat, data = data) # ~1 means no grouping, overall survival curve

summary(fit)   # Shows survival probabilities at each time point

plot(fit, xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
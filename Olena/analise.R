data <- read.csv("https://raw.githubusercontent.com/jeanchristophechiem/Helshi_jc/refs/heads/master/Olena/data.csv")
head(data)

############################################################
# 1. Подготовка: установка и подключение пакетов
############################################################

# Если пакеты ещё не установлены – раскомментируй и запусти эти строки:
# install.packages("survival")
# install.packages("survminer")

library(survival)
library(survminer)

############################################################
str(data)

# Ожидаем такие колонки:
# ID, treat, surv, survind, pfs, pfsind, trial

############################################################
# 3. Объект выживаемости (Kaplan–Meier)
############################################################

# surv  = время до события (например, смерть)
# survind = 1 если событие произошло, 0 если цензурировано

surv_obj <- Surv(time = data$surv, event = data$survind)

############################################################
# 4. Модель Kaplan–Meier по группам лечения
############################################################

# treat = 0 / 1 (например, контроль / лечение)
fit <- survfit(surv_obj ~ treat, data = data)

# Текстовый вывод (табличка с вероятностями)
summary(fit)

############################################################
# 5. Красивый график Kaplan–Meier
############################################################

plot_km <- ggsurvplot(
  fit,
  data = data,
  conf.int = TRUE,              # доверительные интервалы
  pval = TRUE,                  # p-value log-rank
  risk.table = TRUE,            # таблица числа пациентов под риском
  legend.labs = c("Control", "Treatment"),  # подписи групп
  legend.title = "Treatment",
  xlab = "Time",
  ylab = "Survival probability",
  title = "Kaplan–Meier Survival Curves by Treatment",
  ggtheme = theme_minimal()
)

print(plot_km)

############################################################
# 6. Log-rank тест (сравнение кривых выживаемости)
############################################################

logrank_res <- survdiff(surv_obj ~ treat, data = data)
logrank_res

# p-value:
p_value_logrank <- 1 - pchisq(logrank_res$chisq, length(logrank_res$n) - 1)
p_value_logrank



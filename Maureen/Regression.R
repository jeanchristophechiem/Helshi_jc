library(ggplot2)
# Génération de la base de données
set.seed(123)  # pour la reproductibilité
n <- 100       # nombre de points
x <- runif(n, min = 0, max = 10)              # valeurs de x uniformes entre 0 et 10
epsilon <- rnorm(n, mean = 0, sd = 2)         # bruit gaussien
y <- 5 * x + epsilon                          # relation linéaire avec bruit

# Création explicite d'un data frame
data <- data.frame(x = x, y = y)

# Vérification du type
str(data)   # doit afficher 'data.frame'

# Ajustement du modèle de régression linéaire
model <- lm(y ~ x, data = data)

# Résumé du modèle
summary(model)

# Graphique avec ggplot2
library(ggplot2)

ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Régression linéaire : y = 5x + epsilon",
       x = "x",
       y = "y") +
  theme_minimal()

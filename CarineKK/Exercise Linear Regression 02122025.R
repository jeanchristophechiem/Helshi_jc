# Exemple de données
set.seed(123)
X <- 1:50
Y <- 2*X + 5 + rnorm(50, sd = 10)   # vraie relation + bruit

# Ajustement du modèle linéaire
modele <- lm(Y ~ X)

# Affichage du résumé du modèle
summary(modele)

# Génération du graphique
plot(X, Y,
     main = "Régression linéaire : Y = a·X + b",
     xlab = "X",
     ylab = "Y",
     pch = 19,
     col = "blue")

# Ajout de la droite de régression
abline(modele, col = "red", lwd = 2)

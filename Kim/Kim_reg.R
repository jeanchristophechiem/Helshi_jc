## Génération d'une base de données

set.seed(123)

# Taille de l'échantillon
n <- 100

# Générer x
x <- runif(n, min = 0, max = 10)   

# Générer du bruit
bruit <- rnorm(n, mean = 0, sd = 2) 

# Générer y
y <- 5 * x + bruit

# Mettre dans un data.frame
donnees <- data.frame(x = x, y = y)

head(donnees)

#plot lm des données
plot(x, y, pch = 19, col = "blue",
     main = "Données simulées : y = 5x + bruit",
     xlab = "x", ylab = "y") + 
  abline(lm(y ~ x), col = "red", lwd = 2)

#modèle m(y ~ x)
lm <- lm(y ~ x)
summary(lm)

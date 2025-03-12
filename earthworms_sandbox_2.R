#earthworms sandbox 2

# 1. Importer le fichier CSV
# Remplace "chemin/vers/ton/fichier.csv" par le chemin vers ton fichier CSV
data <- read.csv2("C:\\R_projects\\dataset_sandbox.csv")

str(data)
data$results <- as.numeric(data$results)
head(data$results)

# 2. Visualiser les premières lignes du dataset
print("Premières lignes du dataset :")
head(data)

# 3. Visualiser la structure du dataset
str(data)

# 4. Afficher un résumé statistique des données
summary(data)

# 5. Visualisation graphique des données
# Si tes colonnes sont numériques, par exemple, un graphique en nuage de points :
plot(data[[1]], data[[2]], 
     main = "Visualisation des données",
     xlab = names(data)[1], 
     ylab = names(data)[2], 
     pch = 19, col = "blue")

# Si tu préfères un graphique en barres, ajusté à tes données :
barplot(data[[2]], names.arg = data[[1]], 
        main = "Barplot des données", 
        xlab = names(data)[1], 
        ylab = names(data)[2], 
        col = "lightblue")

names(data)

#en boxplot
boxplot(results ~ modes, data = data, 
        main = "Boxplot résultats par modes",
        xlab = "Catégories (Colonne1)", 
        ylab = "Valeurs (Colonne2)",
        col = "lightblue", 
        border = "darkred")
Sys.setlocale("LC_ALL", "en_US.UTF-8")

install.packages("car")  # Pour le test de Levene (homogénéité)
install.packages("dplyr")  # Pour la manipulation des données
install.packages("FSA")  # Pour le test post-hoc de Dunn

# Charger les packages nécessaires
library(car)  # Pour le test de Levene
library(dplyr) # Pour la manipulation des données

# Vérifier la normalité par groupe
shapiro_results <- data %>%
  group_by(modes) %>%
  summarise(p_value = shapiro.test(results)$p.value)

data$modes <- as.factor(data$modes)
levels(data$modes)

# Vérifier l'homogénéité des variances
levene_p_value <- leveneTest(results ~ modes, data = data)$"Pr(>F)"[1]

# Déterminer si les conditions sont respectées
normal <- all(shapiro_results$p_value > 0.05)  # TRUE si tous les groupes sont normaux
homogene <- levene_p_value > 0.05  # TRUE si les variances sont homogènes

# Choisir le test statistique en fonction des résultats
if (normal & homogene) {
  cat("✅ Les données sont normales et homogènes → ANOVA\n")
  anova_result <- aov(results ~ modes, data = data)
  print(summary(anova_result))
} else {
  cat("⚠️ Les données ne sont pas normales ou homogènes → Kruskal-Wallis\n")
  kruskal_result <- kruskal.test(results ~ modes, data = data)
  print(kruskal_result)
}

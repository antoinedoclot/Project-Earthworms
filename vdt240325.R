#Chargement des données
data_adultes <- read.csv2("Données_adultes_220325.csv") #Données des adultes
data_juvéniles <- read.csv2("Données_juvéniles_220325.csv") #Données des juvéniles

str(data_juvéniles)
str(data_adultes) #les données poids des adultes ne sont pas numériques

#Convertir les données "Poids" en numérique
data_adultes <- data_adultes %>%
  mutate(Poids = as.numeric(as.character(Poids)))

#Controle
str(data_adultes)

#Fusionner les deux feuilles
library(dplyr)
data3 <- bind_rows(data_adultes, data_juvéniles)
data3 <- data3 %>%
  mutate(Pratiques = as.factor(Pratiques))

#Biomasse par parcelle
biomasse_par_parcelle <- data3 %>%
  group_by(Site, Pratiques) %>%  # Regroupement par parcelle et pratique agricole
  summarise(biomasse_totale = sum(Poids, na.rm = TRUE))  # Somme des poids

biomasse_moyenne_par_pratique <- biomasse_par_parcelle %>%
  group_by(Pratiques) %>%
  summarise(biomasse_moyenne = mean(biomasse_totale, na.rm = TRUE))

library(ggplot2)

ggplot(biomasse_par_parcelle, aes(x = Pratiques, y = biomasse_totale)) +
  geom_boxplot() +
  labs(title = "Biomasse totale des vers de terre par pratique agricole",
       x = "Pratique agricole",
       y = "Biomasse totale (g)") +
  theme_minimal()

#Espèces par parcelle
espèces_par_parcelle <- data3 %>%
  group_by(Site,Pratiques) %>%
  summarise(espèces_par_parcelle = n_distinct(Binomial, na.rm = TRUE))

espèces_moyenne_par_pratique <- espèces_par_parcelle %>%
  group_by(Pratiques) %>%
  summarise(espèces_moyenne = mean(espèces_par_parcelle, na.rm = TRUE))

ggplot(espèces_par_parcelle, aes(x = Pratiques, y = espèces_par_parcelle, fill=Pratiques)) +
  geom_boxplot() +
  labs(title = "Nombre d'espèces différentes de vers de terre par pratique agricole",
       x = "Pratique agricole",
       y = "Nombre d'espèces") +
  theme_minimal()

# Palette de couleurs personnalisée
palette_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

# Graphique Biomasse
ggplot(biomasse_par_parcelle, aes(x = Pratiques, y = biomasse_totale, fill = Pratiques)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Suppression des outliers pour éviter le fouillis
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Ajout des points pour mieux voir la distribution
  scale_fill_manual(values = palette_colors) +
  labs(title = "Biomasse totale des vers de terre par pratique agricole",
       x = "Pratique agricole",
       y = "Biomasse totale (g)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Graphique Nombre d'espèces
ggplot(espèces_par_parcelle, aes(x = Pratiques, y = espèces_par_parcelle, fill = Pratiques)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  scale_fill_manual(values = palette_colors) +
  labs(title = "Nombre d'espèces différentes de vers de terre par pratique agricole",
       x = "Pratique agricole",
       y = "Nombre d'espèces") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#Anova/Kruskal-Wallis pour la biomasse

myAOV_biomasse = aov(biomasse_totale ~ Pratiques, data=biomasse_par_parcelle)
summary(myAOV_biomasse)

#Vérification de la normalité
hist(residuals(myAOV_biomasse))
qqnorm(residuals(myAOV_biomasse))
qqline(residuals(myAOV_biomasse))
shapiro.test(residuals(myAOV_biomasse))
#vérification de l'homoscédasticité
bartlett.test(biomasse_totale ~ Pratiques, data=biomasse_par_parcelle)

kruskal.test(biomasse_totale ~ Pratiques, data = biomasse_par_parcelle)
TukeyHSD(myAOV_biomasse)

#Anova/Kruskal Wallis pour espèces
myAOV_especes = aov(espèces_par_parcelle ~ Pratiques, data=espèces_par_parcelle)
summary(myAOV_especes)

#Vérification de la normalité
hist(residuals(myAOV_especes))
qqnorm(residuals(myAOV_especes))
qqline(residuals(myAOV_especes))
shapiro.test(residuals(myAOV_especes))
#vérification de l'homoscédasticité
bartlett.test(espèces_par_parcelle ~ Pratiques, data=espèces_par_parcelle)

kruskal.test(espèces_par_parcelle ~ Pratiques, data=espèces_par_parcelle)
TukeyHSD(myAOV_especes)

#Indice de Shannon
install.packages("vegan")
library(vegan)

# Charger les packages nécessaires
library(dplyr)

# Calculer l'indice de Shannon par parcelle
shannon_index <- data3 %>%
  group_by(Site, Pratiques) %>%  # Grouper par parcelle et pratiques
  summarise(H = -sum((table(Binomial)/sum(table(Binomial))) * log(table(Binomial)/sum(table(Binomial)))), .groups = "drop")

# Afficher les résultats
print(shannon_index)

ggplot(shannon_index, aes(x = Pratiques, y = H, fill=Pratiques)) +
  geom_boxplot() +
  labs(title = "Nombre d'espèces différentes de vers de terre par pratique agricole",
       x = "Pratique agricole",
       y = "Nombre d'espèces") +
  theme_minimal()

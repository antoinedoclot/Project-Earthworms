project earthworms
#Chargement et visualisation des données
donnees_csv <- read.csv("DonnéesVDT_teststats.csv", header = TRUE, sep = ";")
str(donnees_csv)  # Vérifie la structure des colonnes

donnees_csv$Traitement <- as.factor(donnees_csv$Traitement)
#boxplot de la Biomasse en fonction des traitements
boxplot(Biomasse ~ Traitement, data=donnees_csv)
#bxoplot des Espèces en fonction des traitements
boxplot(Espèces~ Traitement, data=donnees_csv)

#Code Antoine Doclot-Florence Pivetta-Lilia Zarrella

#Chargement des packages nécessaires à l'ensemble du code

#install.packages("ggplot2")
#install.packages("vegan")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("pheatmap")
#install.packages("reshape2")
#install.packages("tidyverse")
#install.packages("betapart")
#install.packages("ggpubr")
#install.packages("indicspecies")
#install.packages("patchwork")
#devtools::install_github("cmartin/ggConvexHull")
#devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#install.packages("caret", dependencies = TRUE)

library(ggplot2)
library(vegan)
library(dplyr)
library(readxl)
library(pheatmap)
library(reshape2)
library(tidyverse)
library(devtools)
library(patchwork)
library(betapart)
library(ggpubr)
library(ggConvexHull)
library(ggrepel)
library(pairwiseAdonis)
library(indicspecies)
library(corrplot)
library(caret)

#_______________________________________________________________________________________

#-------------------1.Chargement des données------------------

#_______________________________________________________________________________________
Liste=read_excel("C:/Users/antoi/OneDrive - Université Libre de Bruxelles/DB_F4F_2025_Antoine110425.xlsx",sheet="Liste_Sites")
Coord = read_excel("C:/Users/antoi/OneDrive - Université Libre de Bruxelles/DB_F4F_2025_Antoine110425.xlsx", sheet = "Coords")
Dates = read_excel("C:/Users/antoi/OneDrive - Université Libre de Bruxelles/DB_F4F_2025_Antoine110425.xlsx", sheet = "Dates")
Sites = Liste[, c(3:6)]
VDT=read_excel("C:/Users/antoi/OneDrive - Université Libre de Bruxelles/DB_F4F_2025_Antoine110425.xlsx",sheet="VDT")
Weight_VDT=read_excel("C:/Users/antoi/OneDrive - Université Libre de Bruxelles/DB_F4F_2025_Antoine110425.xlsx",sheet="Weight_VDT")

#Elimination des colonnes vides ou inutiles
VDT_adultes=VDT[,c(1:3,5:6,16:18)]
VDT_juvéniles=Weight_VDT[,c(1:3,5:10)]

Sites = Sites %>% #je remplace Sites par nouvelle version de Sites
  left_join(Coord[, c(1, 5, 6)], by = "Site") %>% #on joint tout ce qui ce correspond à sites à gauche
  filter(Site %in% union(VDT$Site, Weight_VDT$Site))

#Vérification que les poids sont bien en numérique
str(VDT_juvéniles)
str(VDT_adultes)

#Changement de noms de certaines colonnes
VDT_adultes <- VDT_adultes %>%
  rename(Poids = `Poids (g)`)

VDT_juvéniles <- VDT_juvéniles %>%
  rename(Poids = `Poids SubAdults/Juvenils (g)`)

#Conversion du poids en numérique pour les données adultes
VDT_adultes <- VDT_adultes %>%
  mutate(Poids = as.numeric(as.character(Poids)))

#Contrôle
str(VDT_adultes)

#Ajout d'une colonne abondance pour les données adultes
VDT_adultes <- VDT_adultes %>%
  mutate(Abondance = 1)

#Regroupement des données adultes et juvéniles
data3 <- bind_rows(VDT_adultes, VDT_juvéniles)     
#_______________________________________________________________________________________

#--------------2.Biomasse, abondance et nombres d'espèces par parcelle--------------

#_______________________________________________________________________________________
#--------------2.1 Abondance par parcelle-------------------
abondance_par_parcelle <- data3 %>%
  group_by(Site) %>%
  summarise(abondance_totale = sum(Abondance, na.rm = TRUE)) 

abondance_par_parcelle <- abondance_par_parcelle %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")

#--------------2.2 Biomasse par parcelle---------------------
biomasse_par_parcelle <- data3 %>%
  group_by(Site) %>%  
  summarise(biomasse_totale = sum(Poids, na.rm = TRUE))  

biomasse_par_parcelle <- biomasse_par_parcelle %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")

#--------------2.3 Nombre d'espèces------------------
espèces_par_parcelle <- data3 %>%
  group_by(Site) %>%
  summarise(espèces_par_parcelle = n_distinct(Binomial, na.rm = TRUE))

espèces_par_parcelle <- espèces_par_parcelle %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")


#--------------2.4 Anova/Kruskal-Wallis--------------------

#Biomasse

myAOV_biomasse = aov(biomasse_totale ~ TYPE, data=biomasse_par_parcelle)
summary(myAOV_biomasse)

#Vérification de la normalité
hist(residuals(myAOV_biomasse))
qqnorm(residuals(myAOV_biomasse))
qqline(residuals(myAOV_biomasse))
shapiro.test(residuals(myAOV_biomasse))
#vérification de l'homoscédasticité
bartlett.test(biomasse_totale ~ TYPE, data=biomasse_par_parcelle)

#Normalité et homoscédasticité non-vérifiées => Kruskal-Wallis
kruskal.test(biomasse_totale ~ TYPE, data = biomasse_par_parcelle)


#Abondance

myAOV_abondance = aov(abondance_totale ~ TYPE, data=abondance_par_parcelle)
summary(myAOV_abondance)

# Vérification de la normalité des résidus
hist(residuals(myAOV_abondance))
qqnorm(residuals(myAOV_abondance))
qqline(residuals(myAOV_abondance))
shapiro.test(residuals(myAOV_abondance))

# Vérification de l'homoscédasticité
bartlett.test(abondance_totale ~ TYPE, data = abondance_par_parcelle)

#Normalité et homoscédasticité non-vérifiées => Kruskal-Wallis
kruskal.test(abondance_totale ~ TYPE, data = abondance_par_parcelle)


#Nombres d'espèces

myAOV_espèces = aov(espèces_par_parcelle ~ TYPE, data=espèces_par_parcelle)
summary(myAOV_espèces)

#Vérification de la normalité
hist(residuals(myAOV_espèces))
qqnorm(residuals(myAOV_espèces))
qqline(residuals(myAOV_espèces))
shapiro.test(residuals(myAOV_espèces))
#vérification de l'homoscédasticité
bartlett.test(espèces_par_parcelle ~ TYPE, data=espèces_par_parcelle)

#Homoscédasticité non-vérifiée => Kruskal-Wallis
kruskal.test(espèces_par_parcelle ~ TYPE, data=espèces_par_parcelle)


#--------------2.5 Visualisation boxplot-------------------

palette_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

#Graphe 1 : Biomasse 
graph_biomasse <- ggplot(biomasse_par_parcelle, aes(x = TYPE, y = biomasse_totale, fill = TYPE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Suppression des outliers pour éviter le fouillis
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Ajout des points pour mieux voir la distribution
  scale_fill_manual(values = palette_colors) +
  labs(title = "Biomasse par pratique",
       x = "Pratique agricole",
       y = "Biomasse totale (g)") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    
    axis.title = element_text(face = "bold", color = "black", size = 14),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    
    strip.text = element_text(face = "bold", color = "black", size = 14),
    strip.background = element_rect(fill = "white", color = NA)
  )
#Graphe 2 : Abondance 
graph_abondance <- ggplot(abondance_par_parcelle, aes(x = TYPE, y = abondance_totale, fill = TYPE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Suppression des outliers pour éviter le fouillis
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Ajout des points pour mieux voir la distribution
  scale_fill_manual(values = palette_colors) +
  labs(title = "Abondance par pratique",
       x = "Pratique agricole",
       y = "Abondance totale") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    
    axis.title = element_text(face = "bold", color = "black", size = 14),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    
    strip.text = element_text(face = "bold", color = "black", size = 14),
    strip.background = element_rect(fill = "white", color = NA)
  )
#Graphe 3: Richesse spécifique 
graph_richesse <- ggplot(espèces_par_parcelle, aes(x = TYPE, y = espèces_par_parcelle, fill = TYPE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Suppression des outliers pour éviter le fouillis
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Ajout des points pour mieux voir la distribution
  scale_fill_manual(values = palette_colors) +
  labs(title = "Nombre d'espèces par pratique",
       x = "Pratique agricole",
       y = "Nombre d'espèces") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    
    axis.title = element_text(face = "bold", color = "black", size = 14),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    
    strip.text = element_text(face = "bold", color = "black", size = 14),
    strip.background = element_rect(fill = "white", color = NA)
  )
graph_biomasse| graph_abondance | graph_richesse


#_______________________________________________________________________________________

#--------------3. Diversité alpha et indices associés-------------------

#_______________________________________________________________________________________
#Mesure de la diversité alpha à l'aide de 3 indices : Shannon, Simpson et Piélou
#Les indices sont calculés à l'aide du package "vegan"

#---------------3.1 Indice de Shannon------------------

# Création de la matrice d'abondance par site
matrice_abondance <- data3 %>%
  select(Site, Binomial) %>%
  filter(!is.na(Binomial))%>% #Pour ne avoir de NAs à cause des juvéniles
  count(Site, Binomial) %>%
  spread(Binomial, n, fill = 0) %>%
  column_to_rownames("Site")

shannon_df <- data.frame(Site = rownames(matrice_abondance),
                         Shannon = diversity(matrice_abondance, index = "shannon"))

#Ajout d'une colonne avec le type de pratique agricole
shannon_df <- shannon_df %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")

#---------------3.2 Indice de Simpson------------------

simpson_df <- data.frame(Site = rownames(matrice_abondance), 
                         Simpson = diversity(matrice_abondance, index = "simpson"))

#Ajout d'une colonne avec le type de pratique agricole
simpson_df <- simpson_df %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")

#---------------3.3 Indice de Piélou-------------------

pielou_df <- data.frame(Site = rownames(matrice_abondance), 
                        Pielou = diversity(matrice_abondance, index = "shannon") / log(specnumber(matrice_abondance)))

#Ajout d'une colonne avec le type de pratique agricole
pielou_df <- pielou_df %>%
  left_join(Sites[, c("Site", "TYPE")], by = "Site")

#---------------3.4 Anova/Kruskal-wallis---------------
#Shannon
myAOV_shannon = aov(Shannon ~ TYPE, data=shannon_df)
summary(myAOV_shannon)

# Vérification de la normalité des résidus
hist(residuals(myAOV_shannon))
qqnorm(residuals(myAOV_shannon))
qqline(residuals(myAOV_shannon))
shapiro.test(residuals(myAOV_shannon))

# Vérification de l'homoscédasticité
bartlett.test(Shannon ~ TYPE, data = shannon_df)

#Normalité vérifiée et homoscédasticité non-vérifiées => Kruskal-Wallis
kruskal.test(Shannon ~ TYPE, data = shannon_df)

#Simpson
myAOV_simpson = aov(Simpson ~ TYPE, data=simpson_df)
summary(myAOV_simpson)

# Vérification de la normalité des résidus
hist(residuals(myAOV_simpson))
qqnorm(residuals(myAOV_simpson))
qqline(residuals(myAOV_simpson))
shapiro.test(residuals(myAOV_simpson))

# Vérification de l'homoscédasticité
bartlett.test(Simpson ~ TYPE, data = simpson_df)

#Normalité et homoscédasticité vérifiées =>  mais Kruskal-Wallis pour vérification
kruskal.test(Simpson ~ TYPE, data = simpson_df)

#Piélou
myAOV_pielou = aov(Pielou ~ TYPE, data=pielou_df)
summary(myAOV_pielou)

# Vérification de la normalité des résidus
hist(residuals(myAOV_pielou))
qqnorm(residuals(myAOV_pielou))
qqline(residuals(myAOV_pielou))
shapiro.test(residuals(myAOV_pielou))

# Vérification de l'homoscédasticité
bartlett.test(Pielou ~ TYPE, data = pielou_df)

#Normalité et homoscédasticité vérifiées =>  mais Kruskal-Wallis pour vérification
kruskal.test(Pielou ~ TYPE, data = pielou_df)

#---------------3.5 Visualisation----------------------

#Fusion des trois dataframes en un seul
diversity_df <- shannon_df %>%
  left_join(simpson_df, by = c("Site", "TYPE")) %>%
  left_join(pielou_df, by = c("Site", "TYPE"))

# Conversion en format long
diversity_long <- diversity_df %>%
  pivot_longer(cols = c("Shannon", "Simpson", "Pielou"),
               names_to = "Indice",
               values_to = "Valeur")

# Palette de couleurs
palette_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

# Graphique combiné
ggplot(diversity_long, aes(x = TYPE, y = Valeur, fill = TYPE)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  scale_fill_manual(values = palette_colors) +
  facet_wrap(~ Indice, scales = "free_y") +
  labs(title = "Indices de diversité alpha par pratique agricole",
       x = "Pratique agricole",
       y = "Valeur de l'indice") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title = element_text(face = "bold", color = "black", size = 14),
    axis.text = element_text(face = "bold", color = "black", size = 12),
    plot.title = element_text(face = "bold", color = "black", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", color = "black", size = 14),
    strip.background = element_rect(fill = "white", color = NA)
  )
#_______________________________________________________________________________________

#--------------4.Pheatmap---------------------------------

#_______________________________________________________________________________________
Prim_DB = VDT_adultes[, c("Site", "Year", "Trap N", "Method", "Binomial","LifeStage", "Poids", "Abondance")]
Prim_DB = Prim_DB[order(Prim_DB$Site),]

Melt = melt(Prim_DB,
            id.vars = c("Site", "Year", "Binomial","Poids"),
            measure.vars = "Abondance")

Data_C = dcast(Melt, Site ~ Binomial, fun.aggregate = sum, value.var = "value")
Data_C = Sites[, c(1:6)] %>%
  left_join(Data_C, by = "Site")

Data_samp = dcast(Melt, Site ~ Binomial, fun.aggregate = sum, value.var = "value")
Data_samp = Sites[, c(1:6)] %>%
  left_join(Data_samp, by = "Site")

t.Data_C = as.data.frame(t(Data_C[, -c(1:6)]))
colnames(t.Data_C) = Data_C$Site

#Création d'un dataframe pour l'annotation des types
annotation_col = data.frame(TYPE = Data_C$TYPE)
rownames(annotation_col) = Data_C$Site

ordered_sites <- annotation_col %>%
  arrange(TYPE) %>%
  rownames()

t.Data_C <- t.Data_C[, ordered_sites]

# Palette de couleurs
palette_colors <- c("ABC"="#1b9e77","BIO"="#d95f02","CST"="#7570b3","CVL"= "#e7298a")

#Générer le heatmap
pheatmap(t.Data_C,
         border_color = "white",
         cellwidth = 10,
         cellheight = 8,
         display_numbers = FALSE,
         legend = TRUE,
         legend_breaks = c(0,10,20,30,40),
         fontsize_col = 10,
         fontsize_row = 6,
         main = "Earthworms abundance",
         scale = "none",
         cluster_row = FALSE,
         cluster_col = FALSE,
         annotation_col = annotation_col,
         annotation_colors = list(TYPE = palette_colors))
#_______________________________________________________________________________________

#--------------5.Accumulation curves----------------------

#_______________________________________________________________________________________
Prim_DB = data3[, c("Site", "Year", "Trap N","Method", "Binomial", "LifeStage", "Poids","Abondance")]
Prim_DB = Prim_DB[order(Prim_DB$Site),]

Melt = melt(Prim_DB,
            id.vars = c("Site", "Year","Trap N","Method", "Binomial"),
            measure.vars = "Abondance")

Data_V = dcast(Melt, Site ~ Binomial, fun.aggregate = sum, value.var = "value")
Data_V = Sites[, c(1:6)] %>%
  left_join(Data_V, by = "Site")

Data_samp = dcast(Melt, Site ~ Binomial, fun.aggregate = sum, value.var = "value")
Data_samp = Sites[, c(1:6)] %>%
  left_join(Data_samp, by = "Site")

accum = specaccum(Data_V[, -c(1:6)], method = "random", permutations = 100)
accum_df = data.frame(
  sites = accum$sites,
  richness = accum$richness,
  sd = accum$sd
)

ggplot(accum_df, aes(x = sites, y = richness)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon( #erreur standard
    aes(ymin = richness - sd, ymax = richness + sd),
    fill = "blue", alpha = 0.2  #gérer transfarence 20% opacité
  ) +
  labs(
    x = "Nombre d'échantillons",
    y = "Richesse spécifique",
    title = "Courbe d'accumulation d'espèces"
  ) +
  theme_bw()

#Courbes d'accumulation pour les différents TYPEs  
types_site = unique(Data_V$TYPE)
accum_list = list()

for(t in types_site) {
  df_sub = Data_V %>%
    filter(TYPE == t)
  
  accum_t = specaccum(df_sub[, -c(1:6)], method = "random", permutations = 100)
  
  accum_df_t = data.frame(
    sites = accum_t$sites,
    richness = accum_t$richness,
    sd = accum_t$sd,
    Type = t
  )
  accum_list[[t]] = accum_df_t
}

accum_all_df = do.call(rbind, accum_list) #rbind = joindre par les lignes

ggplot(accum_all_df, aes(x = sites, y = richness, color = Type, fill = Type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = richness - sd, ymax = richness + sd), 
    alpha = 0.2, color = NA
  ) +
  labs(
    x = "Nombre de sites",
    y = "Richesse spécifique",
    title = "Courbes d'accumulation d'espèces par type de site"
  ) +
  theme_bw()

#_______________________________________________________________________________________

#-------------------6. Diversité beta --------------

#_______________________________________________________________________________________

Comm = as.data.frame(Data_C[, c(7:ncol(Data_C))])
row.names(Comm) = Data_C$Site
Comm_bin = Comm
Comm_bin[Comm_bin > 0] = 1

(beta_result = beta.pair(Comm_bin, index.family = "sorensen"))
beta_total = as.matrix(beta_result$beta.sor)
beta_turnover = as.matrix(beta_result$beta.sim)
beta_nestedness = as.matrix(beta_result$beta.sne)
sites = rownames(Comm_bin)

beta_df = data.frame(
  Site1 = character(),
  Site2 = character(),
  Type1 = character(),
  Type2 = character(),
  Beta_Tot = numeric(),
  Beta_Turnover = numeric(),
  Beta_Nestedness = numeric(),
  stringsAsFactors = FALSE
)

for(i in 1:(length(sites) - 1)) {
  for(j in (i + 1):length(sites)) {
    beta_df = rbind(
      beta_df,
      data.frame(
        Site1 = sites[i],
        Site2 = sites[j],
        Type1 = Data_C$TYPE[Data_C$Site == sites[i]],
        Type2 = Data_C$TYPE[Data_C$Site == sites[j]],
        Beta_Tot = beta_total[i, j],
        Beta_Turnover = beta_turnover[i, j],
        Beta_Nestedness = beta_nestedness[i, j]
      )
    )
  }
}

beta_long = beta_df %>%
  mutate(Type_Comparison = paste(Type1, "vs", Type2)) %>%
  mutate(Comparison_Type = ifelse(Type1 == Type2, "Intra", "Inter")) %>%
  tidyr::pivot_longer(
    cols = starts_with("Beta"),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(beta_long, aes(x = Type_Comparison, y = Value, fill = Metric)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    title = "Diversité Bêta par Paires de Types",
    x = "Comparaison de Types",
    y = "Valeur de Diversité Bêta",
    fill = "Composant"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#Analyses Krukal-Wallis

# Séparation des données
intra_beta = beta_df %>% filter(Type1 == Type2)
inter_beta = beta_df %>% filter(Type1 != Type2)

# Ajouter une colonne pour identifier Intra ou Inter
beta_df$Comparison_Type = ifelse(beta_df$Type1 == beta_df$Type2, "Intra", "Inter")

# Test de Kruskal-Wallis pour chaque métrique
kruskal_tot = kruskal.test(Beta_Tot ~ Comparison_Type, data = beta_df)
kruskal_turnover = kruskal.test(Beta_Turnover ~ Comparison_Type, data = beta_df)
kruskal_nestedness = kruskal.test(Beta_Nestedness ~ Comparison_Type, data = beta_df)

# Affichage des résultats
kruskal_tot
kruskal_turnover
kruskal_nestedness

#_______________________________________________________________________________________

#---------------7. NMDS---------------

#_______________________________________________________________________________________

Comm = as.data.frame(Data_C[, c(7:ncol(Data_C))])
row.names(Comm) = Data_C$Site
Comm_hel = decostand(Comm, "hellinger", na.rm = TRUE)

set.seed(123)
Meta = metaMDS(Comm_hel, distance = "bray", engine = "monoMDS", k = 2, trymax = 100) #k=nombre de dimensions de notre espace
#si stress <0,2 système fonctionne bien / mesure la fidélité de la représentation
stressplot(Meta)
gof = goodness(Meta)
gof

plot(Meta, display = "sites", type = "none")
points(Meta, display = "sites", cex = 2*gof/mean(gof)) #Pas de site qui possede une part de la variance trop importante

#Data_C <- left_join(Data_C, Sites, by = "Site")

Meta.points = data.frame(Meta$points)
Meta.points$Type = Data_C$TYPE
Meta.points$Zone=Data_C$ZONE
sp = data.frame(Meta$species)

stress = round(Meta$stress, 3)
Stress.lab = paste0("Stress == ", stress)


ggplot() +
  theme_bw() +
  geom_convexhull(
    data = Meta.points,
    mapping = aes(
      x = MDS1, 
      y = MDS2, 
      group = factor(Type), 
      fill = Type, 
      color = Type
    ),
    alpha = 0.1,
    linewidth = 0.7
  ) +
  annotate(
    "text", 
    x = -1.4, 
    y = -1.5, 
    label = Stress.lab,
    parse = TRUE,
    hjust = 0.2,
    size = 5,
    fontface = "bold"
  ) +
  geom_point(
    data = Meta.points, 
    mapping = aes(x = MDS1, y = MDS2),
    size = 3, alpha = 0.9, color = "black"
  ) +
  geom_point(
    data = sp, 
    mapping = aes(x = MDS1, y = MDS2),
    alpha = 1, color = "darkorange"
  ) +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold")
  )

dist_bray = vegdist(Comm_hel, method = "bray")
# Calculer les dispersions pour chaque niveau de "Type"
dispersion_TYPE = betadisper(dist_bray, Data_C$TYPE)
dispersion_ZONE = betadisper(dist_bray, Data_C$ZONE)
# Test statistique pour l'homogénéité des dispersions
anova(dispersion_TYPE)
anova(dispersion_ZONE)

# Visualiser les dispersions
plot(dispersion_TYPE)
plot(dispersion_ZONE)
#Homogénéité des Dispersions : La valeur p associée au facteur "Type" est de 0.896,
#Cela indique qu'il n'y a pas de différence significative dans la dispersion
#(variabilité interne) entre les différents niveaux de "Type".

#hypothèse d'homogénéité des dispersions est respectée.On peut faire une PERMANOVA

(Ado = adonis2(dist_bray ~ TYPE, data = Data_C, perm = 999))
(Ado = adonis2(dist_bray ~ ZONE, data = Data_C, perm = 999))

(Pair_Ado = pairwise.adonis2(dist_bray ~ TYPE, data=Data_C))
(Pair_Ado = pairwise.adonis2(dist_bray ~ ZONE, data=Data_C))

#_______________________________________________________________________________________

#---------------- 8. IndVal --------------------

#_______________________________________________________________________________________


IV = as.data.frame(Data_C)
IV$TYPE = factor(IV$TYPE)
row.names(IV) = IV$Site

indic = IV[, -c(1:6)]
indic = indic[, colSums(indic != 0) >= 1]
indicpa = ifelse(indic > 0, 1, 0)

indval_1 = multipatt(indicpa, IV$TYPE, func = "IndVal.g", max.order = 1, #on peut changer type pour nouveau cluster
                     control = how(nperm = 999))
indval_2 = multipatt(indicpa, IV$ZONE, func = "IndVal.g", max.order = 1, #on peut changer type pour nouveau cluster
                     control = how(nperm = 999))


#A = Specificity, 1=> 100% des spécimens de cette espèce ont été retrouvées dans ce Type X.
#B = Fidelity, 1=> Parmi les sites du Type X, sur 100% des sites on a retrouvé cette espèce
summary(indval_1, indvalcomp = TRUE)
summary(indval_2, indvalcomp = TRUE)
summary(indval_1, indvalcomp = TRUE, At = 0.6, Bt = 0.6) #Valeurs standards appliquées généralement
summary(indval_2, indvalcomp = TRUE, At = 0.6, Bt = 0.6) #Valeurs standards appliquées généralement

#_______________________________________________________________________________________

#--------------9.Régression linéaire STIR----------------------

#_______________________________________________________________________________________
#Les données de STIR sont contenues dans un dataframe généré par un autre code
#"final_data" contient les valeurs de STIR pour chaque parcelle et ce depuis 2022

# Filtrer les années d'intérêt
stir_filtree <- final_data %>% filter(year %in% c(2022, 2023, 2024, 2025))

stir_filtree_2025 <- stir_filtree %>%
  filter(year_study == 2025)

# Calcul du STIR basé sur différentes périodes
stir_aggrege <- stir_filtree_2025 %>%
  group_by(site) %>%
  summarise(
    STIR_sum_4ans = sum(stir_total[year %in% c(2022, 2023, 2024, 2025)],na.rm = TRUE),
    STIR_sum_3ans = sum(stir_total[year %in% c(2023, 2024, 2025)],na.rm = TRUE),
    STIR_sum_2ans = sum(stir_total[year %in% c(2024, 2025)],na.rm = TRUE),
    STIR_sum_1an = sum(stir_total[year %in% c(2025)],na.rm = TRUE),
    STIR_moy_3ans = mean(stir_total[year %in% c(2022, 2023, 2024)], na.rm = TRUE),
    STIR_moy_2ans = mean(stir_total[year %in% c(2023, 2024)], na.rm = TRUE),
    STIR_sum2_3ans = sum(stir_total[year %in% c(2022, 2023, 2024)], na.rm = TRUE),
    STIR_sum2_2ans = sum(stir_total[year %in% c(2023, 2024)], na.rm = TRUE),
    STIR_sum2_1an = sum(stir_total[year %in% c(2024)], na.rm = TRUE),
    STIR_2025 = stir_total[year == 2025],
    STIR_2024 = stir_total[year == 2024],
    STIR_2023 = stir_total[year == 2023],
    STIR_2022 = stir_total[year == 2022]
  )

#-----------------9.1 Abondance-----------------
#Vérification que le nom de la colonne "site" est bien le même partout
abondance_par_parcelle <- abondance_par_parcelle %>%
  rename(site=Site)

stir_aggrege_abondance <- left_join(stir_aggrege, abondance_par_parcelle, by = "site")

#Régressions linéaires pour l'abondance
reg_moyenne_3ans <- lm(abondance_totale ~ STIR_moy_3ans, data= stir_aggrege_abondance)
summary(reg_moyenne_3ans)
reg_moyenne_2ans <- lm(abondance_totale ~ STIR_moy_2ans, data= stir_aggrege_abondance)
summary(reg_moyenne_2ans)
reg_sum_4ans <- lm(abondance_totale ~ STIR_sum_4ans, data= stir_aggrege_abondance)
summary(reg_sum_4ans)
reg_sum_3ans <- lm(abondance_totale ~ STIR_sum_3ans, data= stir_aggrege_abondance)
summary(reg_sum_3ans)
reg_sum_2ans <- lm(abondance_totale ~ STIR_sum_2ans, data= stir_aggrege_abondance)
summary(reg_sum_2ans)
reg_sum_1an <- lm(abondance_totale ~ STIR_sum_1an, data= stir_aggrege_abondance)
summary(reg_sum_1an)
reg_sum2_3ans <- lm(abondance_totale ~ STIR_sum2_3ans, data= stir_aggrege_abondance)
summary(reg_sum2_3ans)
reg_sum2_2ans <- lm(abondance_totale ~ STIR_sum2_2ans, data= stir_aggrege_abondance)
summary(reg_sum2_2ans)
reg_sum2_1an <- lm(abondance_totale ~ STIR_sum2_1an, data= stir_aggrege_abondance)
summary(reg_sum2_1an)

#pas de visualisation ici car toutes les p-values > 0,05

#-----------------9.2 Biomasse---------------------
biomasse_par_parcelle <- biomasse_par_parcelle %>%
  rename(site=Site)

stir_aggrege_biomasse <- left_join(stir_aggrege, biomasse_par_parcelle, by = "site")

# Régressions linéaires pour la biomasse
reg_biom_moyenne_3ans <- lm(biomasse_totale ~ STIR_moy_3ans, data = stir_aggrege_biomasse)
summary(reg_biom_moyenne_3ans)
reg_biom_moyenne_2ans <- lm(biomasse_totale ~ STIR_moy_2ans, data = stir_aggrege_biomasse)
summary(reg_biom_moyenne_2ans)
reg_biom_sum_4ans <- lm(biomasse_totale ~ STIR_sum_4ans, data = stir_aggrege_biomasse)
summary(reg_biom_sum_4ans)
reg_biom_sum_3ans <- lm(biomasse_totale ~ STIR_sum_3ans, data = stir_aggrege_biomasse)
summary(reg_biom_sum_3ans)
reg_biom_sum_2ans <- lm(biomasse_totale ~ STIR_sum_2ans, data = stir_aggrege_biomasse)
summary(reg_biom_sum_2ans)
reg_biom_sum_1an <- lm(biomasse_totale ~ STIR_sum_1an, data = stir_aggrege_biomasse)
summary(reg_biom_sum_1an)
reg_biom_sum2_3ans <- lm(biomasse_totale ~ STIR_sum2_3ans, data = stir_aggrege_biomasse)
summary(reg_biom_sum2_3ans)
reg_biom_sum2_2ans <- lm(biomasse_totale ~ STIR_sum2_2ans, data = stir_aggrege_biomasse)
summary(reg_biom_sum2_2ans)
reg_biom_sum2_1an <- lm(biomasse_totale ~ STIR_sum2_1an, data = stir_aggrege_biomasse)
summary(reg_biom_sum2_1an)
#Plusieurs p-values < 0,05 et R² entre 20 et 30%

#Visualisation des relations Biomasse ~ STIR

ggplot(stir_aggrege_biomasse, aes(x = STIR_moy_3ans, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_biomasse, aes(x = STIR_moy_2ans, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_biomasse, aes(x = STIR_sum_4ans, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_biomasse, aes(x = STIR_sum_3ans, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_biomasse, aes(x = STIR_sum_2ans, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_biomasse, aes(x = STIR_sum_1an, y = biomasse_totale)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()


#-----------------9.3 Nombre d'espèces-----------
# Fusion des données
espèces_par_parcelle <- espèces_par_parcelle %>%
  rename(site = Site)

stir_aggrege_especes <- left_join(stir_aggrege, espèces_par_parcelle, by = "site")

# Régressions linéaires pour le nombre d'espèces
reg_esp_moyenne_3ans <- lm(espèces_par_parcelle ~ STIR_moy_3ans, data = stir_aggrege_especes)
summary(reg_esp_moyenne_3ans)
reg_esp_moyenne_2ans <- lm(espèces_par_parcelle ~ STIR_moy_2ans, data = stir_aggrege_especes)
summary(reg_esp_moyenne_2ans)
reg_esp_sum_4ans <- lm(espèces_par_parcelle ~ STIR_sum_4ans, data = stir_aggrege_especes)
summary(reg_esp_sum_4ans)
reg_esp_sum_3ans <- lm(espèces_par_parcelle ~ STIR_sum_3ans, data = stir_aggrege_especes)
summary(reg_esp_sum_3ans)
reg_esp_sum_2ans <- lm(espèces_par_parcelle ~ STIR_sum_2ans, data = stir_aggrege_especes)
summary(reg_esp_sum_2ans)
reg_esp_sum_1an <- lm(espèces_par_parcelle ~ STIR_sum_1an, data = stir_aggrege_especes)
summary(reg_esp_sum_1an)
reg_esp_sum2_3ans <- lm(espèces_par_parcelle ~ STIR_sum2_3ans, data = stir_aggrege_especes)
summary(reg_esp_sum2_3ans)
reg_esp_sum2_2ans <- lm(espèces_par_parcelle ~ STIR_sum2_2ans, data = stir_aggrege_especes)
summary(reg_esp_sum2_2ans)
reg_esp_sum2_1an <- lm(espèces_par_parcelle ~ STIR_sum2_1an, data = stir_aggrege_especes)
summary(reg_esp_sum2_1an)

#Visualisation
ggplot(stir_aggrege_especes, aes(x = STIR_moy_3ans, y = espèces_par_parcelle)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
ggplot(stir_aggrege_especes, aes(x = STIR_sum_2ans, y = espèces_par_parcelle)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

#_______________________________________________________________________________________

#-------------------10. Modéle linéaire général------------

#_______________________________________________________________________________________
#Je crée un dataframe avec toutes les variables explicatives à tester. 
#J'ajoute d'abord la densité de semis et la distance inter-rangs
data_reg = final_data[,c(1:2,21:22)]
data_reg_2025 <- data_reg %>%
  filter(year == 2025)

#J'ajoute les 3 variables réponses (biomasse, abondance et nombre d'espèces)
#et toutes les valeurs de STIR caluculées précedemment
data_reg_2025 <- left_join(data_reg_2025, stir_aggrege, by = "site")
data_reg_2025 <- left_join(data_reg_2025, biomasse_par_parcelle[,c(1:2)], by = "site")
data_reg_2025 <- left_join(data_reg_2025, espèces_par_parcelle[,c(1:2)], by = "site")
data_reg_2025 <- left_join(data_reg_2025, abondance_par_parcelle, by = "site")


#On extrait les variables explicatives

vars_expl = data_reg_2025[,c(3:17)]

# Calcul de la matrice de corrélation
cor_var <- cor(vars_expl, use = "complete.obs", method = "pearson")

# On récupère les indices de la partie supérieure de la matrice
upper_tri_indices <- which(upper.tri(cor_var), arr.ind = TRUE)

# Transformation en dataframe longue
df_cor_var <- data.frame(
  VarA = rownames(cor_var)[upper_tri_indices[, 1]],
  VarB = colnames(cor_var)[upper_tri_indices[, 2]],
  Correlation = cor_var[upper_tri_indices],
  Corr_abs = abs(cor_var[upper_tri_indices])
)

# On garde les corrélations > 0.7 (en valeur absolue)
df_cor_var_70 <- subset(df_cor_var, Corr_abs >= 0.7)

corrplot(cor_var, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         number.digits = 1, cl.cex = 0.7, diag = FALSE)


library(car)
model_vif = lm(biomasse_totale ~ scale(densite_semis) + scale(STIR_sum_4ans) + scale(STIR_2025),
               data = data_reg_2025)
summary(model_vif)






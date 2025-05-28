*BING-F535 - travail de fin d'année : Analyse de l’impact des pratiques agricoles sur la diversité des communautés lombriciennes*

Le script principal Doclot_Pivetta_Zarrella permet de comparer les effets des pratiques agricoles sur les vers de terre en présence, ainsi que les effets du STIR (soil tillage intensity rating), lui-même calculé avec le script STIR_new. 

#contenu

- ANOVA/Kruskal-Wallis
- Indices Shannon, Simpson, Piélou
- Courbes d'accumulation
- diversité bêta
- NMDS
- régressions linéaires STIR et modèle linéaire général

#structure

├── Doclot_Pivetta_Zarella_R # Script principal

├── STIR_new.R # Script secondaire (utilise DRBDC.json)

├── stir.json # Fichier JSON requis pour S.R

└── README.md

  #installation
  
  ```bash
  git clone https://github.com/antoinedoclot/Project-Earthworms.git
  cd Project-Earthworms



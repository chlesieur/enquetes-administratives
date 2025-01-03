library(readxl)
library(dplyr)
library(stringr)

### Cette partie serait utile si on gardait le num_candidat dans pamplemousse

# il faur transformer le base pamplemousse en UTF en modifiant enregistrer sous dans excel
base_pamplemousse <- read.csv2("data/export_pamplemousse.csv", encoding = "UTF-8", header = T)

colnames(base_pamplemousse)[1] <- "lib_voie"

### Détermination du périmètre (admis en 1A, concours externe, concours 2024)

# base_pamplemousse_1A <- base_pamplemousse %>%
#   dplyr::filter(str_starts(lib_voie, "1A,1A"))
# 
# base_pamplemousse_1A_concours_externe <- base_pamplemousse_1A %>%
#   dplyr::filter(str_starts(concours_origine, "Concours externe"))

base_pamplemousse_2024 <- base_pamplemousse %>%
  dplyr::filter(str_starts(concours_annee, "2024"))

# ### Lecture des fichiers des inscriptions
maths <- read_excel("data/Inscription_maths.xlsx", skip = 1, col_names = TRUE)

BL_attache <- read_excel("data/Inscription_attache_BL.xls", skip = 1, col_names = TRUE)
BL_ingenieur <- read_excel("data/Inscription_ingenieur_BL.xls",skip = 1, col_names = TRUE)

D2_attache <- read_excel("data/Inscription_attaché_D2.xls",skip = 1, col_names = TRUE)
D2_ingenieur <- read_excel("data/Inscription_ingenieur_D2.xls",skip = 1, col_names = TRUE)

inscris <- rbind(maths,BL_ingenieur,BL_attache,D2_attache,D2_ingenieur)

### Sélection des Admis dans les fichiers sous Z:/

admis <- read_excel("data/Admis.xlsx") %>% rename(CODE_CANDIDAT = admis)

admis_unique <- unique(admis)

### Fusion des Admis avec les informations sur les inscrits

admis <- left_join(admis_unique,inscris, by ="CODE_CANDIDAT")

admis_unique <- admis[!duplicated(admis$CODE_CANDIDAT),]

### Statistique par voie d'admission




### Statistique par filière

library(dplyr)  
library(stringr)

admis_unique$SERIE

admis_unique <- admis_unique %>% filter(statut=="Ing")


admis_unique_hors_bacheliers_etrangers <- admis_unique %>%
  filter(!SERIE %in% c("Bac ou équivalent étranger",
                       "SESM Maroc - Sciences Expérimentales - Sciences Mathématiques"))

avant_reforme_2021 <- admis_unique_hors_bacheliers_etrangers %>% 
  filter(SERIE != "BAC Général  (réforme 2021)") %>% 
  mutate(type_bac = case_when(
    substr(SERIE,1,1) == "S" ~ "dont BAC S",
    TRUE ~ "autres")) %>% 
  group_by(MENTION, type_bac) %>% 
  summarise(effectif = n())

avant_reforme_2021

apres_reforme_2021_physique_chimie <- admis_unique_hors_bacheliers_etrangers %>% 
  filter(SERIE == "BAC Général  (réforme 2021)",
         SPECIALITE_BAC_TERMINALE1 %in% c("Mathématiques","Physique-Chimie") & 
         SPECIALITE_BAC_TERMINALE2 %in% c("Mathématiques","Physique-Chimie")) %>% 
  group_by(MENTION) %>% 
  summarise(effectif = n())

apres_reforme_2021_physique_chimie

maths_sciences_exactes_defi <- c("Mathématiques", "Physique-Chimie", "Numérique et sciences informatiques", "Science de l'Ingénieur (Sous-Épreuves)")
shs_defi <- c("Sciences économiques et sociales","Hist.-Géo. géopolitique & sc.Politiques","Humanités, littérature et philosophie","Langues, litt. & cult. étrang. & région.","Hist.-Géo. géopolitique & sc.Politiques")

apres_reforme_2021_ensemble <- admis_unique_hors_bacheliers_etrangers %>% 
  filter(SERIE == "BAC Général  (réforme 2021)") %>% 
  mutate(filiere_defi = case_when(
    SPECIALITE_BAC_TERMINALE1 %in% maths_sciences_exactes_defi & SPECIALITE_BAC_TERMINALE2 %in% maths_sciences_exactes_defi ~ "Maths + sciences exactes",
    (SPECIALITE_BAC_TERMINALE1 == "Mathématiques" & SPECIALITE_BAC_TERMINALE2 %in% shs_defi) | (SPECIALITE_BAC_TERMINALE2 == "Mathématiques" & SPECIALITE_BAC_TERMINALE1 %in% shs_defi) ~ "Maths + shs",
    TRUE ~ "autres"))


table(apres_reforme_2021_ensemble$filiere_defi, apres_reforme_2021_ensemble$MENTION)

  

# Créer la nouvelle variable avec dplyr
df <- df %>%
  mutate(est_dans_domaine = if_any(c(domaine1, domaine2), ~ .x %in% domaines_cibles))

unique(admis_unique_hors_bacheliers_etrangers$SERIE)
unique(admis_unique_hors_bacheliers_etrangers$SPECIALITE_BAC_TERMINALE1)
unique(admis_unique_hors_bacheliers_etrangers$SPECIALITE_BAC_TERMINALE2)




  group_by(SPECIALITE_BAC_TERMINALE1, SPECIALITE_BAC_TERMINALE2, MENTION) %>% 
  summarise(effectif=n())

library(tidyverse)

# Transformer en tableau croisé
filiere <- mention_options %>%
  pivot_wider(
    names_from = MENTION,
    values_from = effectif,
    values_fill = NA
  )

write.csv2(filiere,"type de bac des primo-entrants en formation d'ingénieur.csv")

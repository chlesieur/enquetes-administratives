library(readxl)
library(dplyr)
library(stringr)

### Cette partie serait utile si on gardait le num_candidat dans pamplemousse

# il faur transformer le base pamplemousse en UTF en modifiant enregistrer sous dans excel
base_pamplemousse <- read.csv2("export_pamplemousse.csv", encoding = "UTF-8")

### Détermination du périmètre (admis en 1A, concours externe, concours 2024)
base_pamplemousse_1A <- base_pamplemousse %>%
  dplyr::filter(str_starts(lib_voie, "1A,1A"))

base_pamplemousse_1A_concours_externe <- base_pamplemousse_1A %>%
  dplyr::filter(str_starts(concours_origine, "Concours externe"))

base_pamplemousse_1A_concours_externe_2024 <- base_pamplemousse_1A_concours_externe %>%
  dplyr::filter(str_starts(concours_annee, "2024"))

# ### Lecture des fichiers des inscriptions
BL_attache <- read_excel("Inscription_attache_BL.xls", skip = 1, col_names = TRUE)
BL_ingenieur <- read_excel("Inscription_ingenieur_BL.xls",skip = 1, col_names = TRUE)

D2_attache <- read_excel("Inscription_attaché_D2.xls",skip = 1, col_names = TRUE)
D2_ingenieur <- read_excel("Inscription_ingenieur_D2.xls",skip = 1, col_names = TRUE)

inscris <- rbind(maths,BL_ingenieur,BL_attache,D2_attache,D2_ingenieur)

### Sélection des Admis dans les fichiers sous Z:/

admis <- read_excel("Admis.xlsx") %>% rename(CODE_CANDIDAT = admis)

admis_unique <- unique(admis)

### Fusion des Admis avec les informations sur les inscrits

admis <- left_join(admis_unique,inscris, by ="CODE_CANDIDAT")

admis_unique <- admis[!duplicated(admis$CODE_CANDIDAT),]

### Statistique par filière

library(dplyr)  # Assuming you're already using dplyr
library(stringr)

admis_unique <- admis_unique %>%
  mutate(CLASSE2 = case_when(
    CLASSE == "MP" ~ "MP",
    CLASSE == "MP*" ~ "MP",
    substr(CLASSE, 1, 2) == "PC" ~ "PC",
    substr(CLASSE, 1, 3) == "PSI" ~ "PSI",
    substr(CLASSE, 1, 3) == "MPI" ~ "MPI",
    substr(CLASSE, 1, 3) == "Let" ~ "BL",
    substr(CLASSE, 1, 3) == "Cac" ~ "D2",
    CLASSE == "Non scolarisé Ingénieur statisticien" ~ "D2",
    TRUE ~ ""
  ))

stat_prepa <- admis_unique %>% 
  group_by(VILLE_ETABLISSEMENT,ETABLISSEMENT,CLASSE2) %>% 
  summarise(n = n())

library(tidyverse)

# Transformer en tableau croisé
stat_prepa_etab <- stat_prepa %>%
  pivot_wider(
    names_from = CLASSE2,
    values_from = n,
    values_fill = NA
  )

write.csv2(stat_prepa_etab,"L'Étudiant Origine élèves Ensai prépa_2024.csv")

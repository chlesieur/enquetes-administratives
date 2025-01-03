library(readxl)
library(dplyr)
library(stringr)

# Import et mise au propre de la base

base_pamplemousse <- read.csv2("data/export_requete_CGE_20241127.csv", encoding = "latin1")
noms_variables <- names(base_pamplemousse)
nouveaux_noms <- gsub("^X.", "", noms_variables)
names(base_pamplemousse) <- nouveaux_noms

supprimer_egal <- function(x) {
  if (is.character(x)) {
    gsub("^=", "", x)
  } else {
    x
  }
}

base_pamplemousse <- as.data.frame(sapply(base_pamplemousse, supprimer_egal))

base_pamplemousse_2 <- base_pamplemousse %>% 
  filter(!grepl("sort", lib_voie, ignore.case = TRUE),
         !substr(lib_voie,1,1)=="!") %>% 
    mutate(
    voie = case_when(
      grepl("ing", libelle_statut_etudiant) == TRUE ~ "Ingénieurs",
      grepl("att", libelle_statut_etudiant) == TRUE ~ "Attachés",
      grepl("doct", libelle_statut_etudiant) == TRUE ~ "Doctorant",
      grepl("Mastère", libelle_statut_etudiant) == TRUE ~ "Mastère",
      TRUE ~ "Autres"),
    filiere = case_when(
      grepl("Césure", lib_voie, ignore.case = TRUE) == TRUE & grepl("sortant", lib_voie, ignore.case = TRUE) == FALSE ~ "Césure",
      grepl("Doct", lib_voie, ignore.case = TRUE) == TRUE ~ "Doctorant",
      grepl("MSP", lib_voie, ignore.case = TRUE) == TRUE ~ "MSP",
      grepl("MSD", lib_voie, ignore.case = TRUE) == TRUE ~ "MSD",
      grepl("digi", lib_voie, ignore.case = TRUE) == TRUE ~ "DIGI",
      grepl("ES", lib_voie, ignore.case = TRUE) == TRUE ~ "ES",
      grepl("MS Datascience", lib_voie, ignore.case = TRUE) == TRUE ~ "Mastère",
      grepl("ext", lib_voie, ignore.case = TRUE) == TRUE ~ "Scolarité à l'extérieur",
      substr(lib_voie, 1, 2) == "1A" ~ "3A",
      substr(lib_voie, 1, 2) == "2A" ~ "4A" ,
      substr(lib_voie, 1, 2) == "3A" ~ "5A",
      TRUE ~ "NC"
    ),
    sexe = case_when(
      libelle_etat_civil == "Monsieur" ~ "Hommes",
      libelle_etat_civil == "Madame" ~ "Femmes",
      TRUE ~ "0"
    ),
    nationalite = case_when(
      id_nationalite == 100 ~ "Français",
      TRUE ~"Étranger"
    )
    )

### Tableau sur les inscrits 3A - 4A - 5A - Césure

tab_eff_1 <- base_pamplemousse_2 %>% 
  filter(!grepl("att", lib_voie, ignore.case = TRUE)) %>% 
  group_by(sexe, nationalite, filiere) %>% 
  summarise(n = n()) 

### Autres formations de l'établissement

tab_eff_2 <- base_pamplemousse_2 %>% 
  filter(grepl("(MS|MSP|Mastère|Doct|STD|ext|ES|digi)", lib_voie, ignore.case = TRUE)) %>% 
  group_by(sexe, nationalite, filiere) %>% 
  summarise(n = n())


### Bourses

tab_eff_3 <- base_pamplemousse_2 %>% 
  filter(!bourse_montant =='') %>% 
  group_by(sexe, nationalite, filiere, bourse_org_attribution) %>% 
  summarise(n = n())


# Tri a plat sur la base ponderee ------



table(Annee2021_svy$SEXE_rec)
## 1. Tri à plat sexe ########
Annee2021_svy %>%
  tbl_svysummary(include = c("SEXE_rec"))
## 2. Tri à plat Actif de Annee2021_svy$NBACTIF en Annee2021_svy$NBACTIF_rec ########

Annee2021_svy %>% 
  tbl_svysummary(include = "NBACTIF_rec")
## 3. Tri à plat du Diplome et diplome du conjoint ########

Annee2021_svy %>% 
  tbl_svysummary(include = "DIPLOME_rec")
## 4. Tri à plat niveau d'étude ######

Annee2021_svy %>% 
  tbl_svysummary(include = "NIVETUDE_rec")

Annee2021_svy %>% 
  tbl_svysummary(include = "ETUDECONJ_rec")

## 5. Tri à plat situation économique ########

Annee2021_svy %>% 
  tbl_svysummary(include = "SITUAECO_rec")

## 6. Tri à plat Mois ####

Annee2021_svy %>% 
  tbl_svysummary(include = "MOISENQ_rec")
## 7.Tri à plat nb enfants moins 14 ans #####

Annee2021_svy %>% 
  tbl_svysummary(include = "NBENFANT_rec")
## 8. Création de la variable enfant total  #######

Annee2021_svy %>% 
  tbl_svysummary(include = "NBENFANTTOTAL_rec")


## 9. Tri à plat profession #######

Annee2021_svy %>% 
  tbl_svysummary(include = "PROFESSION")

## 10. Tri à plat profession conjoint et conjoint deja travaillé  #######

Annee2021_svy %>% 
  tbl_svysummary(include = "PROFCONJOINT")
freq(Annee2021_svy$DEJTRACJ_rec)

## 11. Temps complet ou partiel  #######
Annee2021_svy  %>% 
  tbl_svysummary(
    include = "QUOTITE2_rec")
freq(Annee2021_svy$QUOTITC2_rec)

## 12. Tri à plat Type d'emploi : CDD ou CDI ####
freq(Annee2021_svy$TYPEMPL2_rec)
Annee2021_svy  %>% 
  tbl_svysummary(
    include = "TYPEMPL2_rec")

## 13. Tri à plat Type d'emploi : CDD ou CDI ####

freq(Annee2021_svy$TYPEMPC2_rec)
Annee2021_svy  %>% 
  tbl_svysummary(
    include = "TYPEMPC2_rec")
## 14. Tri à plat nombre d'habitant dans le logement #######


freq(Annee2021_svy$NBPERS_rec)

## 15. Tri à plat de l'âge ########

Annee2021_svy %>%
  tbl_svysummary(
    include = "Age")


## 17. Tri à plat vie en couple : Vivez-vous en couple ?#######

freq(Annee2021_svy$CONJOINT_rec)

Annee2021_svy %>%
  tbl_svysummary(
    include = "Monoparental")

### 18. Création de la variable MONOPARENTALE : oui ou non ! 

Annee2021_svy %>%
  tbl_svysummary(
    include = "MONOPARENTALE")


## 18. Création variable familles monoparentale 2 avec des enfants aussi de plus de 14 ans #####
Annee2021_svy %>%
  tbl_svysummary(
    include = "Monoparental2")

# II. Variable dépenses et conjoncture ########

## 1. Tri à plat Achat logement ########
freq(Annee2021_svy$LOGEMENT_rec)
## 2. Tri à plat dépenses logement ####
freq(Annee2021_svy$DEPENSLO_rec)
## 3. Tri à plat Achat future de voiture #####
freq(Annee2021_svy$AUTO_rec)
## 4. Tri à plat dépenses future ####
freq(Annee2021_svy$DEPENSES_rec)
## 5. ACHATS #####
freq(Annee2021_svy$ACHATS_rec)
## 6. EPARGNER ####
freq(Annee2021_svy$EPARGNER_rec)

## 7. FINANCES ####
freq(Annee2021_svy$FINANCES_rec)

## 8. FINANPAS ####
freq(Annee2021_svy$FINANPAS_rec)

## 8. FINANFUT ####
freq(Annee2021_svy$FINANFUT_rec)



# ETAPE 2 : Tri à plat des six variables sur le prix ##########
freq(Annee2021_svy$PRIX_rec)
## 1. PRIX_rec : Variable qualitative : "Pensez-vous qu’au cours des douze derniers mois les prix ont…" ########

Annee2021_svy %>%
  tbl_svysummary(
    include = "PRIX_rec")
# 3 PRIXBAIS : Variable quantitative : De quel pourcentage pensez-vous que les prix ont baissé au cours des douze derniers mois ? #####
Annee2021_svy %>%
  tbl_svysummary(include = "PRIXBAIS_rec")
# 4. EVOLPRIX : variable qualitative : subjective sur le future !!  Par rapport aux douze derniers mois, quelle sera à votre avis l’évolution des prix au cours des douze prochains mois ?#####
### Evolution des prix
### Tableau 
Annee2021_svy %>% 
  tbl_svysummary(include ="EVOLPRIX_rec") 
# 5. Indice inflation 
# Annee2021_svy %>% 
#   tbl_svysummary(include ="IPC")

## ACM des pratiques ##"


## 1. Renommer variables ACM ------
### base -----
tt <- Annee2021

## renommer variable ------
list <- c("NBACTIF_rec", "NBENFANT_rec","NBENFANTTOTAL_rec","LOGEMENT_rec", "DEPENSLO_rec", "AUTO_rec", "DEPENSES_rec", "ACHATS_rec", "EPARGNER_rec", "FINANCES_rec", "FINANPAS_rec", "FINANFUT_rec")
for (v in list) {
  tt[[v]] <- paste0(v, sep="_", tt[[v]]) 
}

# 2. ACM des perceptions des prix -----
df1 <- tt[, c("PRIX_rec", "PRIXPLUS_rec",
               # variables éco 
              "MOISENQ_rec", "SITUAECO_rec",
               ## Variable pratiques 
               "LOGEMENT_rec", "DEPENSLO_rec", "AUTO_rec", "DEPENSES_rec", "ACHATS_rec", "EPARGNER_rec", "FINANCES_rec", "FINANPAS_rec", "FINANFUT_rec", 
                     ## Variable socio-demo 
                     "SEXE_rec","NBACTIF_rec", "DIPLOME_rec", "NIVETUDE_rec","PROFESSION","Age", "MONOPARENTALE")]
acm <- MCA(df1, quali.sup = 3:20)
explor(acm)
freq(tt$DEPENSLO_rec)

## ACM changement des pratique par rapport à l'inflation 



res <- explor::prepare_results(res)
explor::MCA_var_plot(res, xax = 1, yax = 3, var_sup = TRUE, var_sup_choice = c("PCS_MERE",
                                                                               "GENRE_EGO", "REVFOYER_rec", "nombre_enfants_total", "GENRE_KISH"), var_lab_min_contrib = 0,
                     col_var = "Variable", symbol_var = "Type", size_var = NULL, size_range = c(10,
                                                                                                300), labels_size = 9, point_size = 65, transitions = TRUE, labels_positions = "auto",
                     labels_prepend_var = FALSE, xlim = c(-1, 96, 2, 7), ylim = c(-1, 85, 2,
                                                                                  18))

## 2.3 ACM cuisine avec genre, âge et PCS de l'enfant -----
base_acm_2 <- base[, c("ENFREQCUISINE_rec", "PCS_EGO_rec", "AGE_KISH_TRANCHES", "GENRE_KISH")]
res_2 <- MCA(base_acm_2)
explor(res_2)

## On a un pb avec les NA de la PCS. 

## 2.4 ACM des pratiques avec genre, âge et PCS de l'enfant -----
base_acm_3 <- base[, c("ENFREQCUISINE_rec", "ENFREQCHAMBREREC_rec", "ENFREQJARDINREC_rec","ENFREQOCCUPREC_rec", "PCS_EGO_rec", "AGE_KISH_TRANCHES", "GENRE_KISH")]
res_3 <- MCA(base_acm_3)
explor(res_3) 

## ACM exploratoire 
res_3 <- explor::prepare_results(res_3)
explor::MCA_var_plot(res_3, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 5, col_var = "Variable", symbol_var = NULL, size_var = "Contrib",
                     size_range = c(52.5, 700), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-2, 14,
                                                                                     1, 93), ylim = c(-1, 47, 2, 6))


## 3. Manière de féliciter l'enfant ------

base_acm_4 <- base[, c("ENFSPONT_REC", "ENFCONV_REC", "ENFMERCI_REC","ENFFELICIT_REC", "AGE_KISH_TRANCHES", "GENRE_KISH", "PCS_MERE","PCS_PERE", "GENRE_EGO", "REVFOYER_rec","nombre_enfants_total")]
res_4 <- MCA(base_acm_4, quali.sup = 5:11)
explor(res_4) 

## Avoir une mère joue beaucoup dans le fait de ne jamais être félicité.e
## Avoir beaucoup d'enfants fait qu'on facilite plus ??? 
## Age de l'enfant joue beaucoup dans la manière de se faire féliciter. 



#ACM valentine

list <- c("ENFAUTRE", "ENFSANCTION",
          "ENFECRAN", "ENFJEU",
          "ENFARGENT", "ENFSORTIE",
          "ENFGROND","ENFSPONT_REC", "ENFCONV_REC", "ENFMERCI_REC","ENFFELICIT_REC")

for (v in list) {
  base[[v]] <- paste0(v, sep="_", base[[v]]) 
}

base_acm_3 <- base[, c("PCS_MERE", "PCS_PERE","AGE_KISH_TRANCHES", 
                       "GENRE_KISH", "NOMBRESANCTION",
                       "ENFAUTRE", "ENFSANCTION",
                       "ENFECRAN", "ENFJEU",
                       "ENFARGENT", "ENFSORTIE",
                       "ENFGROND", "ENFSPONT_REC", "ENFCONV_REC", "ENFMERCI_REC","ENFFELICIT_REC")]
res_3 <- MCA(base_acm_3, quali.sup=1:4, quanti.sup=5)
explor(res_3)

## Sanctions

base_acm_3 <- base[, c("GENRE_EGO", "REVFOYER_rec","nombre_enfants_total", "PCS_MERE", "PCS_PERE","AGE_KISH_TRANCHES", "GENRE_KISH", 
                       "NOMBRESANCTION",
                       "ENFAUTRE", "ENFSANCTION",
                       "ENFECRAN", "ENFJEU",
                       "ENFARGENT", "ENFSORTIE",
                       "ENFGROND", "ENFSPONT_REC", "ENFCONV_REC", "ENFMERCI_REC","ENFFELICIT_REC")]

res_3 <- MCA(base_acm_3, quali.sup=1:8)
explor(res_3)
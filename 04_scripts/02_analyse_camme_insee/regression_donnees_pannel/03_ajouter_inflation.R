
# Ajouter l'inflation --------------

df <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v3.csv")



# Inflation -----------------------------
inflation_values <- fread("02_data/inflation/Inflation_OCDE_1945_2023.csv")


# Prendre certaines varibales 
inflation_values <- inflation_values %>%
  select(TIME_PERIOD, OBS_VALUE)
inflation_values[, `:=`(
  ANENQ = as.numeric(substr(TIME_PERIOD, 1, 4)),
  MOISENQ = as.numeric(substr(TIME_PERIOD, 6, 7))
)]

# Rename OBS_VALUE to IPC
setnames(inflation_values, "OBS_VALUE", "IPC")

# Sélectionner uniquement les colonnes nécessaires dans inflation_values
inflation_values <- inflation_values %>%
  select(ANENQ, MOISENQ, IPC)

# Fusionner les deux dataframes en utilisant left_join

df <- df %>%
  left_join(inflation_values, by = c("ANENQ", "MOISENQ"))

# df$IDENT <- df$`IDENT,ANENQ` 
# df$`IDENT,ANENQ`  <- NULL
# df$`ISCOCJ,ANENQ` <- NULL
# df$`ident2,ANENQ` <- NULL
# df$`CODE_CS_CJ_R,ANENQ` <- NULL
# df$`ident2,ANENQ` <- NULL


# # Extract the IPC column from merged_df
# IPC <- merged_df$IPC
# 
# # Combine df_cleaned and IPC using cbind
# df_final <- cbind(df_cleaned, IPC)



# Identifier les colonnes doubles -----------------
# library(data.table)
# 
# df <- df[, 1:101, with = FALSE]
# 
# # Fonction pour renommer les colonnes en double avec "_v2" à la fin
# rename_duplicate_columns <- function(dt) {
#   cols <- names(dt)
#   duplicated_columns <- cols[duplicated(cols)]
#   for (col in duplicated_columns) {
#     indices <- which(cols == col)
#     for (i in seq_along(indices)) {
#       if (i > 1) {
#         new_name <- paste0(cols[indices[i]], "_v2")
#         setnames(dt, old = cols[indices[i]], new = new_name)
#       }
#     }
#   }
# }
# 
# # Renommer les colonnes en double dans df
# rename_duplicate_columns(df)


# # Sélectionner uniquement les colonnes nécessaires dans inflation_values
# inflation_values <- inflation_values[, .(ANENQ, MOISENQ, IPC)]
# 
# # Fusionner les deux data.tables
# df <- merge(df, inflation_values, by = c("ANENQ", "MOISENQ"), all.x = TRUE)

# Save the cleaned DataFrame
write.csv(df,"C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v4.csv", row.names = FALSE)

# Enquête emploi ------------

## indiv221 ---------------
emploi <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/enquete emploi/lil-1619b.csv/Csv/indiv221.csv")
View(emploi)

emploi$ESEG_2_Y
freq(emploi$ESEG_2_Y)

emploi$NAFANTG021N[]


emploi_info <- subset(emploi, emploi$NAFANTG021N =="J")

class(emploi$NAFANTG021N)
freq(!is.na(emploi$NAFANTG021N))

# Code 0: Fases  ---------------------------------------
# 1 Load packages -----------------------------------------------------
pacman::p_load(tidyverse, stringi,googlesheets4)

# 2 Load data ---------------------------------------------------------
## Cargar datos cuarentenas
datapaso <- read_sheet("https://docsgooglecom/spreadsheets/d/1oLhUR9jflNbAo8kXaQ-jdrElDSQcDEb2nXmxRlFUagg/edit?usp=sharing",
                   sheet = "Paso a Paso 2020")

# 3 Lockdown structure data -----------------------------------------
# Long format 
paso_long <- gather(datapaso, dia, paso,
                            colnames(datapaso)[2]:tail(colnames(datapaso),n=1),
                            factor_key=F, -c(CUT,COMUNA))

# Create dates
paso_long <- paso_long %>% separate(dia, sep ="-", c("day","month")) %>% 
  mutate(year=2020,
         month=tolower(month),
         month=ifelse(month=="mar",3,
                      ifelse(month=="abr",4,
                             ifelse(month=="may",5,
                                    ifelse(month=="jun",6,
                                           ifelse(month=="jul",7,
                                                  ifelse(month=="aug",8,
                                                         ifelse(month=="sep",9,
                                                                ifelse(month=="oct",10,
                                                                       ifelse(month=="nov",11,
                                                                              ifelse(month=="dic",12,NA)))))))))),
         date=lubridate::make_date(year, month, day),
         semana=lubridate::week(date)) %>%
  select(-day,-month,-year,comuna=COMUNA, fase=paso)

# Reorder and slice
paso_long <- paso_long %>%
  group_by(comuna, semana) %>%
  arrange(desc(date)) %>% 
  slice(1) %>%
  ungroup()

# Data by week, and lag of lockdown  to explore delay effects
paso_longwk <- paso_long  %>%
  group_by(comuna)  %>%
  arrange(comuna,date) %>% 
  select(-date) %>%
  group_by(comuna, semana) %>% 
  summarise_all(funs(mean), narm = TRUE) %>%                                         
  mutate(fasewk = fase,
         fasewk_lag1 = lag(fase, n=1),
         fasewk_lag2 = lag(fase, n=2),  
         fasewk_lag3 = lag(fase, n=3),
         fasewk_lag3 = lag(fase, n=4)) %>% 
  select(-fase, -CUT, -REG) %>% 
  ungroup()

# Merge between logn and wide lockdowns data
pasos <- left_join(paso_long, paso_longwk, by=c("comuna", "semana")) 

# Homogenize variables to merge with movid data
pasos <- pasos %>% 
  mutate(date = as.Date(date),
         CUT = as.numeric(CUT))

pasos$comuna <- chartr('áéíóúñü','aeiounu', pasos$comuna)
pasos$comuna <- chartr('ÁÉÍÓÚÑ','AEIOUN', pasos$comuna)
pasos$comuna <- ifelse(pasos$comuna=="O'Higgins", "OHiggins", pasos$comuna)
pasos$comuna <- ifelse(pasos$comuna=="Padre Las Casas", "Padre las Casas", pasos$comuna)
pasos$comuna <- tolower(stringi::stri_trans_general(pasos$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula

pasos$comuna <- tolower(stringi::stri_trans_general(pasos$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula


# 4 Export --------------------------------------------------------
lockdowns_movid_impact <- pasos; remove(pasos) 
saveRDS(lockdowns_movid_impact, "output/data/lockdowns_movid_impact.RDS")


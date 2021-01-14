# Code 1: Lockdowns ---------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyr, dplyr, lubridate, readxl, stringi)

# 2. Load data ---------------------------------------------------------
## Cargar datos cuarentenas
data.cuarentena <- read_excel("input/data/CUT_CUARENTENAS_COVID.xls")
data.cuarentena <- data.cuarentena[,-1]


# 3. Lockdown structure data -----------------------------------------
# Long format y dates
cuarentena_long <- tidyr:::gather(data.cuarentena, dia, cuarentena,colnames(data.cuarentena)[2]:tail(colnames(data.cuarentena),n=1), factor_key=F)
cuarentena_long <- cuarentena_long %>% separate(dia, sep ="-", c("day","month")) %>% 
  mutate(year=2020,
         month=tolower(month),
         month=ifelse(month=="mar.",3,
                      ifelse(month=="abr.",4,
                             ifelse(month=="may.",5,
                                    ifelse(month=="jun.",6,
                                           ifelse(month=="jul.",7,
                                                  ifelse(month=="aug.",8,
                                                         ifelse(month=="sep.",9,
                                                                ifelse(month=="oct.",10,
                                                                       ifelse(month=="nov.",11,
                                                                              ifelse(month=="dic.",12,NA)))))))))),
         date=lubridate::make_date(year, month, day),
         semana=lubridate::week(date))

cuarentena_long <- cuarentena_long %>% dplyr::select(-day,-month,-year)

cuarentena_long <- cuarentena_long %>%
  group_by(comuna, semana) %>%
  arrange(desc(date)) %>% 
  slice(1) %>%
  ungroup

names(cuarentena_long)[names(cuarentena_long) == "cuarentena"] <- "fase" 

# Data by week, and lag of lockdown  to explore delay effects
cuarentena_longwk <- cuarentena_long  %>% group_by(comuna)  %>% arrange(comuna,date) %>% 
  dplyr::select(-date) %>% group_by(comuna, semana) %>% 
  summarise_all(funs(mean), na.rm = TRUE) %>%                                         
  mutate(fasewk = fase,
         fasewk_lag1 = lag(fase, n=1),
         fasewk_lag2 = lag(fase, n=2),  
         fasewk_lag3 = lag(fase, n=3),
         fasewk_lag3 = lag(fase, n=4)) %>% 
  dplyr::select(-fase)

# Merge between logn and wide lockdowns data
cuarentenas <- left_join(cuarentena_long, cuarentena_longwk, by=c("comuna", "semana"))

# Homogenize variables to merge with movid data
cuarentenas$fecha <- as.Date(cuarentenas$date)

cuarentenas$comuna <- chartr('áéíóúñü','aeiounu', cuarentenas$comuna)
cuarentenas$comuna <- chartr('ÁÉÍÓÚÑ','AEIOUN', cuarentenas$comuna)
cuarentenas$comuna <- ifelse(cuarentenas$comuna=="O'Higgins", "OHiggins", cuarentenas$comuna)
cuarentenas$comuna <- ifelse(cuarentenas$comuna=="Padre Las Casas", "Padre las Casas", cuarentenas$comuna)
cuarentenas$comuna <- tolower(stringi::stri_trans_general(cuarentenas$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula

cuarentenas$fecha_ymd <- as.Date(cuarentenas$date)
cuarentenas$comuna <- tolower(stringi::stri_trans_general(cuarentenas$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula
cuarentenas <- cuarentenas[,-9]


# 4. Export --------------------------------------------------------
lockdowns <- cuarentenas; remove(cuarentenas) 
saveRDS(lockdowns, "output/data/lockdowns.RDS")


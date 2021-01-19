# Code 3: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i <- haven::read_dta("input/data/210118_base_movid_version01.dta")

## Lockdowns
readRDS(lockdowns, "output/data/lockdowns.RDS")

# 3. Recodes -----------------------------------------------------

# ID and sociodemographic --------------------------------------
## Id
movid_i$id_pob <- as.numeric(as.factor(movid_i$id_encuesta))

## Sex 
movid_i$sexo <- car::recode(movid_i$sexo, c("1='Hombre';2='Mujer'"), as.factor = T,
                            levels = c("Hombre", "Mujer"))


# Age ---------------------------------------------------------------------

## Edad
str(movid_i$edad) # num

## Edad_3cat
movid_i$edad_3cat <- ifelse(movid_i$edad<40, "18 a 39",
                          ifelse(movid_i$edad<65 & movid_i$edad>39, "40 a 64",
                                 ifelse(movid_i$edad>64, "65 y más", NA)))

movid_i$edad_3cat <- as_factor(movid_i$edad_3cat)


# Education ---------------------------------------------------------------
# Education: 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- as.numeric(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$educ_3cat, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                               levels = c("Media o menos", "Profesional", 'Técnica'))

table(movid_i$educ_3cat)

# Worker ------------------------------------------------------------------
movid_i$trabaja <- ifelse(movid_i$pr3_ocupacion=="Trabaja de manera remunerada",1,0)
# Worker (g1 or/type g10)

# Health risk -------------------------------------------------------------
# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies

## Health risk General
table(movid_i$c1_8)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 2 ~ 1,
                                                   c1_3 == 3 ~ 1,
                                                   c1_4 == 4 ~ 1,
                                                   c1_5 == 5 ~ 1,
                                                   c1_6_esp == "artritis" ~ 1,
                                                   c1_7 == 7 ~ 1,
                                                   c1_8 == 8 ~ NA_real_,
                                                   c1_9 ==9 ~ NA_real_,
                                                   TRUE ~ 0))

table(movid_i$cronicos) ## Artritis
178+339+48+91+161+567+7
# Health insurance --------------------------------------------------------

## Prev General
table(movid_i$b2)
movid_i$pr2_prevision <- as.numeric(movid_i$b2)
movid_i$pr2_prevision <- car::recode(movid_i$pr2_prevision, c("1='FONASA';2='ISAPRE';3='Fuerzas Armadas y de Orden';4='Otr0';5='Ninguna';c(8,9)=NA"), as.factor = T,
                                 levels = c("FONASA", "ISAPRE", 'Fuerzas Armadas y de Orden', 'Otro', 'Ninguna' ))

table(movid_i$pr2_prevision)

## Prev recodificaciones
movid_i$prev_2categ <- as.factor(ifelse(movid_i$pr2_prevision=="FONASA",0,
                                        ifelse(movid_i$pr2_prevision=="ISAPRE",1,2)))
levels(movid_i$prev_2categ) <- c("FONASA","ISAPRE", "Otro")


movid_i$prev_4categ <- as.factor(ifelse(movid_i$pr2_prevision=="Ninguna",0,
                                        ifelse(movid_i$pr2_prevision=="FONASA",1,
                                               ifelse(movid_i$pr2_prevision=="ISAPRE",2,3))))
levels(movid_i$prev_4categ) <- c("Ninguna","FONASA","ISAPRE", "Otro")


# Lack income due COVID ---------------------------------------------------

# Residency ------------------------------------- -------------------------
## Comuna ------------------------------------------------------------------
movid_i$comuna <- chartr('áéíóúñü','aeiounu', movid_i$comuna)
movid_i$comuna <- chartr('ÁÉÍÓÚÑ','AEIOUN', movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="O'Higgins", "OHiggins", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="Padre Las Casas", "Padre las Casas", movid_i$comuna)
movid_i$comuna <- tolower(stringi::stri_trans_general(movid_i$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula
movid_i$comuna <- ifelse(movid_i$comuna=="aysen", "aisen", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="coyhaique", "coihaique", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="la calera", "calera", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="llay-llay", "llaillay", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="paihuano", "paiguano", movid_i$comuna)
movid_i$comuna <- ifelse(movid_i$comuna=="til til", "tiltil", movid_i$comuna)


# Lockdown ----------------------------------------------------------------

##Week
## Aquí debo crear variable para ver semana del encuestado, asi agregar cuarentena ------------
movid_i$semana <- ifelse(movid_i$semana==15, 16, movid_i$semana)
movid_i$semana0 <- ifelse(movid_i$semana0==-1, 0, movid_i$semana0)






# Models variables -----------------------------------------------------

## 1.  Compliance
###Aunque a veces no estemos de acuerdo con las autoridades sanitarias y las medidas que se proponen, es nuestro deber seguir sus indicaciones al pie de la letra.
### En la última semana, ¿con qué frecuencia ha realizado las siguientes acciones para protegerse del coronavirus? Ud. me debe decir si las ha realizado Casi nunca, A veces, Frecuentemente, Casi siempre o Siempre

## Dic Salidas
#movid_i$salidas_dic <- ifelse(movid_i$salidas>2,1,0)

## 2. Sociodemographic
# Sex
# Age
# Education: 3 categories: High school or less, Technical qualification and University degree
# Worker (g1 or/type g10)
# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
# Lack income due COVID
# Residency
# Lockdown

## 3. Instrumental factors
### A. Perived risk (f6)
### ¿Qué tan peligroso cree que es el coronavirus para usted y sus cercanos?

## Riesgo ordinal
movid_i$per_riesgo_ord <- ifelse(movid_i$per_riesgo=="Muy en desacuerdo",1,
                               ifelse(movid_i$per_riesgo=="En desacuerdo",2,
                                      ifelse(movid_i$per_riesgo=="Ni de acuerdo ni en desacuerdo",3,
                                             ifelse(movid_i$per_riesgo=="De acuerdo",4,
                                                    ifelse(movid_i$per_riesgo=="Muy de acuerdo",5,NA)))))
### Niveles de riesgo
movid_i$alto_riesgo <- ifelse(movid_i$per_riesgo=="Muy de acuerdo" | movid_i$per_riesgo=="De acuerdo",1,0)
movid_i$bajo_riesgo <- ifelse(movid_i$per_riesgo=="Muy en desacuerdo" | movid_i$per_riesgo=="En desacuerdo",1,0)

## B. Legal enforcement  (f5.5)
### En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado.


## 4. Normative factors (f8)
### A. Perceived social norms: Pensando en distintas medidas de cuidado ante el coronavirus (quedarse en casa, usar     mascarilla, mantener distanciamiento social o lavarse las manos). ¿En qué medida diría Ud.     que su círculo cercano (personas que viven con Ud. o su familia cercana) cumple estas     recomendaciones?

## Normas
movid_i$cumple_normas <- ifelse(movid_i$normas=="Completamente" | movid_i$normas=="En gran medida",1,0)
movid_i$nocumple_normas <- ifelse(movid_i$normas=="Nada" | movid_i$normas=="Poco" | movid_i$normas=="Algo",1,0)

### B. Legitimacy (f3.3)
###Aunque a veces no estemos de acuerdo con las autoridades sanitarias y las medidas que se proponen, es nuestro deber seguir sus indicaciones al pie de la letra.
###Acuerdo

# 4.Merge data ------------------------------------------------------------
movid_i_proc <- movid_i; remove(movid_i)
movid_i <- left_join(movid_i_proc, lockdowns, by=c("comuna", "semana"))

# 5. Save  -----------------------------------------------------------------
saveRDS(lockdowns, "output/data/movid_i_proc.rds")
save(movid_i, file = "output/data/movid_i.RData")

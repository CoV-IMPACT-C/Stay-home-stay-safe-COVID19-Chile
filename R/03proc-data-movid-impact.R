# Code 3: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i <- read.csv("input/data/movid_impact.csv", sep=",", 
                  encoding = "UTF-8", stringsAsFactors = F )

## Lockdowns
readRDS(lockdowns, "output/data/lockdowns.RDS")

# 3. Recodes -----------------------------------------------------

# ID and sociodemographic --------------------------------------

## Rename
names(movid_i)[names(movid_i) == "fecha_obs"] <- "fecha"
movid_i$fecha_ymd <- as.Date(movid_i$fecha)
names(movid_i)[names(movid_i) == "a2"] <- "sexo"
names(movid_i)[names(movid_i) == "a3"] <- "edad"
names(movid_i)[names(movid_i) == "a4"] <- "jefhogar"
names(movid_i)[names(movid_i) == "a5"] <- "resp_jefhogar"
names(movid_i)[names(movid_i) == "a5"] <- "rel_jefhogar"
names(movid_i)[names(movid_i) == "a6"] <- "rel_jefhogar"
names(movid_i)[names(movid_i) == "a7"] <- "ecivil"
names(movid_i)[names(movid_i) == "a5"] <- "rel_jefhogar"
names(movid_i)[names(movid_i) == "a5"] <- "rel_jefhogar"
names(movid_i)[names(movid_i) == "a5"] <- "rel_jefhogar"




names(movid_i)[names(movid_i) == "r1_nombre"] <- "firstName"
names(movid_i)[names(movid_i) == "u1_region"] <- "region"
names(movid_i)[names(movid_i) == "u2_comuna"] <- "comuna"
names(movid_i)[names(movid_i) == "u3_calle"] <- "calle"
names(movid_i)[names(movid_i) == "r5_educ"] <- "educ"
names(movid_i)[names(movid_i) == "pr1_wrk_salud"] <- "tra_salud"
names(movid_i)[names(movid_i) == "pr2_prevision"] <- "prev"
names(movid_i)[names(movid_i) == "X.U.FEFF.X.U.FEFF.pob_id"] <- "pob_id"

## Id
movid_i$id_pob <- as.numeric(as.factor(movid_i$pob_id))

## Sex 
movid_i$sexo <- ifelse(movid_i$sexo=="Otro",NA,movid_i$sexo)

## Educ
movid_i$edad_3cat <- ifelse(movid_i$edad<40, "18 a 39",
                          ifelse(movid_i$edad<65 & movid_i$edad>39, "40 a 64",
                                 ifelse(movid_i$edad>64, "65 y más", NA)))
## Week

## Aquí debo crear variable para ver semana del encuestado, asi agregar cuarentena ------------
movid_i$semana <- ifelse(movid_i$semana==15, 16, movid_i$semana)
movid_i$semana0 <- ifelse(movid_i$semana0==-1, 0, movid_i$semana0)

## Prev
movid_i$prev_2categ <- as.factor(ifelse(movid_i$pr2_prevision=="FONASA",0,
                                      ifelse(movid_i$pr2_prevision=="ISAPRE",1,2)))
levels(movid_i$prev_2categ) <- c("FONASA","ISAPRE", "Otro")


movid_i$prev_4categ <- as.factor(ifelse(movid_i$pr2_prevision=="Ninguna",0,
                                      ifelse(movid_i$pr2_prevision=="FONASA",1,
                                             ifelse(movid_i$pr2_prevision=="ISAPRE",2,3))))
levels(movid_i$prev_4categ) <- c("Ninguna","FONASA","ISAPRE", "Otro")

## Work
movid_i$trabaja <- ifelse(movid_i$pr3_ocupacion=="Trabaja de manera remunerada",1,0)


# Comuna ------------------------------------------------------------------
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

# Education ---------------------------------------------------------------
movid_i$educ_4cat <- ifelse(movid_i$educ=="Sin estudios" | movid_i$educ=="Educación Básica (primaria o preparatoria)", "Basica o sin estudios",
                          ifelse(movid_i$educ == "Educación Media (Humanidades)", "Media",
                                 ifelse(movid_i$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                        ifelse(movid_i$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Tecnica", NA))))
movid_i$educ_3cat <- ifelse(movid_i$educ=="Sin estudios" | movid_i$educ=="Educación Básica (primaria o preparatoria)" | movid_i$educ == "Educación Media (Humanidades)", "Media o menos",
                          ifelse(movid_i$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                 ifelse(movid_i$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Técnica", NA)))

movid_i$educ_2cat <- ifelse(movid_i$educ=="Sin estudios" | movid_i$educ=="Educación Básica (primaria o preparatoria)" | movid_i$educ == "Educación Media (Humanidades)", "Media o menos",
                          ifelse(movid_i$educ == "Educación Profesional (Carreras de 4 o más años)" | movid_i$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Más que media", NA))

# Income and Work ------------------------------------------------------
movid_i$tertil_ingre_c <- ifelse(movid_i$tertil_ingre==1, "Ingresos bajos", 
                               ifelse(movid_i$tertil_ingre==2, "Ingresos medios",
                                      ifelse(movid_i$tertil_ingre==3, "Ingresos altos", NA)))

# Practices ---------------------------------------------------------------
movid_i$dic_trabajo <- ifelse(movid_i$p1_pra_trabajo==0, 0,
                            ifelse(movid_i$p1_pra_trabajo>0, 1, NA))

movid_i$dic_tramite <- ifelse(movid_i$p1_pra_tramite==0, 0,
                            ifelse(movid_i$p1_pra_tramite>0, 1, NA))

movid_i$dic_visita <- ifelse(movid_i$p1_pra_visita==0, 0,
                           ifelse(movid_i$p1_pra_visita>0, 1, NA))

movid_i$dic_recrea <- ifelse(movid_i$p1_pra_recrea==0, 0,
                           ifelse(movid_i$p1_pra_recrea>0, 1, NA))

movid_i$dic_transporte <- ifelse(movid_i$p1_pra_transporte==0, 0,
                               ifelse(movid_i$p1_pra_transporte>0, 1, NA))

movid_i$dic_invitado <- ifelse(movid_i$p1_pra_invitado==0, 0,
                             ifelse(movid_i$p1_pra_invitado>0, 1, NA))

movid_i$dic_otro <- ifelse(movid_i$p1_pra_otro==0, 0,
                         ifelse(movid_i$p1_pra_otro>0, 1, NA))

movid_i$dic_protesta <- ifelse(movid_i$p1_pra_protesta==0, 0,
                             ifelse(movid_i$p1_pra_protesta>0, 1, NA))

movid_i$dic_practicas <- ifelse((movid_i$dic_trabajo==0 & movid_i$dic_tramite==0 & movid_i$dic_invitado==0 &
                                 movid_i$dic_recrea==0 & movid_i$dic_transporte==0 & movid_i$dic_visita==0), 0,
                              ifelse((movid_i$dic_trabajo>0 | movid_i$dic_tramite>0 | movid_i$dic_invitado>0 |
                                        movid_i$dic_recrea>0 | movid_i$dic_transporte>0 | movid_i$dic_visita>0), 1, NA))

movid_i$n_salidas <- (movid_i$p1_pra_trabajo+movid_i$p1_pra_recrea+movid_i$p1_pra_tramite+movid_i$p1_pra_visita)


# Speciall practices -----------------------------------------------------------------

# Crear variable dummy de trabajar
movid_i$trabaja <- ifelse(movid_i$pr3_ocupacion== 'Trabaja de manera remunerada',1,0)

# Crear nsalidas y trabaja
movid_i <- movid_i %>% mutate(p1_pra_otro = ifelse(is.na(p1_pra_otro),0,1),
                          salidas= p1_pra_trabajo + p1_pra_tramite + p1_pra_recrea + p1_pra_visita + p1_pra_otro,
                          trabaja = ifelse(is.na(trabaja),0,trabaja), # correcion trabajo
                          tra_salud = ifelse(is.na(tra_salud),0,tra_salud), # correcion tra_salud
                          p3_transp_publico = ifelse(is.na(p3_transp_publico),0,p3_transp_publico))


# Sintomas ----------------------------------------------------------------
movid_i$sintoma <- ifelse((movid_i$s1_snt_fiebre==1 | movid_i$s1_snt_anosmia==1 | movid_i$s1_snt_disnea==1 | movid_i$s1_snt_tos==1 |
                           movid_i$s1_snt_mialgias==1 | movid_i$s1_snt_odinofagia==1 | movid_i$s1_snt_dol_torax==1 |
                           movid_i$s1_snt_cefalea==1 | movid_i$s1_snt_diarrea==1 | movid_i$s1_snt_disgeusia==1), 1,
                        ifelse((movid_i$s1_snt_fiebre==0 & movid_i$s1_snt_anosmia==0 & movid_i$s1_snt_disnea==0 & movid_i$s1_snt_tos==0 &
                                  movid_i$s1_snt_mialgias==0 & movid_i$s1_snt_odinofagia==0 & movid_i$s1_snt_dol_torax==0 &
                                  movid_i$s1_snt_cefalea==0 & movid_i$s1_snt_diarrea==0 & movid_i$s1_snt_disgeusia==0), 0, NA))
movid_i$sintoma <- ifelse(movid_i$s1_snt_null==1, 0, movid_i$sintoma)


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

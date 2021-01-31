# Code 2: Proc MOVID-19 ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(dplyr)

# 2. Load data  -------------------------------------------
## MOVID-19
movid <- read.csv("input/data/movid19.csv", sep=",", 
                  encoding = "UTF-8", stringsAsFactors = F )

## Lockdowns
lockdowns <- readRDS("output/data/lockdowns_movid.RDS")

# 3. Recodes -----------------------------------------------------

# ID and sociodemographic --------------------------------------

## Rename
names(movid)[names(movid) == "fecha_obs"] <- "fecha"
movid$fecha_ymd <- as.Date(movid$fecha)
names(movid)[names(movid) == "r2_sexo"] <- "sexo"
names(movid)[names(movid) == "r1_nombre"] <- "firstName"
names(movid)[names(movid) == "u1_region"] <- "region"
names(movid)[names(movid) == "u2_comuna"] <- "comuna"
names(movid)[names(movid) == "u3_calle"] <- "calle"
names(movid)[names(movid) == "r5_educ"] <- "educ"
names(movid)[names(movid) == "pr1_wrk_salud"] <- "tra_salud"
names(movid)[names(movid) == "pr2_prevision"] <- "prev"
names(movid)[names(movid) == "X.U.FEFF.X.U.FEFF.pob_id"] <- "pob_id"

## Id
movid$id_pob <- as.numeric(as.factor(movid$pob_id))

## Sex
movid$sexo <- ifelse(movid$sexo=="Otro",NA,movid$sexo)

## Educ
movid$edad_3cat <- ifelse(movid$edad<40, "18 a 39",
                          ifelse(movid$edad<65 & movid$edad>39, "40 a 64",
                                 ifelse(movid$edad>64, "65 y más", NA)))
## Week
movid$semana <- ifelse(movid$semana==15, 16, movid$semana)
movid$semana0 <- ifelse(movid$semana0==-1, 0, movid$semana0)

## Prev

movid$prev_2categ <- as.factor(ifelse(movid$pr2_prevision=="FONASA",0,
                                      ifelse(movid$pr2_prevision=="ISAPRE",1,2)))
levels(movid$prev_2categ) <- c("FONASA","ISAPRE", "Otro")


movid$prev_4categ <- as.factor(ifelse(movid$pr2_prevision=="Ninguna",0,
                                      ifelse(movid$pr2_prevision=="FONASA",1,
                                             ifelse(movid$pr2_prevision=="ISAPRE",2,3))))
levels(movid$prev_4categ) <- c("Ninguna","FONASA","ISAPRE", "Otro")

## Work
movid$trabaja <- ifelse(movid$pr3_ocupacion=="Trabaja de manera remunerada",1,0)


# Comuna ------------------------------------------------------------------
movid$comuna <- chartr('áéíóúñü','aeiounu', movid$comuna)
movid$comuna <- chartr('ÁÉÍÓÚÑ','AEIOUN', movid$comuna)
movid$comuna <- ifelse(movid$comuna=="O'Higgins", "OHiggins", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="Padre Las Casas", "Padre las Casas", movid$comuna)
movid$comuna <- tolower(stringi::stri_trans_general(movid$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula
movid$comuna <- ifelse(movid$comuna=="aysen", "aisen", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="coyhaique", "coihaique", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="la calera", "calera", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="llay-llay", "llaillay", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="paihuano", "paiguano", movid$comuna)
movid$comuna <- ifelse(movid$comuna=="til til", "tiltil", movid$comuna)

# Education ---------------------------------------------------------------
movid$educ_4cat <- ifelse(movid$educ=="Sin estudios" | movid$educ=="Educación Básica (primaria o preparatoria)", "Basica o sin estudios",
                          ifelse(movid$educ == "Educación Media (Humanidades)", "Media",
                                 ifelse(movid$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                        ifelse(movid$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Tecnica", NA))))
movid$educ_3cat <- ifelse(movid$educ=="Sin estudios" | movid$educ=="Educación Básica (primaria o preparatoria)" | movid$educ == "Educación Media (Humanidades)", "Media o menos",
                          ifelse(movid$educ == "Educación Profesional (Carreras de 4 o más años)", "Profesional",
                                 ifelse(movid$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Técnica", NA)))

movid$educ_2cat <- ifelse(movid$educ=="Sin estudios" | movid$educ=="Educación Básica (primaria o preparatoria)" | movid$educ == "Educación Media (Humanidades)", "Media o menos",
                          ifelse(movid$educ == "Educación Profesional (Carreras de 4 o más años)" | movid$educ == "Educación Técnica Nivel Superior (Carreras de 1 a 3 años)", "Más que media", NA))

# Income and Work ------------------------------------------------------
movid$tertil_ingre_c <- ifelse(movid$tertil_ingre==1, "Ingresos bajos", 
                               ifelse(movid$tertil_ingre==2, "Ingresos medios",
                                      ifelse(movid$tertil_ingre==3, "Ingresos altos", NA)))

# Practices ---------------------------------------------------------------
movid$dic_trabajo <- ifelse(movid$p1_pra_trabajo==0, 0,
                            ifelse(movid$p1_pra_trabajo>0, 1, NA))

movid$dic_tramite <- ifelse(movid$p1_pra_tramite==0, 0,
                            ifelse(movid$p1_pra_tramite>0, 1, NA))

movid$dic_visita <- ifelse(movid$p1_pra_visita==0, 0,
                           ifelse(movid$p1_pra_visita>0, 1, NA))

movid$dic_recrea <- ifelse(movid$p1_pra_recrea==0, 0,
                           ifelse(movid$p1_pra_recrea>0, 1, NA))

movid$dic_transporte <- ifelse(movid$p1_pra_transporte==0, 0,
                               ifelse(movid$p1_pra_transporte>0, 1, NA))

movid$dic_invitado <- ifelse(movid$p1_pra_invitado==0, 0,
                             ifelse(movid$p1_pra_invitado>0, 1, NA))

movid$dic_otro <- ifelse(movid$p1_pra_otro==0, 0,
                         ifelse(movid$p1_pra_otro>0, 1, NA))

movid$dic_protesta <- ifelse(movid$p1_pra_protesta==0, 0,
                             ifelse(movid$p1_pra_protesta>0, 1, NA))

movid$dic_practicas <- ifelse((movid$dic_trabajo==0 & movid$dic_tramite==0 & movid$dic_invitado==0 &
                                 movid$dic_recrea==0 & movid$dic_transporte==0 & movid$dic_visita==0), 0,
                              ifelse((movid$dic_trabajo>0 | movid$dic_tramite>0 | movid$dic_invitado>0 |
                                        movid$dic_recrea>0 | movid$dic_transporte>0 | movid$dic_visita>0), 1, NA))

movid$n_salidas <- (movid$p1_pra_trabajo+movid$p1_pra_recrea+movid$p1_pra_tramite+movid$p1_pra_visita)


# Speciall practices -----------------------------------------------------------------

# Crear variable dummy de trabajar
movid$trabaja <- ifelse(movid$pr3_ocupacion== 'Trabaja de manera remunerada',1,0)

# Crear nsalidas y trabaja
movid <- movid %>% mutate(p1_pra_otro = ifelse(is.na(p1_pra_otro),0,1),
                                    salidas= p1_pra_trabajo + p1_pra_tramite + p1_pra_recrea + p1_pra_visita + p1_pra_otro,
                                    trabaja = ifelse(is.na(trabaja),0,trabaja), # correcion trabajo
                                    tra_salud = ifelse(is.na(tra_salud),0,tra_salud), # correcion tra_salud
                                    p3_transp_publico = ifelse(is.na(p3_transp_publico),0,p3_transp_publico))


# Sintomas ----------------------------------------------------------------
movid$sintoma <- ifelse((movid$s1_snt_fiebre==1 | movid$s1_snt_anosmia==1 | movid$s1_snt_disnea==1 | movid$s1_snt_tos==1 |
                           movid$s1_snt_mialgias==1 | movid$s1_snt_odinofagia==1 | movid$s1_snt_dol_torax==1 |
                           movid$s1_snt_cefalea==1 | movid$s1_snt_diarrea==1 | movid$s1_snt_disgeusia==1), 1,
                        ifelse((movid$s1_snt_fiebre==0 & movid$s1_snt_anosmia==0 & movid$s1_snt_disnea==0 & movid$s1_snt_tos==0 &
                                  movid$s1_snt_mialgias==0 & movid$s1_snt_odinofagia==0 & movid$s1_snt_dol_torax==0 &
                                  movid$s1_snt_cefalea==0 & movid$s1_snt_diarrea==0 & movid$s1_snt_disgeusia==0), 0, NA))
movid$sintoma <- ifelse(movid$s1_snt_null==1, 0, movid$sintoma)


# Models variables -----------------------------------------------------
## Dic Salidas
movid$salidas_dic <- ifelse(movid$salidas>2,1,0)
## Riesgo ordinal
movid$per_riesgo_ord <- ifelse(movid$per_riesgo=="Muy en desacuerdo",1,
                              ifelse(movid$per_riesgo=="En desacuerdo",2,
                                     ifelse(movid$per_riesgo=="Ni de acuerdo ni en desacuerdo",3,
                                            ifelse(movid$per_riesgo=="De acuerdo",4,
                                                   ifelse(movid$per_riesgo=="Muy de acuerdo",5,NA)))))
### Niveles de riesgo
movid$alto_riesgo <- ifelse(movid$per_riesgo=="Muy de acuerdo" | movid$per_riesgo=="De acuerdo",1,0)
movid$bajo_riesgo <- ifelse(movid$per_riesgo=="Muy en desacuerdo" | movid$per_riesgo=="En desacuerdo",1,0)

## Normas
movid$cumple_normas <- ifelse(movid$normas=="Completamente" | movid$normas=="En gran medida",1,0)
movid$nocumple_normas <- ifelse(movid$normas=="Nada" | movid$normas=="Poco" | movid$normas=="Algo",1,0)


# 4.Merge data ------------------------------------------------------------
movid_proc <- movid; remove(movid)
movid <- left_join(movid_proc, lockdowns, by=c("comuna", "semana"))

# 5. Save  -----------------------------------------------------------------
saveRDS(movid, "output/data/movid_proc.RDS")

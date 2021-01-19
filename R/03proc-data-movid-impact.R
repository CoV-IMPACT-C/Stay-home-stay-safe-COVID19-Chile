# Code 3: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i <- haven::read_dta("input/data/210118_base_movid_version01.dta")

## Lockdowns
lockdowns <- readRDS("output/data/lockdowns.RDS")

# 3. Recodes -----------------------------------------------------

# 1.  Compliance (f7_*)----------------------------------------------------------
### En la última semana, ¿con qué frecuencia ha realizado las siguientes acciones para protegerse del coronavirus? Ud. me debe decir si las ha realizado Casi nunca, A veces, Frecuentemente, Casi siempre o Siempre
#- Ver Distribución
#- Opciones de colapsar
movid_i <- movid_i %>% mutate_at(vars(contains("f7_")), funs(as.numeric(.)))
table(movid_i$comp_wash, useNA ="ifany")
movid_i$comp_wash <- car::recode(movid_i$f7_1, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                            levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$comp_dist <- car::recode(movid_i$f7_2, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                                 levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$comp_soc <- car::recode(movid_i$f7_3, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                                 levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$comp_mask <- car::recode(movid_i$f7_4, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                                 levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

movid_i$comp_mask2 <- car::recode(movid_i$f7_5, c("1='Casi nunca';2='A veces';3='Frecuentemente';4='Casi siempre';5='Siempre';c(8,9)=NA"), as.factor = T,
                                 levels = c("Casi nunca", "A veces", "Frecuentemente", "Casi siempre", "Siempre"))

table(movid_i$f7_1)
table(movid_i$comp_wash)

# 2. Sociodemographic --------------------------------------
# 2.1 ID ------------------------------------------------------------------
movid_i$id_pob <- as.numeric(as.factor(movid_i$id_encuesta))

# 2.2 Sex -----------------------------------------------------------------
movid_i$sexo <- as.numeric(movid_i$sexo)
movid_i$sexo <- car::recode(movid_i$sexo, c("1='Hombre';2='Mujer'"), as.factor = T,
                            levels = c("Hombre", "Mujer"))
# 2.3 Age -----------------------------------------------------------------
## Edad
str(movid_i$edad) # num

## Edad_3cat
movid_i$edad_3cat <- ifelse(movid_i$edad<40, "18 a 39",
                          ifelse(movid_i$edad<65 & movid_i$edad>39, "40 a 64",
                                 ifelse(movid_i$edad>64, "65 y más", NA)))

movid_i$edad_3cat <- as_factor(movid_i$edad_3cat)


# 2.4 Education ---------------------------------------------------------------
# Education: 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- as.numeric(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$educ_3cat, c("c(1,2,3,4,5,6)='Media o menos';7='Técnica';c(8,9)='Profesional';99=NA"), as.factor = T,
                               levels = c("Media o menos", "Profesional", 'Técnica'))

table(movid_i$educ_3cat)

# 2.5 Worker ------------------------------------------------------------------
movid_i$trabaja <- as.numeric(movid_i$g1)
movid_i$trabaja <- car::recode(movid_i$trabaja, c("1='Si'; 2='No'"), as.factor = T)

# Worker (g1 or/type g10)

# 2.6 Health risk -------------------------------------------------------------
# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(movid_i$c1_9)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 2 ~ 1,
                                                   c1_3 == 3 ~ 1,
                                                   c1_4 == 4 ~ 1,
                                                   c1_5 == 5 ~ 1,
                                                   c1_6_esp == "artritis" ~ 1,
                                                   c1_7 == 7 ~ 1,
                                                   c1_8 == 8 ~ NA_real_,
                                                   c1_9 == 9 ~ NA_real_,
                                                   TRUE ~ 0))   

table(movid_i$cronicos) ## Artritis
178+339+48+91+161+567+7
# 2.7 Health insurance --------------------------------------------------------
## Prev General
table(movid_i$b2)
movid_i$pr2_prevision <- as.numeric(movid_i$b2)
movid_i$pr2_prevision <- car::recode(movid_i$pr2_prevision, c("1='FONASA';2='ISAPRE';3='Fuerzas Armadas y de Orden';4='Otro';5='Ninguna';c(8,9)=NA"), as.factor = T,
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

# 2.8 Lack income due COVID ---------------------------------------------------
movid_i$lack_income <- as.numeric(movid_i$g49)

movid_i$lack_income <- car::recode(movid_i$lack_income, c("1='Ha bajado';2='Se ha mantenido igual';3='Ha subido';c(8,9)=NA"), as.factor = T,
                                levels = c("Ha bajado", "Se ha mantenido igual", "Ha subido"))
table(movid_i$g49)
table(movid_i$lack_income)

# 2.9 Residency ------------------------------------- -------------------------
## Comuna ------------------------------------------------------------------
movid_i <- movid_i %>% mutate(comuna = as.numeric(comuna), region = as.numeric(region)) %>% 
  merge(chilemapas::codigos_territoriales, by.x= "comuna", by.y= "codigo_comuna", all.x = T)
table(movid_i$nombre_comuna)
## Se pegaron nombres

# 2.10 Lockdown ----------------------------------------------------------------
##Week
## Aquí debo crear variable para ver semana del encuestado
## asi agregar cuarentena 

# 3. Instrumental factors -------------------------------------------------
## Transform numeric
movid_i <- movid_i %>% mutate_at(vars(contains("f6"), "f5_5"), funs(as.numeric(.)))

# A. Perived risk (f6) ----------------------------------------------------
### ¿Qué tan peligroso cree que es el coronavirus para usted y sus cercanos?
movid_i$per_risk <- car::recode(movid_i$f6, c("1='Nada peligroso';2='Algo peligroso';3='Bastante peligroso';4='Muy peligroso';5='Extremadamente peligroso';c(8,9)=NA"), as.factor = T,
                                 levels = c("Nada peligroso", "Algo peligroso", "Bastante peligroso","Muy peligroso", "Extremadamente peligroso"))

table(movid_i$f6)
table(movid_i$per_risk)

### Niveles de riesgo
movid_i$high_risk <- ifelse(movid_i$per_risk=="Bastante peligroso" | movid_i$per_risk=="Muy peligroso"| movid_i$per_risk=="Extremadamente peligroso",1,0)
movid_i$low_risk <- ifelse(movid_i$per_risk=="Nada peligroso" | movid_i$per_risk=="Algo peligroso",1,0)

# B. Legal enforcement  (f5.5) --------------------------------------------
### En Chile, si una persona sale sin permiso durante una cuarentena es muy poco probable que sea controlado y multado.
movid_i$leg_enforce <- car::recode(movid_i$f5_5, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                 levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$leg_enforce)

# 4. Normative factors  -----------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("f8"), contains("f3")), funs(as.numeric(.)))

# A. Perceived social norms(f8) ----------------------------------------------
### Pensando en distintas medidas de cuidado ante el coronavirus (quedarse en casa, usar     mascarilla, mantener distanciamiento social o lavarse las manos). ¿En qué medida diría Ud.     que su círculo cercano (personas que viven con Ud. o su familia cercana) cumple estas     recomendaciones?
movid_i$normas <- car::recode(movid_i$f8, c("1='Completamente';2='En gran medida';3='Bastante';4='Algo';5='Poco';6 ='Nada';c(8,9)=NA"), as.factor = T,
                                   levels = c("Completamente", "En gran medida", "Bastante", "Algo", "Poco", "Nada"))

table(movid_i$f8)
table(movid_i$normas)

## Normas
movid_i$cumple_normas <- ifelse(movid_i$normas=="Completamente" | movid_i$normas=="En gran medida",1,0)
movid_i$nocumple_normas <- ifelse(movid_i$normas=="Nada" | movid_i$normas=="Poco" | movid_i$normas=="Algo",1,0)

# B. Legitimacy (f3.3) ----------------------------------------------------
###Aunque a veces no estemos de acuerdo con las autoridades sanitarias y las medidas que se proponen, es nuestro deber seguir sus indicaciones al pie de la letra.
movid_i$soc1_bienestar <- car::recode(movid_i$f3_2, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                  levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

movid_i$soc2_obedecer <- car::recode(movid_i$f3_3, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                   levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

movid_i$soc3_gob <- car::recode(movid_i$f3_4, c("1='Muy de acuerdo';2='De acuerdo';3='Indiferente';4='En desacuerdo';5='Muy en desacuerdo';c(8,9)=NA"), as.factor = T,
                                     levels = c("Muy de acuerdo", "De acuerdo", "Indiferente", "En desacuerdo", "Muy en desacuerdo"))

table(movid_i$f3_3)
table(movid_i$soc2_obedecer)

# 4.Merge data ------------------------------------------------------------
movid_i_proc <- movid_i; remove(movid_i)
# movid_i <- left_join(movid_i_proc, lockdowns, by=c("comuna", "semana"))


# 4.1 Select variables ----------------------------------------------------

movid_i_proc <- movid_i_proc %>% select(id_encuesta, sexo, edad, region, comuna, nombre_comuna, nombre_region, factor_expansion, 161:189)

# 5. Save  -----------------------------------------------------------------
saveRDS(movid_i_proc, "output/data/movid_i_proc.RDS")
save(movid_i, file = "output/data/movid_i.RData")

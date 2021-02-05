# Code 3: Proc movid_i-IMPACT ------------------------------------------------------
# 1. Load packages -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Load data  -------------------------------------------
## movid_i-19
movid_i <- haven::read_dta("input/data/210118_base_movid_version01.dta")
movid_i <- movid_i %>% filter(entrevistado == 1) # Only people survey

## Lockdowns
lockdowns <- readRDS("output/data/lockdowns_movid_impact.RDS")

# 3. Recodes -----------------------------------------------------
# 1.  Compliance (f7_*)----------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("f7_")), funs(as.numeric(.)))

### In the last week, how often have you done the following actions to protect yourself from coronavirus?
movid_i$comp_wash <- car::recode(movid_i$f7_1, c("1='Almost never';2='Sometimes';3='Frequently';4='Almost always';5='Always';c(8,9)=NA"), as.factor = T,
                            levels = c("Almost never", "Sometimes", "Frequently", "Almost always", "Always"))

movid_i$comp_dist <- car::recode(movid_i$f7_2, c("1='Almost never';2='Sometimes';3='Frequently';4='Almost always';5='Always';c(8,9)=NA"), as.factor = T,
                                 levels = c("Almost never", "Sometimes", "Frequently", "Almost always", "Always"))

movid_i$comp_soc <- car::recode(movid_i$f7_3, c("1='Almost never';2='Sometimes';3='Frequently';4='Almost always';5='Always';c(8,9)=NA"), as.factor = T,
                                 levels = c("Almost never", "Sometimes", "Frequently", "Almost always", "Always"))

movid_i$comp_mask <- car::recode(movid_i$f7_4, c("1='Almost never';2='Sometimes';3='Frequently';4='Almost always';5='Always';c(8,9)=NA"), as.factor = T,
                                 levels = c("Almost never", "Sometimes", "Frequently", "Almost always", "Always"))

movid_i$comp_mask2 <- car::recode(movid_i$f7_5, c("1='Almost never';2='Sometimes';3='Frequently';4='Almost always';5='Always';c(8,9)=NA"), as.factor = T,
                                 levels = c("Almost never", "Sometimes", "Frequently", "Almost always", "Always"))

table(movid_i$f7_1)
table(movid_i$comp_wash)

# 2. Sociodemographic --------------------------------------

movid_i <- movid_i %>% mutate_at(vars(starts_with("a"), sexo, edad), funs(as.numeric(.)))
# 2.1 ID ------------------------------------------------------------------
movid_i$id_pob <- as.numeric(as.factor(movid_i$id_encuesta))

# 2.2 Sex -----------------------------------------------------------------
movid_i$sex <- as.numeric(movid_i$sexo)
movid_i$sex <- car::recode(movid_i$sex, c("1='Men';2='Women'"), as.factor = T,
                            levels = c("Women", "Men"))
table(movid_i$sexo)
table(movid_i$sex)
# 2.3 Age -----------------------------------------------------------------
movid_i$age <- as.numeric(movid_i$edad)
str(movid_i$age) # num

## Edad_3cat
movid_i$age_3cat <- car::recode(movid_i$edad, c("0:39='18 to 39';
                                                 40:64='40 to 64';
                                                 65:hi='65 or older'"), as.factor = T,
                                 levels = c("18 to 39", "40 to 64", "65 or older"))


# 2.4 Education ---------------------------------------------------------------
# Education: 3 categories: High school or less, Technical qualification and University degree
table(movid_i$a8a)
movid_i$educ_3cat <- as.numeric(movid_i$a8a)
movid_i$educ_3cat <- car::recode(movid_i$educ_3cat, c("c(1,2,3,4,5,6)='High school or less';7='Technical qualification';c(8,9)='University degree';99=NA"), as.factor = T,
                               levels = c("High school or less", 'Technical qualification','University degree'))

table(movid_i$educ_3cat)


# Economic ----------------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("g"), -ends_with("_esp"), -ends_with("_codif")), funs(as.numeric(.)))
# 2.5 Worker ------------------------------------------------------------------
table(movid_i$g1, useNA = "ifany")
movid_i$work <- as.numeric(movid_i$g1)
movid_i$work <- car::recode(movid_i$work, c("1='Yes'; 2='No'"), as.factor = T)
table(movid_i$work, useNA = "ifany")

# 2.6 Per capita income (log-income_hh) -------------------------------------------------------
###g47 (ingresos = ingresos autonomos + transferencias)
### No excluí a los 7 casos que ganan 0
hogares <- movid_i %>% group_by(id_encuesta) %>% summarise(nhogar = n())
movid_i <- movid_i %>% mutate(g47 = if_else(g47 == 1, 1, NA_real_)) %>% 
  merge(hogares, by = "id_encuesta", all.x = T) %>% 
  mutate(income_hh =  log(g47_monto/nhogar))
remove(hogares)


# Epidemiologic -----------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("c1"), -c1_6_esp), funs(as.numeric(.)))

# 2.6 Health risk -------------------------------------------------------------
# C1_* Enfermedades cronicas ----------------------------------------------
# C1_1 Diabetes -----------------------------------------------------------
movid_i$c1_1 <- ifelse(is.na(movid_i$c1_1), 0, 1)
# C1_2 Hipertension -----------------------------------------------------------
movid_i$c1_2 <- ifelse(is.na(movid_i$c1_2), 0, 1)
# C1_3 Cardiovascular -----------------------------------------------------------
movid_i$c1_3 <- ifelse(is.na(movid_i$c1_3), 0, 1)
# C1_4 Respiratoria -----------------------------------------------------------
movid_i$c1_4 <- ifelse(is.na(movid_i$c1_4), 0, 1)
# C1_5 Mental -----------------------------------------------------------
movid_i$c1_5 <- ifelse(is.na(movid_i$c1_5), 0, 1)
# C1_6 Otra -----------------------------------------------------------
movid_i$c1_6 <- ifelse(is.na(movid_i$c1_6), 0, 1)
# C1_7 Sano -----------------------------------------------------------
movid_i$c1_7 <- ifelse(is.na(movid_i$c1_7), 0, 1)
# C1_9 NA -----------------------------------------------------------
movid_i$c1_8 <- ifelse(is.na(movid_i$c1_8), 0, 1)
movid_i$c1_9 <- ifelse(is.na(movid_i$c1_9), 0, 1)


# Health risk: arterial hypertension, obesity, diabetes, chronic respiratory diseases (asthma, emphysema or other), cardiovascular diseases, active cancer, chronic kidney disease or immunodeficiencies
## Health risk General
table(movid_i$c1_9)
movid_i <- movid_i %>% mutate(cronicos = case_when(c1_1 == 1 ~ 1,
                                                   c1_2 == 1 ~ 1,
                                                   c1_3 == 1 ~ 1,
                                                   c1_4 == 1 ~ 1,
                                                   c1_7 == 1 ~ 0,
                                                   c1_8 == 1 ~ NA_real_,
                                                   c1_9 == 1 ~ NA_real_)) %>%
  mutate(cronicos = if_else(cronicos == 1, "Yes", "No"))

table(movid_i$cronicos, useNA = "always") ## Artritis

movid_i$cronicos <- ifelse(movid_i$c1_1==0 & movid_i$c1_2==0 & movid_i$c1_3==0 & movid_i$c1_4==0, "No",
                           movid_i$cronicos)

movid_i$cronicos <- factor(movid_i$cronicos, levels = c("No", "Yes"))

# 2.7 Health insurance --------------------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(starts_with("b")), funs(as.numeric(.)))
# Prevision 4 categorias --------------------------------------------------
movid_i$prev_4categ <- car::recode(movid_i$b2, c("1='Public';2='Private';4='Other';5='None';hi=NA"), as.factor = T,
                                   levels = c("None","Public", "Private", 'Other'))

table(movid_i$prev_4categ)

# 2.9 Lockdown  ------------------------------------- -------------------------
## Comuna ------------------------------------------------------------------
lockdowns <- lockdowns %>% rename(COMUNA = comuna)

## Recordar que se les pregunta por la semana anterior
## (encuestados entre semana 49 y 51, se les pregunta por 48 a 50)
##Op A
x <- movid_i %>% mutate(CUT = as.numeric(comuna),
                       date = as.Date("2020-12-4"),
                       semana=lubridate::week(date))
##Op B
movid_i <- movid_i %>% mutate(CUT = as.numeric(comuna)) %>% 
  left_join(lockdowns, by = "CUT") %>% 
  filter(semana %in% c(48:51))  %>%
  group_by(id_encuesta) %>% 
  filter(fasewk_lag1 == min(fasewk_lag1)) %>%   #Compliace consult by "IN the last week"
  distinct(id_encuesta, .keep_all = T) %>% 
  mutate(lockdown = if_else(fasewk_lag1 %in% c(1,2), "Yes","No"),
         lockdown = factor(lockdown, levels = c("No", "Yes")))

## Se crea variable lockdown que indica si
## El respondente estuvo en alguna cuarentena o no (fase 1 o 2)
## durante la semana anterior a la respuest, debido a que cumplimiento
## Se consulta respecto a la última semana. 


# 3. Instrumental factors -------------------------------------------------
## Transform numeric
movid_i <- movid_i %>% mutate_at(vars(contains("f6"), "f5_5"), funs(as.numeric(.)))

# A. Perived risk (f6) ----------------------------------------------------
### How dangerous do you think Coronavirus is for you and your closest one?
movid_i$per_risk <- car::recode(movid_i$f6, c("1='Not at all dangerous';2='Somewhat dangerous';3='Pretty dangerous';4='Very dangerous';5='Extremely dangerous';c(8,9)=NA"), as.factor = T,
                                 levels = c("Not at all dangerous", "Somewhat dangerous", "Pretty dangerous","Very dangerous", "Extremely dangerous"))

table(movid_i$f6)
table(movid_i$per_risk)

### Levels de riesgo
movid_i$high_risk <- ifelse(movid_i$per_risk=="Strongly agree" | movid_i$per_risk=="Agree",1,0)
movid_i$low_risk <- ifelse(movid_i$per_risk=="Strongly disagree" | movid_i$per_risk=="Disagree",1,0)

# B. Legal enforcement  (f5.5) --------------------------------------------
### ‘In Chile, if any person goes out without permission during a lockdown, it is very unlikely that this person will be inspected and punished.’

movid_i$leg_enforce <- car::recode(movid_i$f5_5, c("1='Strongly agree';2='Agree';3='Indifferent';4='Disagree';5='Strongly disagree';c(8,9)=NA"), as.factor = T,
                                 levels = c("Strongly agree", "Agree", "Indifferent", "Disagree", "Strongly disagree"))

table(movid_i$leg_enforce)

### Levels leg enforcement
movid_i$high_leg <- ifelse(movid_i$leg_enforce=="Pretty dangerous" | movid_i$leg_enforce=="Very dangerous"| movid_i$leg_enforce=="Extremely dangerous",1,0)
movid_i$low_leg <- ifelse(movid_i$leg_enforce=="Not at all dangerous" | movid_i$leg_enforce=="Somewhat dangerous",1,0)


# 4. Normative factors  -----------------------------------------------
movid_i <- movid_i %>% mutate_at(vars(contains("f8"), contains("f3")), funs(as.numeric(.)))

# A. Perceived social norms(f8) ----------------------------------------------
### Thinking about the different coronavirus care measures ( staying at home, wearing a mask, keeping a social distance or washing your hands), to what extent would you say that your closest ones (people who live with you or your close family) comply with these recommendations?

movid_i$normas <- car::recode(movid_i$f8, c("1='Completely';2='Mostly';3='Very much';4='Somewhat';5='A little';6 ='No at all';c(8,9)=NA"), as.factor = T,
                                   levels = c("Completely", "Mostly", "Very much","Somewhat", "A little", "No at all"))

table(movid_i$f8)
table(movid_i$normas)

## Normas
movid_i$cumple_normas <- ifelse(movid_i$normas=="Completely" | movid_i$normas=="Mostly",1,0)
movid_i$nocumple_normas <- ifelse(movid_i$normas=="Not at all" | movid_i$normas=="A little" | movid_i$normas=="Algo",1,0)

# B. Legitimacy (f3.3) ----------------------------------------------------
### Even if we sometimes disagree with the health authorities and the measures they propose, it is our duty to accuratly follow their indications.

movid_i$soc1_bienestar <- car::recode(movid_i$f3_2, c("1='Strongly agree';2='Agree';3='Indifferent';4='Disagree';5='Strongly disagree';c(8,9)=NA"), as.factor = T,
                                  levels = c("Strongly agree", "Agree", "Indifferent", "Disagree", "Strongly disagree"))

movid_i$soc2_obedecer <- car::recode(movid_i$f3_3, c("1='Strongly agree';2='Agree';3='Indifferent';4='Disagree';5='Strongly disagree';c(8,9)=NA"), as.factor = T,
                                   levels = c("Strongly agree", "Agree", "Indifferent", "Disagree", "Strongly disagree"))

#soc2_obedecer
movid_i$soc3_gob <- car::recode(movid_i$f3_4, c("1='Strongly agree';2='Agree';3='Indifferent';4='Disagree';5='Strongly disagree';c(8,9)=NA"), as.factor = T,
                                     levels = c("Strongly agree", "Agree", "Indifferent", "Disagree", "Strongly disagree"))

table(movid_i$f3_3)
table(movid_i$soc2_obedecer)

# 4.Merge data ------------------------------------------------------------
# 4.1 Select variables ----------------------------------------------------
movid_i_proc <- movid_i %>%   
  select(id_encuesta, sex, age, age_3cat, educ_3cat, income_hh, work, cronicos, prev_4categ, lockdown,
         factor_expansion, starts_with("comp"), contains("risk"),
         contains("normas"), contains("leg"), contains("soc"))

# 5. Save  -----------------------------------------------------------------
saveRDS(movid_i_proc, "output/data/movid_impact_proc.RDS")

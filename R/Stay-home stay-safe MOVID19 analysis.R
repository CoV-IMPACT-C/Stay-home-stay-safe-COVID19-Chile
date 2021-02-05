############################################################################################
################# Stay safe, stay home. Analysis of MOVID-19 data        ###################
################# Proyecto MOVID-19                                      ###################
################# Autor: C. Cuadrado                                     ###################
################# v1: Sept 4, 2020                                       ###################
############################################################################################

### Limpiar espacio de trabajo
rm(list = ls(all.names = TRUE))

### Working Directory

setwd("~/GitHub/Stay-home-stay-safe-COVID19-Chile")

### Cargar paquetes 
library(psych)
library(knitr)
library(kableExtra)
library(tidyverse)
library(dplyr)
library(lubridate)
library(modeldata)
library(sjPlot)
library(MASS)
library(lme4)
library(ggplot2)
library(ggeffects)
library(sjstats)
library(sjmisc)
library(stringr)
library(stringi)
library(recipes)
library(ggsignif)
library(ggplot2)
library(ggpubr)
library("googlesheets4")

### Cargar datos
data <- read_csv("~/Dropbox/MOVID-19/analisis/bases_anonimizadas/movid19_20200904.csv")
load("tsmargins_opinion.R")


# Minor changes
data$semana <- ifelse(data$semana==15,16,data$semana)

data$educ_3categ <- fct_relevel(fct_recode(as.factor(data$educ_3k),
                                            "Educación Media o inferior" = "1", 
                                            "Educación Técnica Nivel Superior" = "2",
                                            "Educación Profesional" = "3"),
                                 "Educación Media o inferior", "Educación Técnica Nivel Superior", "Educación Profesional")


### Cargar datos cuarentenas
data.cuarentena <- read_sheet("https://docs.google.com/spreadsheets/d/1m-UOSySxvqRJp8LjYO26282VWD6L5FU5gm5o8Mx7Uy4/edit?usp=sharing",
                  sheet = "Hoja 1", range = cell_rows(c(2, NA)))
data.cuarentena$comuna <- zoo::na.locf(data.cuarentena$...2) # Rellenar NA de comuna
data.cuarentena <- data.cuarentena %>% slice(.,1:(which(data.cuarentena == "Cordones sanitarios", arr.ind=TRUE)[1]-2))  # Eliminar data redundante
data.cuarentena$...3 <- data.cuarentena$...2 <- data.cuarentena$...1 <- NULL

# Limpiar base colapsando por comuna y asignando 1 a las comunas con al menos una parte de ella en cuarentena en cada día
data.cuarentena$comuna <- sub(pattern=" urbano",replacement = "", x=data.cuarentena$comuna)  # Eliminar "urbano" de algunas comunas
data.cuarentena <- data.cuarentena %>% group_by(comuna) %>% mutate_each(funs(as.numeric)) %>% 
  summarise_all(funs(sum)) %>%  ungroup() %>% 
  mutate_at(vars(-comuna),funs(ifelse(.>0,1,0)))

# Pasar a formato long y arreglar fechas
cuarentena_long <- tidyr:::gather(data.cuarentena, dia, cuarentena,colnames(data.cuarentena)[2]:tail(colnames(data.cuarentena),n=1), factor_key=F)
cuarentena_long <- cuarentena_long %>% separate(dia, sep ="-", c("day","month"))  %>% 
  mutate(year=2020,
         month=tolower(month),
         month=ifelse(month=="mar.",3,
                      ifelse(month=="abr.",4,
                             ifelse(month=="may.",5,
                                    ifelse(month=="jun.",6,
                                           ifelse(month=="jul.",7,
                                                  ifelse(month=="ago.",8,
                                                         ifelse(month=="sept.",9,NA))))))),
         date=lubridate::make_date(year, month, day),
         semana=lubridate::week(date))

cuarentena_long <- cuarentena_long %>% dplyr::select(-day,-month,-year)

# Generar base por semana + generar variable lag de 1 y 2 semanas para explorar efectos con delay
cuarentena_longwk <- cuarentena_long  %>% group_by(comuna)  %>% arrange(comuna,date) %>% 
  dplyr::select(-date) %>% group_by(comuna, semana) %>% 
  summarise_all(funs(mean), na.rm = TRUE) %>%                                         
  mutate(cuarentenawk = cuarentena,
         cuarentenawk_lag1 = lag(cuarentena, n=1),
         cuarentenawk_lag2 = lag(cuarentena, n=2),
         cuarentenawk = tidyr::replace_na(ifelse(cuarentenawk>=0.5,1,0),0), # Dicotomizar cuarentena semanal
         cuarentenawk_lag1 = tidyr::replace_na(ifelse(cuarentenawk_lag1>=0.5,1,0),0), # Dicotomizar cuarentena semanal y reemplazar NA
         cuarentenawk_lag2 = tidyr::replace_na(ifelse(cuarentenawk_lag2>=0.5,1,0),0))  %>%   # Dicotomizar cuarentena semanal reemplazar NA
  dplyr::select(-cuarentena)

### Merge
cuarentenas <- left_join(cuarentena_long, cuarentena_longwk, by=c("comuna", "semana"))

# Homogeneizar variables para el merge
data$fecha <- as.Date(data$fecha_obs)
cuarentenas$fecha <- as.Date(cuarentenas$date)
cuarentenas$comuna <- tolower(stringi::stri_trans_general(cuarentenas$comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula
data$comuna <- tolower(stringi::stri_trans_general(data$u2_comuna,"Latin-ASCII")) # Eliminar acentos y poner todo en minuscula

# Base final
data <- left_join(data, cuarentenas, by=c("comuna", "fecha", "semana"))

# Asignar 0 a las comunas sin cuarentenas en las fechas y semanas correspondientes
data$cuarentena <- tidyr::replace_na(data$cuarentena,0)
data$cuarentenawk <- tidyr::replace_na(data$cuarentenawk,0)
data$cuarentenawk_lag1 <- tidyr::replace_na(data$cuarentenawk_lag1,0)
data$cuarentenawk_lag2 <- tidyr::replace_na(data$cuarentenawk_lag2,0)

# Pegar fitted values de encuestas de opinion
data <- left_join(data, fitted_margins %>% rename(semana=week))

# Proporción de salidas por categoría en el tiempo y total de salidas
data <- data %>% mutate(salidas= p1_pra_trabajo + p1_pra_transporte + p1_pra_tramite + p1_pra_recrea + p1_pra_visita,
                          prop_trabajo=p1_pra_trabajo/salidas,
                          prop_transporte=p1_pra_transporte/salidas,
                          prop_tramite=p1_pra_tramite/salidas,
                          prop_recrea=p1_pra_recrea/salidas,
                          prop_visitar=p1_pra_visita/salidas) 

# Explore variables of interest
summary(data$cr1_per_riesgo)
summary(data$cr2_normas)

# To factor
data$per_riesgo <- as.factor(data$cr1_per_riesgo)
data$normas <- as.factor(data$cr2_normas)

summary(data$per_riesgo)
summary(data$normas)

# Modificar variables necesarias
data$salidas_dic <- ifelse(data$salidas>2,1,0)
data$per_riesgo_ord <- ifelse(data$per_riesgo=="Muy en desacuerdo",1,
                              ifelse(data$per_riesgo=="En desacuerdo",2,
                                     ifelse(data$per_riesgo=="Ni de acuerdo ni en desacuerdo",3,
                                            ifelse(data$per_riesgo=="De acuerdo",4,
                                                   ifelse(data$per_riesgo=="Muy de acuerdo",5,NA)))))
data$alto_riesgo <- ifelse(data$per_riesgo=="Muy de acuerdo" | data$per_riesgo=="De acuerdo",1,0)
data$cumple_normas <- ifelse(data$normas=="Completamente" | data$normas=="En gran medida",1,0)
data$bajo_riesgo <- ifelse(data$per_riesgo=="Muy en desacuerdo" | data$per_riesgo=="En desacuerdo",1,0)
data$nocumple_normas <- ifelse(data$normas=="Nada" | data$normas=="Poco" | data$normas=="Algo",1,0)
data$sexo <- ifelse(data$r2_sexo=="Otro",NA,data$r2_sexo)
data$prev_2categ <- as.factor(ifelse(data$pr2_prevision=="FONASA",0,
                                     ifelse(data$pr2_prevision=="ISAPRE",1,2)))
levels(data$prev_2categ) <- c("FONASA","ISAPRE", "Otro")

data$trabaja <- ifelse(data$pr3_ocupacion=="Trabaja de manera remunerada",1,0)

data$prev_4categ <- as.factor(ifelse(data$pr2_prevision=="Ninguna",0,
                                     ifelse(data$pr2_prevision=="FONASA",1,
                                            ifelse(data$pr2_prevision=="ISAPRE",2,3))))
levels(data$prev_4categ) <- c("Ninguna","FONASA","ISAPRE", "Otro")


data$id_pob <- as.numeric(as.factor(data$pob_id))
summary(data$id_pob)

data.short <- data %>% filter(!is.na(normas))

# Explore by week

ggplot(data %>% filter(!is.na(per_riesgo)), aes(x=semana, y=salidas, 
                                                  colour = per_riesgo, 
                                                  group = per_riesgo)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  labs(y="Número de veces que sale del hogar en última semana", x="Semana",
       colour = "Percepción de riesgo") +
  coord_cartesian(ylim=c(0,7)) +
  scale_x_continuous(breaks = c(31,32,33,34,35,36),
                     labels=c("29 julio", "1 Junio","12 agosto","19 agosto","26 agosto","2 septiembre")) +
  ggsci::scale_color_jama() +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figure 1 - percepción de riesgo.png", width = 7.4*2.5, height = 6.7*2.5, units = "cm", dpi=300, limitsize = FALSE)

ggplot(data %>% filter(!is.na(per_riesgo)) %>% mutate(alto_riesgo=ifelse(alto_riesgo==1,"Bajo riesgo", "Alto riesgo")), 
                                            aes(x=semana, y=salidas, 
                                                colour = as.factor(alto_riesgo), 
                                                group = alto_riesgo)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  labs(y="Número de veces que sale del hogar en última semana", x="Semana",
       colour = "Percepción de riesgo") +
  coord_cartesian(ylim=c(0,7)) +
  scale_x_continuous(breaks = c(31,32,33,34,35,36),
                     labels=c("29 julio", "1 Junio","12 agosto","19 agosto","26 agosto","2 septiembre")) +
  ggsci::scale_color_jama() +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figure 1b - percepción de riesgo dic.png", width = 7.4*2.5, height = 6.7*2.5, units = "cm", dpi=300, limitsize = FALSE)

ggplot(data %>% filter(!is.na(normas)), aes(x=semana, y=salidas, 
                                                colour = normas, 
                                                group = normas)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  labs(y="Número de veces que sale del hogar en última semana", x="Semana",
       colour = "Adherencia norma social") +
  coord_cartesian(ylim=c(0,10)) +
  scale_x_continuous(breaks = c(31,32,33,34,35,36),
                     labels=c("29 julio", "1 Junio","12 agosto","19 agosto","26 agosto","2 septiembre")) +
  ggsci::scale_color_jama() +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figure 2 - adherencia a norma.png", width = 7.4*2.5, height = 6.7*2.5, units = "cm", dpi=300, limitsize = FALSE)

ggplot(data %>% filter(!is.na(normas)) %>% mutate(cumple_normas=ifelse(cumple_normas==1,"Alto cumplimiento", "Bajo cumplimiento")), 
                                          aes(x=semana, y=salidas, 
                                            colour = cumple_normas, 
                                            group = cumple_normas)) +
  stat_summary(fun.y="mean", geom="point") +
  stat_summary(fun.y="mean", geom="line") +
  labs(y="Número de veces que sale del hogar en última semana", x="Semana",
       colour = "Percepción de adherencia norma social") +
  coord_cartesian(ylim=c(0,7)) +
  scale_x_continuous(breaks = c(31,32,33,34,35,36),
                     labels=c("29 julio", "1 Junio","12 agosto","19 agosto","26 agosto","2 septiembre")) +
  ggsci::scale_color_jama() +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figure 2b - adherencia a norma dic.png", width = 7.4*2.5, height = 6.7*2.5, units = "cm", dpi=300, limitsize = FALSE)


# Modelos

# Test
# logit.simple <- glm(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cumple_normas + per_riesgo_ord + cuarentenawk_lag1 +
#                         semana0, data=data.short, family = binomial("logit"))
# tab_model(logit.simple)

# Modelo 0: modelo linear multinivel con variable respuesta número total de salidas
lmer.simple <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + per_riesgo_ord + cumple_normas +
                      semana0 + (1 | semana:id_pob), data=data.short)
save(lmer.simple,file="Model0 lmME.R")
# tab_model(lmer.simple)

lmer.simple.risk <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + per_riesgo_ord +
                      semana0 + (1 | semana:id_pob), data=data.short)
save(lmer.simple.risk,file="Model0risk lmME.R")
# tab_model(lmer.simple.risk)

lmer.simple.norma <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + cumple_normas +
                           semana0 + (1 | semana:id_pob), data=data.short)
save(lmer.simple.norma,file="Model0norma lmME.R")
# tab_model(lmer.simple.norma)

lmer.simple.null <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + 
                            semana0 + (1 | semana:id_pob), data=data.short)
save(lmer.simple.null,file="Model0null lmME.R")
# tab_model(lmer.simple.norma)

lmer.simple.int <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 * per_riesgo_ord + cuarentenawk_lag1 * cumple_normas +
                      semana0 + (1 | semana:id_pob), data=data.short)
save(lmer.simple.int,file="Model0b lmME.R")
# tab_model(lmer.simple.int)

lmer.simple.subet <- lmer(salidas ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + per_riesgo_ord + cumple_normas +
                      semana0 + (1 | semana:id_pob), data=data.short %>% filter(cuarentenawk_lag1==1))
save(lmer.simple,file="Model0a lmME.R")

table1 <- tab_model(lmer.simple, lmer.simple.subet, lmer.simple.int, 
          dv.labels = c("General population", "Restricted", "Interaction"),
          string.pred = "Coefficient",
          string.p = "P-Value",
          # group.pred = TRUE,
          pred.labels = c("Intercept", "Sex (Male=1)", "Age (years)",
                          "Health risk-factor (1=Yes)",
                          "Technical qualification (ref: high-school or less)",
                          "University degree (ref: high-school or less)",
                          "Public health insurance (ref: no insurance)",
                          "Private health insurance (ref: no insurance)",
                          "Other health insurance (ref: no insurance)",
                          "Occupation (1=Works)",
                          "Live in an area under lockdown (1=Yes)",
                          "Perceived risk (1=High)",
                          "Perceived social norms (1=Comply)",
                          "Time (weeks)",
                          "Lockdown * perceived risk",
                          "Lockdown * social norms"),
          show.ci=F, title="Number of out-of-home activities per week (Multilevel linear model)",
          file="table1.html")
table1

table1b <- tab_model(lmer.simple.null, lmer.simple.norma, lmer.simple.risk, lmer.simple, 
                    dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
                    string.pred = "Coefficient",
                    string.p = "P-Value",
                    # group.pred = TRUE,
                    pred.labels = c("Intercept", "Sex (Male=1)", "Age (years)",
                                    "Health risk-factor (1=Yes)",
                                    "Technical qualification (ref: high-school or less)",
                                    "University degree (ref: high-school or less)",
                                    "Public health insurance (ref: no insurance)",
                                    "Private health insurance (ref: no insurance)",
                                    "Other health insurance (ref: no insurance)",
                                    "Occupation (1=Works)",
                                    "Live in an area under lockdown (1=Yes)",
                                    "Time (weeks)",
                                    "Perceived risk (1=High)",
                                    "Perceived social norms (1=Comply)"),
                    show.ci=F, title="Number of out-of-home activities per week (Multilevel linear model)",
                    file="table1b.html")
table1b

# Margins effects plot
ggpredict(lmer.simple.int, type = "re", terms = c("per_riesgo_ord"))
ggemmeans(lmer.simple.int, type = "fe", terms = c("per_riesgo_ord"))

salidas <- ggemmeans(lmer.simple.int, type = "fe", terms = c("per_riesgo_ord"))

salidas.plot <- ggplot(salidas, aes(x, predicted)) + 
  geom_line(show.legend=FALSE) + 
  geom_point(show.legend=FALSE) +
  geom_point(aes(x,conf.low)) +
  geom_ribbon(aes(ymin=salidas$conf.low, ymax=salidas$conf.high), alpha=0.1, show.legend=FALSE) +
  labs(x="Perceived risk of COVID-19",
       y="Frequency of out-of-home activities per week") + 
  # ylim(0,.2) +
  theme_minimal()
salidas.plot
# ggsave("Efectos marginales riesgo percibido y salidas.png", width = 8*1.5, height = 6*1.5)   

plot_model(lmer.simple.int, terms = c("per_riesgo_ord[1:5]","cuarentenawk_lag1"), type="pred") + 
  theme_minimal() +
  labs(x="Perceived risk of COVID-19", title = "", color="Lockdown",
       y="Frequency of out-of-home activities per week") + ylim(0,7)
ggsave("Efectos marginales riesgo percibido y salidas.png", width = 8*1.5, height = 6*1.5)   


# Modelos con variable binaria

# Modelo 1: sexo, edad, educacion, factres de riesgo seguro de salud
logit.model1 <- glmer(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 +
                           semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                           control = glmerControl(optimizer = "bobyqa",  optCtrl=list(maxfun=2e5)))
save(logit.model1,file="Model1 LogitME.R")
tab_model(logit.model1)

# Modelo 2: sexo, edad, educacion, factores de riesgo, seguro de salud, riesgo percibido
logit.model2 <- glmer(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + alto_riesgo +
                        semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
save(logit.model2,file="Model2 LogitME.R")
tab_model(logit.model2)

# Modelo 3: sexo, edad, educacion, factores de riesgo, seguro de salud, normas
logit.model3 <- glmer(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + cumple_normas +
                        semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
save(logit.model3,file="Model3 LogitME.R")
tab_model(logit.model3)

# Modelo 4: sexo, edad, educacion, factores de riesgo, seguro de salud, riesgo percibido, normas
logit.model4 <- glmer(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 + alto_riesgo + cumple_normas +
                              semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                            control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
save(logit.model4,file="Model4 LogitME.R")
tab_model(logit.model4)

# Modelo 5: sexo, edad, educacion, factores de riesgo, seguro de salud, riesgo percibido, normas
logit.model5 <- glmer(salidas_dic ~ sexo + edad + riesgo + educ_3categ + prev_4categ + trabaja + cuarentenawk_lag1 * alto_riesgo + cuarentenawk_lag1 * cumple_normas +
                        semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))
save(logit.model5,file="Model5 LogitME.R")
tab_model(logit.model5)

table2 <- tab_model(logit.model1, logit.model2, logit.model3, logit.model4, logit.model5,
                    dv.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
                    string.pred = "Coefficient",
                    string.p = "P-Value",
                    # group.pred = TRUE,
                    pred.labels = c("Intercept", "Sex (Male=1)", "Age (years)",
                                    "Health risk-factor (1=Yes)",
                                    "Technical qualification (ref: high-school or less)",
                                    "University degree (ref: high-school or less)",
                                    "Public health insurance (ref: no insurance)",
                                    "Private health insurance (ref: no insurance)",
                                    "Other health insurance (ref: no insurance)",
                                    "Occupation (1=Works)",
                                    "Live in an area under lockdown (1=Yes)",
                                    "Time (weeks)",
                                    "Perceived risk (1=High)",
                                    "Perceived social norms (1=Comply)",
                                    "Lockdown * perceived risk",
                                    "Lockdown * social norms"),
                    show.ci=F, title="Persons that report 2 or more out-of-home activities per week (Multilevel logistic models)",
                    file="table2.html")

# Paper figures
data.short$cumple_normas2 <- as.factor(data.short$cumple_normas)
data.short$cumple_normas2 <- ifelse(data.short$cumple_normas2==0, "Low", "High")
table(data.short$cumple_normas2)
data.short$trabaja2 <- as.factor(data.short$trabaja)
data.short$trabaja2 <- ifelse(data.short$trabaja==0, "Does not work", "Works")
table(data.short$trabaja2)
data.short$edad_cat3 <- cut(data.short$edad, c(0,35,60,110))
data.short$edad_cat3 <- ifelse(data.short$edad_cat3=="(0,35]", "18-34",
                               ifelse(data.short$edad_cat3=="(35,60]", "35-60","60+"))
table(data.short$edad_cat3)

data.short$cuarentenawk_lag1.2 <- ifelse(data.short$cuarentenawk_lag1==0, "No", "Yes")
table(data.short$cuarentenawk_lag1.2)
data.short$sexo2 <- ifelse(data.short$sexo=="Femenino", "Women", "Man")
table(data.short$sexo2)

logit.model4b <- glmer(salidas_dic ~ sexo2 + edad_cat3 + riesgo + educ_3categ + prev_4categ + trabaja2 + cuarentenawk_lag1.2 + cumple_normas2 + per_riesgo_ord +
                         semana0 + (1 | id_pob), data=data.short, family = binomial("logit"),
                       control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)))

ggpredict(logit.model4b, type = "re", terms = c("sexo2"))

sex <- ggemmeans(logit.model4b, type = "fe", terms = c("sexo2"))
age <- ggemmeans(logit.model4, type = "fe", terms = c("edad"))
works <- ggemmeans(logit.model4b, type = "fe", terms = c("trabaja2"))
norms <- ggemmeans(logit.model4b, type = "fe", terms = c("cumple_normas2"))
cierre <- ggemmeans(logit.model4b, type = "fe", terms = c("cuarentenawk_lag1.2"))
riesgo <- ggemmeans(logit.model4b, type = "fe", terms = c("per_riesgo_ord"))

sexo <- ggplot(sex, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Sex", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
sexo

edad <- ggplot(age, aes(y=predicted, x=x)) + geom_line(size=1) + geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3) +
  coord_cartesian(y=c(0,1)) + labs(x = "Age", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
edad

trabaja <- ggplot(works, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Occupation", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
trabaja

normas <- ggplot(norms, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Perceived Social Norms", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
normas

lockdown <- ggplot(cierre, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Lockdown in area", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
lockdown

risk <- ggplot(riesgo, aes(y=predicted, x=x)) + geom_line(size=1) + geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3) +
  coord_cartesian(y=c(0,1)) + labs(x = "Perceived risk COVID", y = "Left home at least twice a week (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
risk


plot <- ggarrange(sexo + theme(axis.title.y = element_text(size = 9)), 
                  edad + theme(axis.title.y = element_text(size =9)), 
                  trabaja + theme(axis.title.y = element_text(size = 9)), 
                  lockdown + theme(axis.title.y = element_text(size = 9)), 
                  normas + theme(axis.title.y = element_text(size = 9)), 
                  risk + theme(axis.title.y = element_text(size =9)), 
                  ncol = 2, nrow = 3,
                  labels = c("A", "B", "C", "D", "E", "F"))
plot

ggsave(
  plot = plot,
  filename = "plot_est_eff_MOVID.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 18,
  height = 23)

p1 <- plot_model(logit.model4b, terms = c("per_riesgo_ord[1:5]"), type="pred") + 
  theme_minimal() +
  labs(x="Perceived risk of COVID-19", title = "",
       y="logit.model4b") + ylim(0,1)
p1
f1 <- plot_model(lmer.simple.int, terms = c("per_riesgo_ord[1:5]"), type="pred") + 
  theme_minimal() +
  labs(x="Perceived risk of COVID-19", title = "",
       y="Frequency of out-of-home activities per week") + ylim(0,7)
f1

plot2 <- ggarrange(p1, f1, ncol = 1, nrow = 2,
                  labels = c("A", "B"))

ggsave(
  plot = plot2,
  filename = "plot_est_eff_MOVID_risk.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 18,
  height = 15)


##############

# Sample description
data.short %>% 
group_by(sexo) %>% 
summarise(percent = 100 * n() / nrow(data.short))

data.short %>% 
  mutate(age_group=cut(edad, breaks = c(0,35,60,100))) %>% 
  group_by(age_group) %>% 
  summarise(percent = 100 * n() / nrow(data.short))

data.short %>% 
  group_by(salidas_dic) %>% 
  summarise(n=n(), percent = 100 * n() / nrow(data.short))

data.short %>% filter(semana==31)  %>%
  group_by(salidas_dic) %>% 
  summarise(percent = 100 * n() / nrow(data.short %>% filter(semana==31)))

data.short %>% filter(semana==34)  %>%
  group_by(salidas_dic) %>% 
  summarise(percent = 100 * n() / nrow(data.short %>% filter(semana==35)))

data.short %>%
  group_by(semana) %>% 
  summarise(mean = mean(salidas, na.rm=T),
            min = min(salidas, na.rm=T),
            max = max(salidas, na.rm=T),
            sd = sd(salidas, na.rm=T))

data.short %>% 
  group_by(normas) %>% 
  summarise(n=n(), percent = 100 * n() / nrow(data.short))


## MOVID-Impact models 
# Cargar datos
movid_i <- readRDS("output/data/movid_impact_proc.RDS")

# Recodificaciones
data <- movid_i %>% ungroup() %>%  
  mutate(across(c(starts_with("comp")), ## Compliance
                ~case_when(
                  . == "Always" ~ 1,
                  is.na(.) ~ NA_real_,
                  TRUE ~ 0)), 
         across(c(per_risk), ~as.numeric(.)), ## Instrumental A 
         leg_enforce = case_when( ## Instrumental B
           leg_enforce %in% c("Strongly disagree", "Disagree") ~ 1,
           is.na(leg_enforce) ~ NA_real_,
           TRUE ~ 0),
         leg_enforce = factor(leg_enforce),
         normas = (as.numeric(normas)*-1)+7, ## Normative A
         norms = if_else(nocumple_normas == 1,0,1), #Creo
         soc2_obedecer = (as.numeric(soc2_obedecer)*-1)+6) ##Normative B

data$cumple_normas <-  ifelse(data$cumple_normas == 1, "High", 
                              ifelse(data$cumple_normas == 0, "Low", NA ))
data$cumple_normas <-  factor(data$cumple_normas, levels = c("Low", "High"))

data$leg_enforce <-  ifelse(data$leg_enforce == 1, "High", 
                              ifelse(data$leg_enforce == 0, "Low", NA ))
data$leg_enforce <-  factor(data$leg_enforce, levels = c("Low", "High"))

# Modelos de regresión
predictors <- c("sex + age + educ_3cat +  cronicos + 
                  prev_4categ + work + lockdown +  
                  per_risk + leg_enforce +
                  cumple_normas + soc2_obedecer")

m_wash  <- glm(as.formula(paste0("comp_wash ~", predictors)), 
               data=data, family="binomial")
m_dist   <- glm(as.formula(paste0("comp_dist ~", predictors)),
                data=data, family="binomial")
m_soc <- glm(as.formula(paste0("comp_soc ~", predictors)),
             data=data, family="binomial")
m_mask   <- glm(as.formula(paste0("comp_mask ~", predictors)),
                data=data, family="binomial")
m_mask2  <- glm(as.formula(paste0("comp_mask2 ~", predictors)),
                data=data, family="binomial")
tab_model(m_wash, m_dist, m_soc, m_mask, m_mask2)

# Gráficos de valores estimados
sex <- ggemmeans(m_dist, type = "fe", terms = c("sex"))
age <- ggemmeans(m_dist, type = "fe", terms = c("age"))
works <- ggemmeans(m_dist, type = "fe", terms = c("work"))
riesgo <- ggemmeans(m_dist, type = "fe", terms = c("per_risk"))
leg <- ggemmeans(m_dist, type = "fe", terms = c("leg_enforce"))
norms <- ggemmeans(m_dist, type = "fe", terms = c("cumple_normas"))

sexo <- ggplot(sex, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Sex", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
sexo

edad <- ggplot(age, aes(y=predicted, x=x)) + geom_line(size=1) + geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3) +
  coord_cartesian(y=c(0,1)) + labs(x = "Age", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
edad

trabaja <- ggplot(works, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Works", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
trabaja


normas <- ggplot(norms, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Perceived Social Norms", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
normas

leg_enf <- ggplot(leg, aes(y=predicted, x=x)) + geom_point(size=3) + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size=1, width=0.05) +
  coord_cartesian(y=c(0,1)) + labs(x = "Perceived legal enforcement", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
leg_enf

risk <- ggplot(riesgo, aes(y=predicted, x=x)) + geom_line(size=1) + geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.3) +
  coord_cartesian(y=c(0,1)) + labs(x = "Perceived risk COVID", y = "Keep at least 2 meters away from people (%)") +
  theme_minimal() + scale_y_continuous(labels=scales::percent)
risk


plot2 <- ggarrange(sexo + theme(axis.title.y = element_text(size = 9)), 
                  edad + theme(axis.title.y = element_text(size =9)), 
                  trabaja + theme(axis.title.y = element_text(size = 9)), 
                  risk + theme(axis.title.y = element_text(size = 9)), 
                  leg_enf + theme(axis.title.y = element_text(size = 9)), 
                  normas + theme(axis.title.y = element_text(size =9)), 
                  ncol = 2, nrow = 3,
                  labels = c("A", "B", "C", "D", "E", "F"))
plot2

ggsave(
  plot = plot2,
  filename = "output/Figures/plot_est_eff_MOVID-impact.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 18,
  height = 23)



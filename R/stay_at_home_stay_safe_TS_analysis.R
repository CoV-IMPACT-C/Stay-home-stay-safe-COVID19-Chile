############################################################################################
################# Stay Home Stay Safe - September 2020                  ###################
################# Proyecto MOVID-19                                      ###################
################# Author: M. Gerber                                      ###################
################# v1 Last Update: November, 8 2020                       ###################
############################################################################################

## Study 1: Termómetro Social

# Libraries
library(haven)
library(dplyr)
library(readxl)
library(sjPlot)
library(ggplot2)
library(ggpubr)

# Data
TS <- read_dta("TS3_Entrega.dta")

# Missing
TS[TS==-999 | TS==-888] <- NA

# Sociodemographics
TS$sexo <- ifelse(TS$a2==1, 1,
                   ifelse(TS$a2==2, 0, NA))

TS$sexo_f <- ifelse(TS$a2==1, "Man",
                  ifelse(TS$a2==2, "Woman", NA))

TS$sexo_f <- factor(TS$sexo_f,levels = c("Woman", "Man"))

            
TS$educ_3cat <- ifelse(TS$f1<10, "Media completa o menos",
                       ifelse(TS$f1<12, "Técnico",
                              ifelse((TS$f1==12 | TS$f1==13 | TS$f1==14), "Universitaria", NA)))

TS$educ_media <- ifelse(TS$educ_3cat=="Media completa o menos", 1,
                        ifelse(TS$educ_3cat=="Técnico" | TS$educ_3cat=="Universitaria", 0, NA))

TS$educ_tecnico <- ifelse(TS$educ_3cat=="Media completa o menos", 0,
                        ifelse(TS$educ_3cat=="Técnico", 1,
                               ifelse(TS$educ_3cat=="Universitaria", 0, NA)))

TS$educ_univ <- ifelse(TS$educ_3cat=="Media completa o menos", 0,
                          ifelse(TS$educ_3cat=="Técnico", 0,
                                 ifelse(TS$educ_3cat=="Universitaria", 1, NA)))


# Leaving home
TS <- TS %>% mutate_at(vars(starts_with('d3')), as.numeric)

TS$casa <- ifelse(TS$d34 == 1, 1,
                  ifelse(TS$d34 == 2, 0, NA))

TS$salir <- ifelse(TS$d34 == 1, 0,
                   ifelse(TS$d34 == 2, 1, NA))

# Perceived risk
TS$riesgo <- ifelse(is.na(TS$d1), NA, TS$d1)

# Social norms
TS$norma <- TS$d5
TS$norma_dic <- ifelse(TS$norma<4, 0, 
                ifelse(TS$norma>3, 1, NA)) # Corte Ninguno-Algunos vs. La mayoría-Todos

TS$norma_f <- ifelse(TS$norma<4, "Low", 
              ifelse(TS$norma>3, "High", NA)) # Corte Ninguno-Algunos vs. La mayoría-Todos

TS$norma_f <- factor(TS$norma_f, levels = c("Low", "High"))


# Controlled by law enforcement agencies

TS$fiscalizado <- ifelse(TS$d6==1, 1,
                  ifelse(TS$d6==2, 0, NA))


# Lack of income staying home
TS$cuarentena_ingresos <- TS$d7__2 

# Trust
TS$confianza_minsal <- TS$d111
TS$confianza_presi <- TS$d113

# Income
TS$ingreso_hogar <- TS$f11

TS$mdc_ing <- ifelse(TS$f12 == 1, 164000,
                     ifelse(TS$f12 == 2, 380000,
                            ifelse(TS$f12 == 3, 490000,
                                   ifelse(TS$f12 == 4, 600000,
                                          ifelse(TS$f12 == 5, 710000,
                                                 ifelse(TS$f12 == 6, 850000,
                                                        ifelse(TS$f12 == 7, 1000000,
                                                               ifelse(TS$f12 == 8, 1300000,
                                                                      ifelse(TS$f12 == 9, 1800000,
                                                                             ifelse(TS$f12 == 10, 2600000,NA))))))))))

TS$ing_hogar2 <- ifelse(!is.na(TS$ingreso_hogar),TS$ingreso_hogar,
                        ifelse(is.na(TS$ingreso_hogar), TS$mdc_ing, NA)) ## quedan 67 NA's

TS$log_ingh <- log(TS$ing_hogar2)
TS$log_ingh <- ifelse(!is.infinite(TS$log_ingh), TS$log_ingh, NA)


# Occupation
TS$ocupacion <- as.factor(TS$f5)

TS$trabaja <- ifelse(is.na(TS$f5), NA, 
              ifelse((TS$f5 < 6 | TS$f5==10 | TS$f5==11 | TS$f5==12), 1, 0))

TS$trabaja_f <- ifelse(is.na(TS$f5), NA, 
                       ifelse((TS$f5 < 6 | TS$f5==10 | TS$f5==11 | TS$f5==12), "Works", "Does not work"))

TS$trabaja_f <- factor(TS$trabaja_f,levels = c("Does not work", "Works"))

# Health insurance
TS$prevsalud <- ifelse(TS$f15 == 1, "FONASA",
                       ifelse(TS$f15 == 2, "ISAPRE",
                              ifelse(TS$f15 == 3 | TS$f15 == 5, "Otro", 
                                     ifelse(TS$f15 == 4, "Ninguno", NA))))

TS$prevsalud <- factor(TS$prevsalud, levels = c("Ninguno", "FONASA", "ISAPRE", "Otro"))

TS$fonasa <- ifelse(TS$prevsalud=="FONASA", 1, 
                    ifelse(TS$prevsalud == "ISAPRE", 0, 
                           ifelse(TS$prevsalud == "Otro", 0,
                                  ifelse(TS$prevsalud == "Ninguno", 0, NA))))
TS$isapre <- ifelse(TS$prevsalud=="FONASA", 0, 
                    ifelse(TS$prevsalud == "ISAPRE", 1, 
                           ifelse(TS$prevsalud == "Otro", 0,
                                  ifelse(TS$prevsalud == "Ninguno", 0, NA))))
TS$otro <- ifelse(TS$prevsalud=="FONASA", 0, 
                    ifelse(TS$prevsalud == "ISAPRE", 0, 
                           ifelse(TS$prevsalud == "Otro", 1,
                                  ifelse(TS$prevsalud == "Ninguno", 0, NA))))

# Health risk-factor
TS$preexistencia <- ifelse(TS$b181 == 1 |
                      TS$b182 == 1 |
                      TS$b183 == 1 |
                      TS$b184 == 1 |
                      TS$b185 == 1 |
                      TS$b186 == 1 |
                      TS$b187 == 1 |
                      TS$b188 == 1, 1,0)

TS$riesgo_salud <- ifelse((TS$preexistencia==1 | TS$edad>59), 1,
                          ifelse((TS$preexistencia==0 & TS$edad <60), 0, NA))


# Lockdown
cuarentena <- read_excel("cuarentena.xlsx")
cuarentena$comuna <- tolower(stringi::stri_trans_general(cuarentena$comuna,"Latin-ASCII"))
TS$comuna <- tolower(stringi::stri_trans_general(TS$comuna,"Latin-ASCII"))

TS <- left_join(TS, cuarentena, by=c("comuna"))
table(TS$cuarentena, useNA="always")
TS$cuarentena <- ifelse(is.na(TS$cuarentena), 0, TS$cuarentena)

TS$cuarentena <- ifelse(TS$cuarentena==0, 0, 
                        ifelse(TS$cuarentena==1, 1, NA))


TS$cuarentena_f <- ifelse(TS$cuarentena == 1, "Lockdown",
                       ifelse(TS$cuarentena == 0, "No lockdown", NA))

TS$cuarentena_f <- factor(TS$cuarentena_f, levels = c("No lockdown", "Lockdown"))


## Models
# Sociodemographic
m1 <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena,
          data = TS, family = "binomial")

# Instrumental Factors
m2 <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro +  trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
          riesgo + fiscalizado,
          data = TS, family = "binomial")

tab_model(m2)

# Normative factors
m3 <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro +  trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
          norma_dic + confianza_presi + confianza_minsal,
          data = TS, family = "binomial")

# Full Model
m4 <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
            riesgo + fiscalizado +
            norma_dic + confianza_presi + confianza_minsal,
          data = TS, family = "binomial")
tab_model(m1,m2,m3,m4)

# Plots

m_sexo <- glm(salir ~ sexo_f + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
            riesgo + fiscalizado +
            norma_dic + confianza_presi + confianza_minsal,
          data = TS, family = "binomial")

m_edad <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
                riesgo + fiscalizado +
                norma_dic + confianza_presi + confianza_minsal,
              data = TS, family = "binomial")

m_norma <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
                riesgo + fiscalizado +
                norma_f + confianza_presi + confianza_minsal,
              data = TS, family = "binomial")

m_trabaja <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja_f + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena +
                riesgo + fiscalizado +
                norma_dic + confianza_presi + confianza_minsal,
              data = TS, family = "binomial")

m_cuarentena <- glm(salir ~ sexo + edad + educ_tecnico + educ_univ + isapre + fonasa + otro + trabaja + log_ingh + riesgo_salud + cuarentena_ingresos + cuarentena_f +
                   riesgo + fiscalizado +
                   norma_dic + confianza_presi + confianza_minsal,
                 data = TS, family = "binomial")

theme_minimal()

sexo <- plot_model(m_sexo, type="pred", terms= "sexo_f",
        title = "") + 
        coord_cartesian(y=c(0, 0.5)) + labs(x = "Sex", y = "Left home day before (%)") +
        theme_minimal()

edad <- plot_model(m_edad, type="pred", terms= "edad",
                   title = "") + 
  coord_cartesian(y=c(0, 0.5)) + labs(x = "Age ", y = "Left home day before (%)") +
  theme_minimal()

trabaja <- plot_model(m_trabaja, type="pred", terms= "trabaja_f",
                   title = "") + 
  coord_cartesian(y=c(0, 0.5)) + labs(x = "Occupation", y = "Left home day before (%)") +
  theme_minimal()

normas <- plot_model(m_norma, type="pred", terms= "norma_f",
                   title = "") + 
  coord_cartesian(y=c(0, 0.5)) + labs(x = "Perceived Social Norms", y = "Left home day before (%)") +
  theme_minimal()

cuarentena <- plot_model(m_cuarentena, type="pred", terms= "cuarentena_f",
                     title = "") + 
  coord_cartesian(y=c(0, 0.5)) + labs(x = "Lockdown in area", y = "Left home day before (%)") +
  theme_minimal()

plot <- ggarrange(sexo, edad, trabaja, cuarentena, normas, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

ggsave(
  plot = plot,
  filename = "plot_est_eff.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 18,
  height = 23)



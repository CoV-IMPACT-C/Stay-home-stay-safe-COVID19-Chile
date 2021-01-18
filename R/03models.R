# Code 3: Models --------------------------------------------------------------


# 1. Load packages --------------------------------------------------------


# 2. Load data ------------------------------------------------------------


movid.short <- movid %>% filter(!is.na(normas))


# 3. Explore --------------------------------------------------------------


# 4. Study 1 Models: TS ---------------------------------------------------


# 5. Study 2 Models: MOVID ------------------------------------------------



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

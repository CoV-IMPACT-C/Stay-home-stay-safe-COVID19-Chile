# Code 3: Models --------------------------------------------------------------


# 1. Load packages --------------------------------------------------------


# 2. Load data ------------------------------------------------------------


movid.short <- movid %>% filter(!is.na(normas))


# 3. Explore --------------------------------------------------------------


# 4. Study 1 Models: TS ---------------------------------------------------


# 5. Study 2 Models: MOVID ------------------------------------------------

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


# Tables --------------------------------------------------------------------

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


# Plots -------------------------------------------------------------------

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



# Modelos con variables binarias ------------------------------------------

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
table2

summary(data.short$bajo_riesgo)


# Final export Study ------------------------------------------------------

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


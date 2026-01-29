install.packages("readxl")
library(readxl)
library(summarytools)
library(lavaan)
library(lavaanPlot)
library(MVN)
library(psych)
library(readxl)
library(EFAtools)
library(Amelia)
library(writexl)
HIJOS <- read_excel("HIF_09_22.xlsx", sheet = "ANALISIS",na = "999")
HJ_PRO<- HIJOS[, c("CP1", "CP2", "CP4", "CP5", 
                             "CP7", "CP9", "CP10", "CP12", 
                             "CP13", "CP15")]
PRO_IMP<-amelia(HJ_PRO, ords =c("CP1", "CP2", "CP4", "CP5", 
                       "CP7", "CP9", "CP10", "CP12", 
                       "CP13", "CP15"))
PRO_ANA<-PRO_IMP$imputations$imp3
modelo_prosocial <- '
  ConductaProsocial =~ CP1 + CP2 + CP4 + CP5 +
                       CP7 + CP9 + CP10 + CP12 +
                       CP13 + CP15
'
fit_prosocial <- cfa(modelo_prosocial, 
                     data = PRO_ANA, 
                     estimator = "WLSMV", 
                     ordered = c("CP1","CP2","CP4","CP5",
                                 "CP7","CP9","CP10","CP12",
                                 "CP13","CP15"))
summary(fit_prosocial, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = fit_prosocial,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")
alpha(PRO_ANA)
reliability(PRO_ANA)
omega(PRO_ANA)
#modelo final-version definitiva de Prosocialidad sin 10
modelo_prosocial2<- '
  ConductaProsocial =~ CP1 + CP2 + CP4 + CP5 +
                       CP7 + CP9 + CP12 +
                       CP13 + CP15
'
fit_prosocial2 <- cfa(modelo_prosocial2, 
                     data = PRO_ANA, 
                     estimator = "WLSMV", 
                     ordered = c("CP1","CP2","CP4","CP5",
                                 "CP7","CP9","CP12",
                                 "CP13","CP15"))
lavaanPlot(model = fit_prosocial2,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")
HIJOS_RGE <- HIJOS[, c("RGE1","RGE2","RGE3","RGE4","RGE5",
                       "RGE6","RGE7","RGE8","RGE9","RGE10")]
REG_IMP<-amelia(HIJOS_RGE, ords =c("RGE1","RGE2","RGE3","RGE4","RGE5",
                                   "RGE6","RGE7","RGE8","RGE9","RGE10"))
REG_ANA<-REG_IMP$imputations$imp3
modelo_RGE <- '
  Reevaluacion =~ RGE1 + RGE3 + RGE5 + RGE7 + RGE8 + RGE10
  Supresion    =~ RGE2 + RGE4 + RGE6 + RGE9
'
fit_RGE <- cfa(modelo_RGE, data = HIJOS_RGE, estimator = "MLR")  
summary(fit_RGE, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = fit_RGE,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")
HIJOS_REE <- REG_ANA[, c("RGE1", "RGE3", "RGE5", "RGE7", "RGE8", "RGE10")]
HIJOS_SUP <- REG_ANA[, c("RGE2", "RGE4", "RGE6", "RGE9")]
alpha(HIJOS_REE)
reliability(HIJOS_REE)
alpha(HIJOS_SUP)
reliability(HIJOS_SUP)
#Modelo_Padres
PAD_DEF <- read_excel("PAD_CONSOLIDADO_DEF.xlsx", na = "999")
PAD_SUB <- PAD_DEF[, c("Edad", "SEX_1_M_2_F", "Estrato",
                       "FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                       "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18")]
PAD_SUB$Edad<-as.numeric(PAD_SUB$Edad)
PAD_IMP <- amelia(PAD_SUB, idvars = c("SEX_1_M_2_F","Estrato"),ords = c("Edad", "FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                       "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18"))
PAD_ANA <- as.data.frame(PAD_IMP$imputations$imp3)
PAD_ANA$FR11R <- 8 - PAD_ANA$FR11
PAD_ANA$FR18R <- 8 - PAD_ANA$FR18
modelo_PAD <- '
  # Factor 1: Prementalización
  Prementalizacion =~ FR1 + FR4 + FR7 + FR10 + FR13 + FR16
  # Factor 2: Certeza en estados mentales
  Certeza =~ FR2 + FR5 + FR8 + FR11R + FR14 + FR17
  # Factor 3: Interés y curiosidad
  InteresCuriosidad =~ FR3 + FR6 + FR9 + FR12 + FR15 + FR18R
'
fit_PAD <- cfa(modelo_PAD, 
               data = PAD_ANA, 
               estimator = "WLSMV", ordered = c("FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                                                  "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18"))               
summary(fit_PAD, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = fit_PAD,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")
PAD_Prementalizacion <- PAD_ANA[, c("FR1","FR4","FR7","FR10","FR13","FR16")]
PAD_Certeza <- PAD_ANA[, c("FR2","FR5","FR8","FR11","FR14","FR17")]
PAD_InteresCuriosidad <- PAD_ANA[, c("FR3","FR6","FR9","FR12","FR15","FR18")]
alpha(PAD_Prementalizacion)
reliability(PAD_Prementalizacion)
alpha(PAD_Certeza)
reliability(PAD_Certeza)
alpha(PAD_InteresCuriosidad)
reliability(PAD_InteresCuriosidad)
#modelo2-FRP
modelo_PAD2<- '
  # Factor 1: Prementalización
  Prementalizacion =~ FR1 + FR4 + FR7 + FR10 + FR13 + FR16
  # Factor 2: Certeza en estados mentales
  Certeza =~ FR2 + FR5 + FR8 + FR14 + FR17
  # Factor 3: Interés y curiosidad
  InteresCuriosidad =~ FR3 + FR6 + FR9 + FR12 + FR15
'
fit_PAD2 <- cfa(modelo_PAD2, 
               data = PAD_ANA, 
               estimator = "WLSMV", ordered = c("FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                                                "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18"))               
summary(fit_PAD2, fit.measures = TRUE, standardized = TRUE)
lavaanPlot(model = fit_PAD2,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")
PAD_Prementalizacion2 <- PAD_ANA[, c("FR1","FR4","FR7","FR10","FR13","FR16")]
PAD_Certeza2 <- PAD_ANA[, c("FR2","FR5","FR8","FR14","FR17")]
PAD_InteresCuriosidad2 <- PAD_ANA[, c("FR3","FR6","FR9","FR12","FR15")]
alpha(PAD_Certeza2)
reliability(PAD_Certeza2)
alpha(PAD_InteresCuriosidad2)
reliability(PAD_InteresCuriosidad2)
PAD_ANA$Premetalizacion   <- rowSums(PAD_Prementalizacion2, na.rm = TRUE)
PAD_ANA$Certeza           <- rowSums(PAD_Certeza2, na.rm = TRUE)
PAD_ANA$InteresCuriosidad <- rowSums(PAD_InteresCuriosidad2, na.rm = TRUE)
wilcox.test(Premetalizacion ~ SEX_1_M_2_F, data = PAD_ANA)
wilcox.test(Certeza ~ SEX_1_M_2_F, data = PAD_ANA)
wilcox.test(InteresCuriosidad ~ SEX_1_M_2_F, data = PAD_ANA)
kruskal.test(Premetalizacion ~ Estrato, data = PAD_ANA)
kruskal.test(Certeza ~ Estrato, data = PAD_ANA)
kruskal.test(InteresCuriosidad ~ Estrato, data = PAD_ANA)


#Diadas
PADRES <- read_excel("PAD_CONSOLIDADO_DEF.xlsx", 
                     na = "999")
HIJOS$llave <- ifelse(!is.na(HIJOS$ID_HIJ), HIJOS$ID_HIJ, HIJOS$N_PAR_HIJ)
PADRES$llave <- ifelse(!is.na(PADRES$ID_HIJ), PADRES$ID_HIJ, PADRES$N_PAR_HIJ)
DIADAS_VF <- merge(
  HIJOS, PADRES,
  by = "llave",
  all = FALSE,                # no incluye filas sin match (evita NA duplicadores)
  suffixes = c("_H", "_P")
)
write_xlsx(DIADAS_VF, "DIADAS.xlsx")
DIADAS_316<- read_excel("DIADAS_06_10.xlsx")
DIADAS_VF$FRP_Prementalizacion <- rowMeans(DIADAS_VF[, c("FR1","FR4","FR7","FR10","FR13","FR16")], na.rm = TRUE)
DIADAS_VF$FRP_Certeza <- rowMeans(DIADAS_VF[, c("FR2","FR5","FR8","FR14","FR17")], na.rm = TRUE)
DIADAS_VF$FRP_InteresCuriosidad <- rowMeans(DIADAS_VF[, c("FR3","FR6","FR9","FR12","FR15")], na.rm = TRUE)
DIADAS_VF$REG_Reevaluacion <- rowMeans(DIADAS_VF[, c("RGE1","RGE3","RGE5","RGE7","RGE8","RGE10")], na.rm = TRUE)
DIADAS_VF$REG_Supresion <- rowMeans(DIADAS_VF[, c("RGE2","RGE4","RGE6","RGE9")], na.rm = TRUE)
DIADAS_VF$Prosocialidad <- rowMeans(DIADAS_VF[, c("CP1","CP2","CP4","CP5","CP7","CP9","CP12","CP13","CP15")], na.rm = TRUE)
DIADAS_PTOT<- DIADAS_VF[, c("FRP_Prementalizacion", 
                            "FRP_Certeza", 
                            "FRP_InteresCuriosidad", 
                            "REG_Reevaluacion", 
                            "REG_Supresion", 
                            "Prosocialidad")]
mvn(DIADAS_PTOT,mvnTest = "mardia", univariateTest = "SW")
modelo_diad <- '
  # Factores latentes a partir de dimensiones
  FRP =~ FRP_Prementalizacion + FRP_Certeza + FRP_InteresCuriosidad
  REG =~ REG_Reevaluacion + REG_Supresion
  REG ~ FRP
  Prosocialidad~ FRP
  Prosocialidad~ REG 
'
modelo_diad_completo <- '
  ###################################
  # PADRES: Factores de Reflexión Parental
  ###################################
  FRP_Prementalizacion =~ FR1 + FR4 + FR7 + FR10 + FR13 + FR16
  FRP_Certeza          =~ FR2 + FR5 + FR8 + FR14 + FR17
  FRP_InteresCuriosidad =~ FR3 + FR6 + FR9 + FR12 + FR15

  ###################################
  # HIJOS: Regulación emocional
  ###################################
  REG_Reevaluacion =~ RGE1 + RGE3 + RGE5 + RGE7 + RGE8 + RGE10
  REG_Supresion    =~ RGE2 + RGE4 + RGE6 + RGE9

  ###################################
  # PROSOCIALIDAD
  ###################################
  Prosocialidad =~ CP1 + CP2 + CP4 + CP5 + CP7 + CP9 + CP12 + CP13 + CP15

  ###################################
  # FACTORES DE SEGUNDO ORDEN (PADRES E HIJOS)
  ###################################
  FRP =~ FRP_Prementalizacion + FRP_Certeza + FRP_InteresCuriosidad
  REG =~ REG_Reevaluacion + REG_Supresion

  ###################################
  # RELACIONES ESTRUCTURALES
  ###################################
  REG ~ FRP
  Prosocialidad ~ FRP + REG
'
DIADAS_316_I <- DIADAS_316[, !names(DIADAS_316) %in% c(
  "FRP_Prementalizacion", "FRP_Certeza", "FRP_InteresCuriosidad",
  "REG_Reevaluacion", "REG_Supresion", "Prosocialidad"
)]
items_modelo <- DIADAS_316_I[, c(
  paste0("FR", 1:17),
  paste0("RGE", 1:10),
  paste0("CP", c(1:15))
)]
items_modelo_scaled <- as.data.frame(scale(items_modelo))
DIADAS_316_scaled <- cbind(DIADAS_316_I[, setdiff(names(DIADAS_316_I), names(items_modelo))],
                           items_modelo_scaled)
MDIAD_1 <- sem(modelo_diad_completo,
               data = DIADAS_316_scaled,
               estimator = "MLR",
               missing = "fiml",
               se = "robust")
# Cargar librería
library(seminr)

# --- MODELO DE MEDICIÓN ---

modelo_diad_mm <- constructs(
  
  # === Padres (Reflexión Parental) ===
  composite("Prementalizacion", multi_items("FR", c(1,4,7,10,13,16))),
  composite("Certeza",          multi_items("FR", c(2,5,8,14,17))),
  composite("InteresCuriosidad",multi_items("FR", c(3,6,9,12,15))),
  
  # === Hijos (Regulación Emocional) ===
  composite("Reevaluacion", multi_items("RGE", c(1,3,5,7,8,10))),
  composite("Supresion",    multi_items("RGE", c(2,4,6,9))),
  
  # === Prosocialidad ===
  composite("Prosocialidad", multi_items("CP", c(1,2,4,5,7,9,12,13,15))),
  
  # === Constructos de orden superior (reflectivo-reflectivo, método two_stage) ===
  higher_composite("FRP",
                   dimensions = c("Prementalizacion", "Certeza", "InteresCuriosidad"),
                   method = two_stage, weights = mode_A),
  
  higher_composite("REG",
                   dimensions = c("Reevaluacion", "Supresion"),
                   method = two_stage, weights = mode_A)
)

# --- MODELO ESTRUCTURAL ---
modelo_diad_sm <- relationships(
  paths(from = "FRP", to = c("REG", "Prosocialidad")),
  paths(from = "REG", to = "Prosocialidad")
)

# --- ESTIMACIÓN ---
MDIAD_pls <- estimate_pls(
  data = DIADAS_316,
  measurement_model = modelo_diad_mm,
  structural_model = modelo_diad_sm
)

# --- RESUMEN ---
summary(MDIAD_pls)

# --- DIAGRAMA DEL MODELO ---
plot(MDIAD_pls)

# --- BOOTSTRAP PARA SIGNIFICANCIA ---
boot_MDIAD <- bootstrap_model(MDIAD_pls, nboot = 1000)
summary(boot_MDIAD)
plot(boot_MDIAD)


summary(MDIAD_1, fit.measures = TRUE, standardized = TRUE)

summary(MDIAD_1, fit.measures = TRUE, standardized = TRUE)
summary(MDIAD_1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
lavaanPlot(model = MDIAD_1,node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE,covs = TRUE, stars = "covs")









fit_final <- sem(modelo_final, 
                 data = merge(PAD_ANA, REG_ANA, PRO_ANA), 
                 estimator = "WLSMV", 
                 ordered = c("FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                             "FR10","FR12","FR13","FR14","FR15","FR16","FR17",
                             "CP1","CP2","CP4","CP5","CP7","CP9","CP12","CP13","CP15",
                             "RGE1","RGE2","RGE3","RGE4","RGE5","RGE6","RGE7","RGE8","RGE9","RGE10"))

# Resumen con ajustes y estandarizados
summary(fit_final, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Visualización
library(lavaanPlot)
lavaanPlot(model = fit_final,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"),
           coefs = TRUE, stand = TRUE, covs = TRUE, stars = "regress")




HI_F_C<- HI_F[!is.na(HI_F$ID_HIJ)   & HI_F$ID_HIJ   != "", ]
DIAD<- merge(PAD_V1_C, HI_F_C, by = "ID_HIJ")
items_PAD <- c(
  "FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
  "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18"
)
items_CP <- c(
  "CP1","CP2","CP3","CP4","CP5","CP6","CP7","CP8","CP9","CP10"
)
items_RGE <- c(
  "RGE1","RGE2","RGE3","RGE4","RGE5","RGE6","RGE7","RGE8","RGE9","RGE10"
)
items_PAD <- paste0("FR", 1:18)
PAD_items <- PAD_V1_C[, items_PAD]
items_CP <- paste0("CP", 1:10)
HI_CP_items <- HI_F_C[, items_CP]
items_RGE <- paste0("RGE", 1:10)
HI_RGE_items <- HI_F_C[, items_RGE]
mvn(PAD_items,mvnTest = "mardia", univariateTest = "SW",   multivariateOutlierMethod = "adj", showOutliers=TRUE)
mvn(HI_RGE_items,mvnTest = "mardia", univariateTest = "SW")
mvn(HI_CP_items,mvnTest = "mardia", univariateTest = "SW")
MODELO<-"
  # Modelo medida
    FR =~ FR1+FR2+FR3+FR4+FR5+FR6+FR7+FR8+FR9+FR10+FR11+FR12+FR13+FR14+FR15+FR16+FR17+FR18 
    CP =~ CP1+CP2+CP3+CP4+CP5+CP6+CP7+CP8+CP9+CP10+CP11+CP12+CP13+CP14+CP15   
    REG=~ RGE1+RGE2+RGE3+RGE4+RGE5+RGE6+RGE7+RGE8+RGE9+RGE10
  # Modelo estructurla
    CP ~ FR
    REG ~ FR
    CP ~ REG"
# Cargar librería
library(seminr)

# --- MODELO DE MEDICIÓN ---

modelo_diad_mm <- constructs(
  
  # === Padres (Reflexión Parental) ===
  composite("Prementalizacion", multi_items("FR", c(1,4,7,10,13,16))),
  composite("Certeza",          multi_items("FR", c(2,5,8,14,17))),
  composite("InteresCuriosidad",multi_items("FR", c(3,6,9,12,15))),
  
  # === Hijos (Regulación Emocional) ===
  composite("Reevaluacion", multi_items("RGE", c(1,3,5,7,8,10))),
  composite("Supresion",    multi_items("RGE", c(2,4,6,9))),
  
  # === Prosocialidad ===
  composite("Prosocialidad", multi_items("CP", c(1,2,4,5,7,9,12,13,15))),
  
  # === Constructos de orden superior (reflectivo-reflectivo, método two_stage) ===
  higher_composite("FRP",
                   dimensions = c("Prementalizacion", "Certeza", "InteresCuriosidad"),
                   method = two_stage, weights = mode_A),
  
  higher_composite("REG",
                   dimensions = c("Reevaluacion", "Supresion"),
                   method = two_stage, weights = mode_A)
)

# --- MODELO ESTRUCTURAL ---
modelo_diad_sm <- relationships(
  paths(from = "FRP", to = c("REG", "Prosocialidad")),
  paths(from = "REG", to = "Prosocialidad")
)

# --- ESTIMACIÓN ---
MDIAD_pls <- estimate_pls(
  data = DIADAS_316,
  measurement_model = modelo_diad_mm,
  structural_model = modelo_diad_sm
)

# --- RESUMEN ---
summary(MDIAD_pls)

# --- DIAGRAMA DEL MODELO ---
plot(MDIAD_pls)

# --- BOOTSTRAP PARA SIGNIFICANCIA ---
boot_MDIAD <- bootstrap_model(MDIAD_pls, nboot = 1000)
summary(boot_MDIAD)
plot(boot_MDIAD)



M1<-sem(MODELO, data = DIAD,missing = "ML")
summary(M1, standardized = TRUE)
lavaanPlot(M1,coefs = TRUE, stars = "covs",stand = TRUE)
fitMeasures(M1) 
scores <- lavPredict(M1)
head(scores)
dim(scores)
DIAD <- cbind(DIAD, scores)
dfSummary(DIAD)
alpha(PAD_items,check.keys=TRUE)
HI_CP_items
alpha(HI_CP_items,check.keys=TRUE)

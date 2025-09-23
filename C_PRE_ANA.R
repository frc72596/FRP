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
#modelo final-version definitiva de Prosocialidad




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
PAD_V1 <- read_excel("PAD_CON_VER1.xlsx", na = "999")
PAD_FRP<- PAD_V1[, paste0("FR", 1:18)]
PAD_IMP <- amelia(PAD_FRP, 
                  ords = c("FR1","FR2","FR3","FR4","FR5","FR6","FR7","FR8","FR9",
                           "FR10","FR11","FR12","FR13","FR14","FR15","FR16","FR17","FR18"))
PAD_ANA <- PAD_IMP$imputations$imp3
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
#Diadas
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

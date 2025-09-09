install.packages("readxl")
library(readxl)
library(summarytools)
library(lavaan)
library(lavaanPlot)
library(MVN)
HI_F <- read_excel("HI_F.xlsx", na = "999")
PAD_V1 <- read_excel("PAD_CON_VER1.xlsx", na = "999")
PAD_V1_C<- PAD_V1[!is.na(PAD_V1$ID_HIJ) & PAD_V1$ID_HIJ != "", ]
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

library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
install.packages("ggpubr")
library(ggpubr)
install.packages("cccrm")
library(cccrm)
install.packages("epiR")
library(epiR)

setwd("Z:/DCIS Projects/DCIS Retrospective Studies/DCIS MRI Active Surveillance- Retrospective/2021 Clinical + Imaging Paper/R Work/Swimmer Plot Imaging Paper")

asMR <- read_excel("ScatterPlot_Concordance.xlsx")

plot(x = jitter(asMR$`Radiologist 1 Q1`), y = jitter(asMR$`Radiologist 2 Q1`), main = "Radiologist Read Distribution Timepoint 0", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) 
+ points(jitter(asMR$`Radiologist 1 Q2`), jitter(asMR$`Radiologist 2 Q2`), col = 'blue', pch = 1) 
+ points(jitter(asMR$`Radiologist 1 Q3`), jitter(asMR$`Radiologist 2 Q3`), col = 'green', pch = 2)
+ points(jitter(asMR$`Radiologist 1 Q4`), jitter(asMR$`Radiologist 2 Q4`), col = 'pink', pch = 3)
+ points(jitter(asMR$`Radiologist 1 Q5`), jitter(asMR$`Radiologist 2 Q5`), col = 'orange', pch = 4)
+ abline(0,1)


asMR1 <- read_excel("ScatterPlot_Concordance.xlsx", sheet = 2)

plot(x = jitter(asMR1$`Radiologist 1 Q1`), y = jitter(asMR1$`Radiologist 2 Q1`), main = "Radiologist Read Distribution Timepoint 1", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) 
+ points(jitter(asMR1$`Radiologist 1 Q2`), jitter(asMR1$`Radiologist 2 Q2`), col = 'blue', pch = 1) 
+ points(jitter(asMR1$`Radiologist 1 Q3`), jitter(asMR1$`Radiologist 2 Q3`), col = 'green', pch = 2)
+points(jitter(asMR1$`Radiologist 1 Q4`), jitter(asMR1$`Radiologist 2 Q4`), col = 'pink', pch = 3)
+ points(jitter(asMR1$`Radiologist 1 Q5`), jitter(asMR1$`Radiologist 2 Q5`), col = 'orange', pch = 4)
+ points(jitter(asMR1$`Radiologist 1 Q6`), jitter(asMR1$`Radiologist 2 Q6`), col = 'grey', pch = 5) 
+ points(jitter(asMR1$`Radiologist 1 Q7`), jitter(asMR1$`Radiologist 2 Q7`), col = 'purple', pch = 6)
+ points(jitter(asMR1$`Radiologist 1 Q8`), jitter(asMR1$`Radiologist 2 Q8`), col = 'brown', pch = 7)
+ points(jitter(asMR1$`Radiologist 1 Q9`), jitter(asMR1$`Radiologist 2 Q9`), col = 'black', pch = 8)
+ abline(0,1)

asMR2 <- read_excel("ScatterPlot_Concordance.xlsx", sheet = 3)

plot(x = jitter(asMR2$`Radiologist 1 Q1`), y = jitter(asMR2$`Radiologist 2 Q1`), main = "Radiologist Read Distribution Timepoint 2", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) 
+ points(jitter(asMR2$`Radiologist 1 Q2`), jitter(asMR2$`Radiologist 2 Q2`), col = 'blue', pch = 1) 
+ points(jitter(asMR2$`Radiologist 1 Q3`), jitter(asMR2$`Radiologist 2 Q3`), col = 'green', pch = 2)
+points(jitter(asMR2$`Radiologist 1 Q4`), jitter(asMR2$`Radiologist 2 Q4`), col = 'pink', pch = 3)
+ points(jitter(asMR2$`Radiologist 1 Q5`), jitter(asMR2$`Radiologist 2 Q5`), col = 'orange', pch = 4)
+ points(jitter(asMR2$`Radiologist 1 Q6`), jitter(asMR2$`Radiologist 2 Q6`), col = 'grey', pch = 5) 
+ points(jitter(asMR2$`Radiologist 1 Q7`), jitter(asMR2$`Radiologist 2 Q7`), col = 'purple', pch = 6)
+ points(jitter(asMR2$`Radiologist 1 Q8`), jitter(asMR2$`Radiologist 2 Q8`), col = 'brown', pch = 7)
+ points(jitter(asMR2$`Radiologist 1 Q9`), jitter(asMR2$`Radiologist 2 Q9`), col = 'black', pch = 8)
+ abline(0,1)





bpeIpsilateral <- plot(x = jitter(asMR$`Radiologist 1 Q2`), y = jitter(asMR$`Radiologist 2 Q2`), main = "Radiologist Read Distribution for BPE", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) + 
  points(jitter(asMR1$`Radiologist 1 Q3`), jitter(asMR1$`Radiologist 2 Q3`), col = 'blue', pch = 1) + 
  points(jitter(asMR2$`Radiologist 1 Q3`), jitter(asMR2$`Radiologist 2 Q3`), col = 'green', pch = 2) + abline(0,1)

changeInBPE <- plot(x = jitter(asMR1$`Radiologist 1 Q4`), y = jitter(asMR1$`Radiologist 2 Q4`), main = "Radiologist Read Distribution for Change in BPE", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'blue', pch = 1) + 
   points(jitter(asMR2$`Radiologist 1 Q4`), jitter(asMR2$`Radiologist 2 Q4`), col = 'green', pch = 2) + abline(0,1)

lesion <- plot(x = jitter(asMR$`Radiologist 1 Q4`), y = jitter(asMR$`Radiologist 2 Q4`), main = "Radiologist Read Distribution for Lesion Distinctness", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) + 
  points(jitter(asMR1$`Radiologist 1 Q6`), jitter(asMR1$`Radiologist 2 Q6`), col = 'blue', pch = 1) + 
  points(jitter(asMR2$`Radiologist 1 Q6`), jitter(asMR2$`Radiologist 2 Q6`), col = 'green', pch = 2) + abline(0,1)

changeInLesion <- plot(x = jitter(asMR1$`Radiologist 1 Q7`), y = jitter(asMR1$`Radiologist 2 Q7`), main = "Radiologist Read Distribution for Change in Lesion", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'blue', pch = 1) + 
  points(jitter(asMR2$`Radiologist 1 Q7`), jitter(asMR2$`Radiologist 2 Q7`), col = 'green', pch = 2) + abline(0,1)

baselineornewinvasive <- plot(x = jitter(asMR$`Radiologist 1 Q5`), y = jitter(asMR$`Radiologist 2 Q5`), main = "Radiologist Read Distribution new invasive cancer", xlab = "Radiologist 1 Answers", ylab = "Radiologist 2 Answers", col = 'red', pch = 0) + 
  points(jitter(asMR1$`Radiologist 1 Q9`), jitter(asMR1$`Radiologist 2 Q9`), col = 'blue', pch = 1) + 
  points(jitter(asMR2$`Radiologist 1 Q9`), jitter(asMR2$`Radiologist 2 Q9`), col = 'green', pch = 2) + abline(0,1)






#FRESH


#Putting figures together

par(mfrow=c(3,3))

#Organizing Data for first, second and third MRI for ipsilateral BPE

mrR1BPE <- asMR$`Radiologist 1 Q2`
mrR2BPE <- asMR$`Radiologist 2 Q2`

mr1R1BPE <- asMR1$`Radiologist 1 Q3`
mr1R2BPE <- asMR1$`Radiologist 2 Q3`

mr2R1BPE <- asMR2$`Radiologist 1 Q3`
mr2R2BPE <- asMR2$`Radiologist 2 Q3`

plot(x = jitter(mrR1BPE), y = jitter(mrR2BPE), xlim = c(0,5), ylim = c(0,5), main = "BPE (Ipsilateral Breast)", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'red', pch = 0) 
points(x = jitter(mr1R1BPE), y = jitter(mr1R2BPE), col = 'blue', pch = 1) 
points(jitter(mr2R1BPE), jitter(mr2R2BPE), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR0", "MR1", "MR2"), pch = c(0, 1, 2), col = c("red", "blue", "darkgreen"), cex = .5)
text(x = c(1,1,1,1), y = c(5,4.75,4.5,4.25), labels = c("Percent Agreement:", "MR0 = 46.03%", "MR1 = 65.08%", "MR2 = 68.08%"), col = c("black", "red", "blue", "darkgreen"))


cor(mrR1BPE,mrR2BPE, use = "complete.obs", method = "spearman")
cor(mr1R1BPE, mr1R2BPE, use = "complete.obs", method = "spearman")
cor(mr2R1BPE, mr2R2BPE, use = "complete.obs", method = "spearman")

epi.ccc(mrR1BPE,mrR2BPE)
epi.ccc(mr1R1BPE, mr1R2BPE)
epi.ccc(mr2R1BPE, mr2R2BPE)

#Organizing Data for first, second MRI for change in BPE

mr1R1CBPE <- asMR1$`Radiologist 1 Q4`
mr1R2CBPE <- asMR1$`Radiologist 2 Q4`

mr2R1CBPE <- asMR2$`Radiologist 1 Q4`
mr2R2CBPE <- asMR2$`Radiologist 2 Q4`

plot(x = jitter(mr1R1CBPE), y = jitter(mr1R2CBPE), xlim = c(0,5), ylim = c(0,5), main = "Change in BPE (Ipsilateral Breast)", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'blue', pch = 1)
points(jitter(mr2R1CBPE), jitter(mr2R2CBPE), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR1", "MR2"), pch = c(1, 2), col = c("blue", "darkgreen"), cex = .5)
text(x = c(1,1,1), y = c(5,4.75,4.5), labels = c("Percent Agreement:", "MR1 = 84.13%", "MR2 = 61.70%"), col = c("black", "blue", "darkgreen"))

cor(mr1R1CBPE, mr1R2CBPE, use = "complete.obs", method = "spearman")
cor(mr2R1CBPE, mr2R2CBPE, use = "complete.obs", method = "spearman")

#organizing for distinct lesion

mrR1Lesion <- asMR$`Radiologist 1 Q4`
mrR2Lesion <- asMR$`Radiologist 2 Q4`

mr1R1Lesion <- asMR1$`Radiologist 1 Q6`
mr1R2Lesion <- asMR1$`Radiologist 2 Q6`

mr2R1Lesion <- asMR2$`Radiologist 1 Q6`
mr2R2Lesion <- asMR2$`Radiologist 2 Q6`

plot(x = jitter(mrR1Lesion), y = jitter(mrR2Lesion), xlim = c(0,5), ylim = c(0,5), main = "Lesion Distinctness", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'red', pch = 0) 
points(x = jitter(mr1R1Lesion), y = jitter(mr1R2Lesion), col = 'blue', pch = 1) 
points(jitter(mr2R1Lesion), jitter(mr2R2Lesion), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR0", "MR1", "MR2"), pch = c(0, 1, 2), col = c("red", "blue", "darkgreen"), cex = .5)
text(x = c(1,1,1,1), y = c(5,4.75,4.5,4.25), labels = c("Percent Agreement:", "MR0 = 65.07%", "MR1 = 68.25%", "MR2 = 63.83%"), col = c("black", "red", "blue", "darkgreen"))


cor(mrR1Lesion,mrR2Lesion, use = "complete.obs", method = "spearman")
cor(mr1R1Lesion, mr1R2Lesion, use = "complete.obs", method = "spearman")
cor(mr2R1Lesion, mr2R2Lesion, use = "complete.obs", method = "spearman")

#Organizing Data for first, second MRI for change in Lesion

mr1R1CLesion <- asMR1$`Radiologist 1 Q7`
mr1R2CLesion <- asMR1$`Radiologist 2 Q7`

mr2R1CLesion <- asMR2$`Radiologist 1 Q7`
mr2R2CLesion <- asMR2$`Radiologist 2 Q7`

plot(x = jitter(mr1R1CLesion), y = jitter(mr1R2CLesion), xlim = c(0,5), ylim = c(0,5), main = "Change in Lesion Distinctness", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'blue', pch = 1)
points(jitter(mr2R1CLesion), jitter(mr2R2CLesion), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR1", "MR2"), pch = c(1, 2), col = c("blue", "darkgreen"), cex = .5)
text(x = c(1,1,1), y = c(5,4.75,4.5), labels = c("Percent Agreement:", "MR1 = 77.78%", "MR2 = 78.95%"), col = c("black", "blue", "darkgreen"))

cor(mr1R1CLesion, mr1R2CLesion, use = "complete.obs", method = "spearman")
cor(mr2R1CLesion, mr2R2CLesion, use = "complete.obs", method = "spearman")



#Organizing Data for first, second and third MRI for Contralateral BPE

mrR1ContBPE <- asMR$`Radiologist 1 Q1`
mrR2ContBPE <- asMR$`Radiologist 2 Q1`

mr1R1ContBPE <- asMR1$`Radiologist 1 Q1`
mr1R2ContBPE <- asMR1$`Radiologist 2 Q1`

mr2R1ContBPE <- asMR2$`Radiologist 1 Q1`
mr2R2ContBPE <- asMR2$`Radiologist 2 Q1`

plot(x = jitter(mrR1ContBPE), y = jitter(mrR2ContBPE), xlim = c(0,5), ylim = c(0,5), main = "BPE (Contralateral Breast)", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'red', pch = 0) 
points(x = jitter(mr1R1ContBPE), y = jitter(mr1R2ContBPE), col = 'blue', pch = 1) 
points(jitter(mr2R1ContBPE), jitter(mr2R2ContBPE), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR0", "MR1", "MR2"), pch = c(0, 1, 2), col = c("red", "blue", "darkgreen"), cex = .5)
text(x = c(1,1,1,1), y = c(5,4.75,4.5,4.25), labels = c("Percent Agreement:", "MR0 = 52.94%", "MR1 = 59.26%", "MR2 = 65.00%"), col = c("black", "red", "blue", "darkgreen"))


cor(mrR1ContBPE,mrR2ContBPE, use = "complete.obs", method = "spearman")
cor(mr1R1ContBPE, mr1R2ContBPE, use = "complete.obs", method = "spearman")
cor(mr2R1ContBPE, mr2R2ContBPE, use = "complete.obs", method = "spearman")



#Organizing Data for first, second MRI for change in contralateral BPE

mr1R1CContBPE <- asMR1$`Radiologist 1 Q2`
mr1R2CContBPE <- asMR1$`Radiologist 2 Q2`

mr2R1CContBPE <- asMR2$`Radiologist 1 Q2`
mr2R2CContBPE <- asMR2$`Radiologist 2 Q2`

plot(x = jitter(mr1R1CContBPE), y = jitter(mr1R2CContBPE), xlim = c(0,5), ylim = c(0,5), main = "Change in BPE (Contralateral Breast)", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'blue', pch = 1)
points(jitter(mr2R1CContBPE), jitter(mr2R2CContBPE), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR1", "MR2"), pch = c(1, 2), col = c("blue", "darkgreen"), cex = .5)
text(x = c(1,1,1), y = c(5,4.75,4.5), labels = c("Percent Agreement:", "MR1 = 80.39%", "MR2 = 77.50%"), col = c("black", "blue", "darkgreen"))

cor(mr1R1CContBPE, mr1R2CContBPE, use = "complete.obs", method = "spearman")
cor(mr2R1CContBPE, mr2R2CContBPE, use = "complete.obs", method = "spearman")



#Organizing Data for first, second and third MRI for sym BPE

mrR1SBPE <- asMR$`Radiologist 1 Q3`
mrR2SBPE <- asMR$`Radiologist 2 Q3`

mr1R1SBPE <- asMR1$`Radiologist 1 Q5`
mr1R2SBPE <- asMR1$`Radiologist 2 Q5`

mr2R1SBPE <- asMR2$`Radiologist 1 Q5`
mr2R2SBPE <- asMR2$`Radiologist 2 Q5`

plot(x = jitter(mrR1SBPE), y = jitter(mrR2SBPE), xlim = c(0,5), ylim = c(0,5), main = "BPE Symmetry", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'red', pch = 0) 
points(x = jitter(mr1R1SBPE), y = jitter(mr1R2SBPE), col = 'blue', pch = 1) 
points(jitter(mr2R1SBPE), jitter(mr2R2SBPE), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR0", "MR1", "MR2"), pch = c(0, 1, 2), col = c("red", "blue", "darkgreen"), cex = .5)
text(x = c(1,1,1,1), y = c(5,4.75,4.5,4.25), labels = c("Percent Agreement:", "MR0 = 88.23%", "MR1 = 83.01%", "MR2 = 87.50%"), col = c("black", "red", "blue", "darkgreen"))


cor(mrR1SBPE,mrR2SBPE, use = "complete.obs", method = "spearman")
cor(mr1R1SBPE, mr1R2SBPE, use = "complete.obs", method = "spearman")
cor(mr2R1SBPE, mr2R2SBPE, use = "complete.obs", method = "spearman")


#Organizing Data for first, second and third MRI for IDC liklihood

mrR1LIC <- asMR$`Radiologist 1 Q5`
mrR2LIC <- asMR$`Radiologist 2 Q5`

mr1R1LIC <- asMR1$`Radiologist 1 Q9`
mr1R2LIC <- asMR1$`Radiologist 2 Q9`

mr2R1LIC <- asMR2$`Radiologist 1 Q9`
mr2R2LIC <- asMR2$`Radiologist 2 Q9`

plot(x = jitter(mrR1LIC), y = jitter(mrR2LIC), xlim = c(0,5), ylim = c(0,5), main = "Likelihood of Invasive Cancer", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'red', pch = 0) 
points(x = jitter(mr1R1LIC), y = jitter(mr1R2LIC), col = 'blue', pch = 1) 
points(jitter(mr2R1LIC), jitter(mr2R2LIC), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR0", "MR1", "MR2"), pch = c(0, 1, 2), col = c("red", "blue", "darkgreen"), cex = .5)
text(x = c(1,1,1,1), y = c(5,4.75,4.5,4.25), labels = c("Percent Agreement:", "MR0 = 66.66%", "MR1 = 88.71%", "MR2 = 91.49%"), col = c("black", "red", "blue", "darkgreen"))


cor(mrR1LIC,mrR2LIC, use = "complete.obs", method = "spearman")
cor(mr1R1LIC, mr1R2LIC, use = "complete.obs", method = "spearman")
cor(mr2R1LIC, mr2R2LIC, use = "complete.obs", method = "spearman")
epi.ccc(mr2R1LIC, mr2R2LIC)


#Organizing Data for first, second MRI for change in New or Progressed DCIS

mr1R1NPD <- asMR1$`Radiologist 1 Q8`
mr1R2NPD <- asMR1$`Radiologist 2 Q8`

mr2R1NPD <- asMR2$`Radiologist 1 Q8`
mr2R2NPD <- asMR2$`Radiologist 2 Q8`

plot(x = jitter(mr1R1NPD), y = jitter(mr1R2NPD), xlim = c(0,5), ylim = c(0,5), main = "New or progressed DCIS", xlab = "Radiologist 1", ylab = "Radiologist 2", col = 'blue', pch = 1)
points(jitter(mr2R1NPD), jitter(mr2R2NPD), col = 'darkgreen', pch = 2) 
abline(0,1)
legend(x = "bottomright", legend = c("MR1", "MR2"), pch = c(1, 2), col = c("blue", "darkgreen"), cex = .5)
text(x = c(1,1,1), y = c(5,4.75,4.5), labels = c("Percent Agreement:", "MR1 = 87.09%", "MR2 = 87.23%"), col = c("black", "blue", "darkgreen"))

cor(mr1R1NPD, mr1R2NPD, use = "complete.obs", method = "spearman")
cor(mr2R1NPD, mr2R2NPD, use = "complete.obs", method = "spearman")


epi.ccc(mr2R1NPD, mr2R2NPD)


#Percent concordance for MR 0

sum(asMR$`Radiologist 1 Q1` == asMR$`Radiologist 2 Q1`, na.rm = TRUE)/length(asMR$`Radiologist 1 Q1`[!is.na(asMR$`Radiologist 1 Q1`)])

sum(asMR$`Radiologist 1 Q2` == asMR$`Radiologist 2 Q2`, na.rm = TRUE)/length(asMR$`Radiologist 1 Q2`[!is.na(asMR$`Radiologist 1 Q2`)])

sum(asMR$`Radiologist 1 Q3` == asMR$`Radiologist 2 Q3`, na.rm = TRUE)/length(asMR$`Radiologist 1 Q3`[!is.na(asMR$`Radiologist 1 Q3`)])

sum(asMR$`Radiologist 1 Q4` == asMR$`Radiologist 2 Q4`, na.rm = TRUE)/length(asMR$`Radiologist 1 Q4`[!is.na(asMR$`Radiologist 1 Q4`)])

sum(asMR$`Radiologist 1 Q5` == asMR$`Radiologist 2 Q5`, na.rm = TRUE)/length(asMR$`Radiologist 1 Q5`[!is.na(asMR$`Radiologist 1 Q5`)])


#percent concordance for MR1

sum(asMR1$`Radiologist 1 Q1` == asMR1$`Radiologist 2 Q1`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q1`[!is.na(asMR1$`Radiologist 1 Q1`)])

sum(asMR1$`Radiologist 1 Q2` == asMR1$`Radiologist 2 Q2`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q2`[!is.na(asMR1$`Radiologist 1 Q2`)])

sum(asMR1$`Radiologist 1 Q3` == asMR1$`Radiologist 2 Q3`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q3`[!is.na(asMR1$`Radiologist 1 Q3`)])

sum(asMR1$`Radiologist 1 Q4` == asMR1$`Radiologist 2 Q4`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q4`[!is.na(asMR1$`Radiologist 1 Q4`)])

sum(asMR1$`Radiologist 1 Q5` == asMR1$`Radiologist 2 Q5`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q5`[!is.na(asMR1$`Radiologist 1 Q5`)])

sum(asMR1$`Radiologist 1 Q6` == asMR1$`Radiologist 2 Q6`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q6`[!is.na(asMR1$`Radiologist 1 Q6`)])

sum(asMR1$`Radiologist 1 Q7` == asMR1$`Radiologist 2 Q7`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q7`[!is.na(asMR1$`Radiologist 1 Q7`)])

sum(asMR1$`Radiologist 1 Q8` == asMR1$`Radiologist 2 Q8`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q8`[!is.na(asMR1$`Radiologist 1 Q8`)])

sum(asMR1$`Radiologist 1 Q9` == asMR1$`Radiologist 2 Q9`, na.rm = TRUE)/length(asMR1$`Radiologist 1 Q9`[!is.na(asMR1$`Radiologist 1 Q9`)])

#percent concordance for MR 2

sum(asMR2$`Radiologist 1 Q1` == asMR2$`Radiologist 2 Q1`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q1`[!is.na(asMR2$`Radiologist 1 Q1`)])

sum(asMR2$`Radiologist 1 Q2` == asMR2$`Radiologist 2 Q2`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q2`[!is.na(asMR2$`Radiologist 1 Q2`)])

sum(asMR2$`Radiologist 1 Q3` == asMR2$`Radiologist 2 Q3`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q3`[!is.na(asMR2$`Radiologist 1 Q3`)])

sum(asMR2$`Radiologist 1 Q4` == asMR2$`Radiologist 2 Q4`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q4`[!is.na(asMR2$`Radiologist 1 Q4`)])

sum(asMR2$`Radiologist 1 Q5` == asMR2$`Radiologist 2 Q5`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q5`[!is.na(asMR2$`Radiologist 1 Q5`)])

sum(asMR2$`Radiologist 1 Q6` == asMR2$`Radiologist 2 Q6`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q6`[!is.na(asMR2$`Radiologist 1 Q6`)])

sum(asMR2$`Radiologist 1 Q7` == asMR2$`Radiologist 2 Q7`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q7`[!is.na(asMR2$`Radiologist 1 Q7`)])

sum(asMR2$`Radiologist 1 Q8` == asMR2$`Radiologist 2 Q8`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q8`[!is.na(asMR2$`Radiologist 1 Q8`)])

sum(asMR2$`Radiologist 1 Q9` == asMR2$`Radiologist 2 Q9`, na.rm = TRUE)/length(asMR2$`Radiologist 1 Q9`[!is.na(asMR2$`Radiologist 1 Q9`)])


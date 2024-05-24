library(dplyr)
#install.packages("ggpubr")
library("ggpubr")
library("car")
library(tidyverse)
#install.packages("ggthemes")
library("ggthemes")
#install.packages("multcompView")
library(multcompView)
#install.packages("plotrix")
library("plotrix")

#Read in P fractionation data
Data <- read.csv("Pfractionation_prop_comparefrac.csv")
VALUE <- "Conc"
str(Data)
head(Data)

#Summarise
group_by(Data, Stage) %>% summarise(count = n(), mean = mean(Conc, na.rm = TRUE), sd = sd(Conc, na.rm = TRUE))

#Boxplot
ggboxplot(Data, x = "Stage", y = VALUE, ylab = VALUE, xlab = "Stage")

#ANOVA
Data_anv <- aov(Data$Conc ~ Data$Stage)
summary(Data_anv)
Data_anv_tuk <- TukeyHSD(Data_anv)

#Letters for significance
cld <- multcompLetters4(Data_anv, Data_anv_tuk)
dt <- group_by(Data, Stage) %>% summarise(w=mean(Conc), se = std.error(Conc)) %>% arrange(desc(w))
cld <- as.data.frame.list(cld$Stage)
dt$cld <- cld$Letters
dt %>% arrange(Stage)

##Tests

#Bartlett test for variance homogeneity (p-value must be above 0.05)
bartlett.test(Conc ~ Stage, data = Data)
leveneTest(Conc ~ Stage, data = Data)

#Normality test (p-value must be above 0.05)
Data_anv_res <- residuals(object = Data_anv)
shapiro.test(x = Data_anv_res)

##Graphs

#Graph with letters
ggplot(dt, aes(Stage, w)) + 
  geom_bar(stat = "identity", aes(fill = w), show.legend = FALSE) +
  geom_errorbar(aes(ymin = w-se, ymax=w+se), width = 0.2) +
  labs(x = "", y = expression(paste("Foliar N ", g^-1, (DW)))) +
  geom_text(aes(label = cld, y = w + se), vjust = -0.5) +
  theme_few()

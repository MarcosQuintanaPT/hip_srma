#Load package
library(meta)
library(readxl)


#Import dataframe
MetaGroin <- read_excel("C:/Users/Marcos/OneDrive/Escritorio/MetaGroin.xlsx")


#Filter dataframe for different meta-analyses
dfaddall <- subset(MetaGroin, Maok == 1)
dfaddtl <- subset(MetaGroin, Maok == 2)
dfabdall <- subset(MetaGroin, Maok == 3)
dfabdtl <- subset(MetaGroin, Maok == 4)
dfratio <- subset(MetaGroin, Maok == 5)
dfratiotl <- subset(MetaGroin, Maok == 6)
dfInfluall <- subset(MetaGroin, Mainflu == 1)
dfInflutl <- subset(MetaGroin, Mainflu == 2)


#Adduction strength
#Meta-analysis for adduction strength (all injuries)
meta1 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                                 data = dfaddall, sm = "SMD", method.smd = "Cohen",
                      common = FALSE, prediction = TRUE)

summary(meta1)

#Meta-analysis for adduction strength (all injuries-influence analysis)
metainf <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfInfluall, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(metainf)

#Meta-analysis for adduction strength (time-loss injuries)
meta2 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfaddtl, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(meta2)

#Meta-analysis for adduction strength (time-loss injuries-influence analysis)
metainftl <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfInflutl, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(metainftl)


#Abduction strength
#Meta-analysis for abduction strength (all injuries)
meta3 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfabdall, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(meta3)

#Meta-analysis for abduction strength (time-loss injuries)
meta4 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfabdtl, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(meta4)


#Adduction to abduction strength ratio
#Meta-analysis for adduction to abduction strength ratio (all injuries)
meta5 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfratio, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE)

summary(meta5)

#Meta-analysis for adduction to abduction strength ratio (time-loss injuries)
meta6 <- metacont(nI, mI, sdI, nUI, mUI, sdUI, studlab = Study, 
                  data = dfratiotl, sm = "SMD", method.smd = "Cohen",
                  common = FALSE, prediction = TRUE) #No prediction interval is calculated
                                                      
summary(meta6)


#Meta-regression
#Meta-regression with all injuries (adduction strength and age)
mreg <- metareg(meta1, Age)

summary(mreg)

#Meta-regression with all injuries (adduction strength and age-influence analysis)
mreginflu <- metareg(metainf, Age)

summary(mreginflu)

#Meta-regression with all injuries (adduction strength and diagnosis)
mregdx <- metareg(meta1, Diagnosis) #Secomb et al. not included (Unspecified diagnosis)

summary(mregdx)

#Meta-regression with all injuries (adduction strength and diagnosis-influence analysis)
mregdxinflu <- metareg(metainf, Diagnosis) #Secomb et al. not included (Unspecified diagnosis)

summary(mregdxinflu)

#Visualizations
#Forest plots
#Filter dataframe for different meta-analyses
Forestadd <- subset(MetaGroin, Forest == 1)
Forestabd <- subset(MetaGroin, Forest == 2)
Forestratio <- subset(MetaGroin, Forest == 3)

#Adduction strength
Fpadd <- metacont(nI, mI, sdI, nUI, mUI, sdUI, data = Forestadd,
                   sm = "SMD", method.smd = "Cohen", random = TRUE,
                   common = FALSE, subgroup = Type)

Fig_3 <- forest(Fpadd, lty.random = NULL, studlab = Study, col.subgroup = "black",
       col.square = "skyblue3", col.square.lines = "skyblue3",
       col.diamond = "grey70", digits.mean = 2,
       digits.sd = 2, print.subgroup.name = FALSE, overall.hetstat = FALSE,
       overall = FALSE, test.subgroup = FALSE, smlab = "",
       label.c = "Uninjured", label.e = "Injured")

#Abduction strength
Fpabd <- metacont(nI, mI, sdI, nUI, mUI, sdUI, data = Forestabd,
                  sm = "SMD", method.smd = "Cohen", random = TRUE,
                  common = FALSE, subgroup = Type)

Fig_4 <- forest(Fpabd, lty.random = NULL, studlab = Study, col.subgroup = "black",
                col.square = "skyblue3", col.square.lines = "skyblue3",
                col.diamond = "grey70", digits.mean = 2,
                digits.sd = 2, print.subgroup.name = FALSE, overall.hetstat = FALSE,
                overall = FALSE, test.subgroup = FALSE, smlab = "",
                label.c = "Uninjured", label.e = "Injured")

#Adduction to abduction strength ratio
Fpratio <- metacont(nI, mI, sdI, nUI, mUI, sdUI, data = Forestratio,
                  sm = "SMD", method.smd = "Cohen", random = TRUE,
                  common = FALSE, subgroup = Type)

Fig_5 <- forest(Fpratio, lty.random = NULL, studlab = Study, col.subgroup = "black",
                col.square = "skyblue3", col.square.lines = "skyblue3",
                col.diamond = "grey70", digits.mean = 2,
                digits.sd = 2, print.subgroup.name = FALSE, overall.hetstat = FALSE,
                overall = FALSE, test.subgroup = FALSE, smlab = "",
                label.c = "Uninjured", label.e = "Injured")

#Bubble plots
#Load packages
library(ggplot2)
library(cowplot)

#Overall meta-reg adductors
bublreg1 <- ggplot(MetaGroin, aes(x= Age_bub, y= ES, size = Siz)) +
  scale_size(range = c(3, 18))+
  geom_smooth(method = "lm", se = TRUE, color = "black",
              fill = "grey75", alpha = 0.5, size = 1.05, fullrange = TRUE)+
  geom_point(color = "grey35", alpha = 0.7) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none") +
  labs(x = "Age (years)", y = "All injuries (SMD)") +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  expand_limits(x = c(12, 28)) +
  scale_y_continuous(expand = c(0,0))+
  ylim(-2.5, 1.1)


#Adjusted meta-reg
bublreg2 <- ggplot(MetaGroin, aes(x= Age_bubadj, y= ES_adj, size = Siz_adj)) +
  scale_size(range = c(3, 18))+
  geom_smooth(method = "lm", se = TRUE, color = "black",
              fill = "grey75", alpha = 0.5, size = 1.05, fullrange = TRUE)+
  geom_point(color = "grey35", alpha = 0.7) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.position = "none") +
  labs(x = "Age (years)", y = "Influence Analysis (SMD)") +
  geom_hline(yintercept = 0, size = 0.5, linetype = "dashed") +
  scale_x_continuous(expand = c(0, 0)) +
  expand_limits(x = c(12, 28)) +
  scale_y_continuous(expand = c(0,0))+
  ylim(-2.5, 1.1)


#Combine plots
Fig_6 <- plot_grid(bublreg1, bublreg2,
                labels = "auto", label_size = 15)
Fig_6











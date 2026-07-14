############################################################################
########### IGAMon-Dog: Code online training both cohorts, Q1+Q2 ############
############################################################################

# required packages
require(MuMIn)
options(na.action = "na.fail")
require(GGally)
require(car)
require(effects)
require(dplyr)
require(ggplot2)
require(ggeffects)
require(ggpubr)
require(performance)
library(patchwork)
library(cowplot)


# Data preparation ####
## Read data ####
### cohort 1, 2021 ####
training21 <- read.csv2("online_cohort1.csv", stringsAsFactors = TRUE)
head(training21)
str(training21)
training21$Team <- as.factor(training21$Team) 
training21$FCI_Gruppe <- as.factor(training21$FCI_Gruppe)
training21$Erfahrung <- as.factor(training21$Erfahrung) 

### cohort 2, 2022 ####
training22 <- read.csv2("online_cohort2.csv", stringsAsFactors = TRUE)
head(training22) 
str(training22) 
training22$Team <- as.factor(training22$Team) 
training22$FCI_Gruppe <- as.factor(training22$FCI_Gruppe)
training22$Erfahrung <- as.factor(training22$Erfahrung)

## Merge both cohorts ####
# some pre-work for harmonisation and initial checks
training22$Zielobjekt <- "Pflanze"

names(training21) == names(training22)
colnames(training21)[9] <- colnames(training22)[9]

# Harmonise difference in training time as the number of steps was different among cohorts
training21$sc.Differenz_Durchschnitt <- scale(training21$Differenz_Durchschnitt)
training22$sc.Differenz_Durchschnitt <- scale(training22$Differenz_Durchschnitt)

summary(training21$sc.Differenz_Durchschnitt)
summary(training22$sc.Differenz_Durchschnitt)

summary(training21$Differenz_Durchschnitt)
summary(training22$Differenz_Durchschnitt)

training21$Kohorte <- "C1"
training22$Kohorte <- "C2"

# merge both
training <- bind_rows(training21, training22)
head(training)
tail(training)

# some checks
names(training) == names(training21)
names(training) == names(training22)

dim(training)
dim(training21)
dim(training22)

# all true

# one dogs usually sits (according to video) but very would occasionally stay or lay --> put to sit to avoid single observation per factor
training$Anzeigeverhalten[training$Anzeigeverhalten == "wechselt"] <- "sitzt"
training$Anzeigeverhalten <- droplevels(training$Anzeigeverhalten)

str(training)
training$Zielobjekt <- as.factor(training$Zielobjekt)
training$Kohorte <- as.factor(training$Kohorte)

# was the scaling of the time difference successful?
plot(training$sc.Differenz_Durchschnitt ~ training$Differenz_Durchschnitt)
# yes

## check parameter distribution and correlation ####
# only take parameters in analyses that truely change 
# parameter for analyses indicated by "yes"

table(training$Belohnung) # reward: usually correct "korrekt"
table(training$Anzeige) # alert: usually true positive "rp"
table(training$FCI_Gruppe)  # FCI (dog breed group): yes
table(training$Rasse) # too many, no
table(training$Alter) # dog age: yes
table(training$Stoerreiz) # distraction: yes
table(training$Erfahrung) # experience: yes
table(training$Verstaerkerwahl) # reward type: yes
table(training$Anzeigeverhalten)  # alert behaviour: yes, if possible
table(training$Zielart) # target species: yes, where needed
table(training$MindestdauerAnzeige) # alert duration: only relevant to define whether target goal was achieved

# select dataset when target goal was achieved, as Q1 is only meaningful when target goal was achieved
training_1 <- training[training$Ziel_erreicht==1,]
nrow(training)  # 203
nrow(training_1)  # 179

# check correlation
ggpairs(training_1[,c(6,9,11,13,15,16,18,20)]) 
### Training step and distraction may be correlated, vifs need to be checked

# parameter set:
## training step = Trainingsschritt
## Team
## Dog age = Alter
## FCI breed group = FCI-Gruppe
## Team experience = Erfahrung
## Reward type = Verstaerkerwahl
## maybe Distraction = Stoerreiz


# (Q1) Which social and training factors influenced how quickly training goals were achieved ####

names(training_1) 

mean(training_1$sc.Differenz_Durchschnitt)
sd(training_1$sc.Differenz_Durchschnitt)
hist(training_1$sc.Differenz_Durchschnitt)
qqnorm(training_1$sc.Differenz_Durchschnitt)
qqline(training_1$sc.Differenz_Durchschnitt)
### normal distributed, F-Test possible ###

# we create sub-models to avoid over-parameterisation

## Model a Team ####
lm.t1a <- lm(sc.Differenz_Durchschnitt ~ Team , data = training_1)
drop1(lm.t1a, test="F") # significant
summary(lm.t1a) # which Teams have most influence
mod.lm.t1a <- dredge(lm.t1a, rank = "AIC" ) # model selection
head(mod.lm.t1a)
get.models(mod.lm.t1a,1:2)
sw(mod.lm.t1a) # parameter importance

plot(lm.t1a)  # all good

## Model b Training parameters ####
lm.t1b <- lm(sc.Differenz_Durchschnitt ~ Schritt + Zielobjekt + Stoerreiz + Zielart, data = training_1)
vif(lm.t1b) # check collinearity, all fine
drop1(lm.t1b, test="F")
summary(lm.t1b) 

# model selection
mod.lm.t1b <- dredge(lm.t1b, rank = "AIC")
head(mod.lm.t1b)
get.models(mod.lm.t1b,1:2)
sw(mod.lm.t1b) # parameter importance

# Target species and Disctraction most important and significant
# quick overview:
plot(allEffects(lm.t1b))

plot(lm.t1b)  # all good

## Model c Basic Parameters ####
lm.t1c <- lm(sc.Differenz_Durchschnitt ~ Kohorte + Anzeigeverhalten + FCI_Gruppe + Verstaerkerwahl + Alter + Erfahrung, data = training_1)
vif(lm.t1c) # check collinearity, all fine
drop1(lm.t1c, test="F")
summary(lm.t1c)

# Modellselektion
mod.lm.t1c <- dredge(lm.t1c, rank = "AIC")
head(mod.lm.t1c)
get.models(mod.lm.t1c,1:2)
sw(mod.lm.t1c) 

# all play a role
# quick overview:
plot(allEffects(lm.t1c))

plot(lm.t1c)  # all good

# some simple visualisations to understand results
boxplot(Erfahrung~Ziel_erreicht,data=training)
boxplot(sc.Differenz_Durchschnitt~Erfahrung,data=training_1)
boxplot(sc.Differenz_Durchschnitt~FCI_Gruppe,data=training_1)
boxplot(sc.Differenz_Durchschnitt~Stoerreiz, data=training_1)
boxplot(sc.Differenz_Durchschnitt~Anzeigeverhalten, data=training_1)
boxplot(sc.Differenz_Durchschnitt~Verstaerkerwahl, data=training_1)
boxplot(sc.Differenz_Durchschnitt~Schritt, data=training_1)
boxplot(sc.Differenz_Durchschnitt~Zielart,data=training_1)
boxplot(sc.Differenz_Durchschnitt~Alter,data=training_1)
boxplot(sc.Differenz_Durchschnitt~Team,data=training_1)


# (Q2) Which social and training factors influenced whether the goal was correctly achieved? ####
# now we need the full unfiltered dataset again
names(training)

mean(training$Ziel_erreicht)
sd(training$Ziel_erreicht)
hist(training$Ziel_erreicht)
qqnorm(training$Ziel_erreicht)
qqline(training$Ziel_erreicht)
### not normal distributed --> Chi Test; Target achieved is 0 or 1 --> binomial distribution ###

# same sub-models as above

## Model a Team ####
lm.t2a <- glm(Ziel_erreicht ~ Team, data = training, family = "binomial")
drop1(lm.t2a, test="Chi") 
summary(lm.t2a) 
mod.lm.t2a <- dredge(lm.t2a, rank = "AIC" )
head(mod.lm.t2a)
get.models(mod.lm.t2a,1:2)
# not as important as above, although significant

# Distribution of target achievement per team:
table(training$Team,training$Ziel_erreicht)
boxplot(Ziel_erreicht~Team,data=training)

deviance(lm.t2a) / df.residual(lm.t2a)

## Model b Training ####
lm.t2b <- glm(Ziel_erreicht ~ Stoerreiz +  sc.Differenz_Durchschnitt + Zielobjekt + Schritt + Zielart, data = training, family = "binomial")
vif(lm.t2b) # no collinearity
drop1(lm.t2b, test="Chi") 
summary(lm.t2b)

# model selection
mod.lm.t2b <- dredge(lm.t2b, rank="AIC")
head(mod.lm.t2b,10) #bestes Modell: Stoerreiz
get.models(mod.lm.t2b,1:4)
sw(mod.lm.t2b)

# mostly distraction, but all but sample odour have an effect
# visual overview:
plot(allEffects(lm.t2b))


### target was usually not achieved when alert duration was too short for the given training step
table(training$Ziel_erreicht,training$MindestdauerAnzeige) # perfect correlation
boxplot(Ziel_erreicht~MindestdauerAnzeige,data=training)


## Model c Basis ####
lm.t2c <- glm(Ziel_erreicht ~ Kohorte + FCI_Gruppe + Erfahrung + Verstaerkerwahl + Alter + Anzeigeverhalten, data = training, family = "binomial")
vif(lm.t2c)
drop1(lm.t2c, test="Chi") 
summary(lm.t2c)

# Model selection
mod.lm.t2c <- dredge(lm.t2c, rank="AIC")
head(mod.lm.t2c,10) 
get.models(mod.lm.t2c,1:4)
sw(mod.lm.t2c)

# most important are FCI breed group, reward type, alert behaviour

# initial visualisation:
lm.t2c1 <- glm(Ziel_erreicht ~ Kohorte + Erfahrung + Verstaerkerwahl + Alter + Zielart, data = training, family = "binomial")
plot(allEffects(lm.t2c1))

## have a look at relation between both response parameters
boxplot(sc.Differenz_Durchschnitt~Ziel_erreicht, data=training)


## and compare Teams
table(training$Ziel_erreicht,training$Team)


# Graphs ####
# What to select:
# Alert behaviour
# Reward type
# pace of cohorts?
# pace and age?

# for the graphs, we need to translate everything necessary into English
training_plots_1 <- training_1
training_plots <- training

str(training_plots)
class(training_plots$sc.Differenz_Durchschnitt)
training_plots$sc.Differenz_Durchschnitt <- as.numeric(training_plots$sc.Differenz_Durchschnitt)
training_plots_1$sc.Differenz_Durchschnitt <- as.numeric(training_plots_1$sc.Differenz_Durchschnitt)

# version with sample size
#levels(training_plots$Anzeigeverhalten) <- c("lay (N = 16)", "sit (N = 16)", "freeze (N = 171)")
#levels(training_plots_1$Anzeigeverhalten) <- c("lay (N = 16)", "sit (N = 16)", "freeze (N = 171)")
#levels(training_plots$Verstaerkerwahl) <- c("food (N = 165)", "toy (N = 22)", "both (N = 16)")
#levels(training_plots_1$Verstaerkerwahl) <- c("food (N = 165)", "toy (N = 22)", "both (N = 16)")
#levels(training_plots$Zielobjekt) <- c("KONG (N = 97)", "plant (N = 106)")
#levels(training_plots_1$Zielobjekt) <- c("KONG (N = 97)", "plant (N = 106)")
#levels(training_plots$Zielart) <- c("A. artem. (N = 40)", "F. spp. (N = 82)", "I. glan. (N = 81)")
#levels(training_plots_1$Zielart) <- c("A. artem. (N = 40)", "F. spp. (N = 82)", "I. glan. (N = 81)")
#levels(training_plots$Erfahrung) <- c("1 (N = 60)", "2 (N = 85)", "3 (N = 36)", "0 (N = 22)")
#levels(training_plots_1$Erfahrung) <- c("1 (N = 60)", "2 (N = 85)", "3 (N = 36)", "0 (N = 22)")
#training_plots$Erfahrung <- factor(training_plots$Erfahrung, 
#                                   levels = c("0 (N = 22)", "1 (N = 60)", "2 (N = 85)", "3 (N = 36)"))
#training_plots_1$Erfahrung <- factor(training_plots_1$Erfahrung, 
#                                   levels = c("0 (N = 22)", "1 (N = 60)", "2 (N = 85)", "3 (N = 36)"))

# version without sample size
levels(training_plots$Anzeigeverhalten) <- c("lay", "sit", "freeze")
levels(training_plots_1$Anzeigeverhalten) <- c("lay", "sit", "freeze")
levels(training_plots$Verstaerkerwahl) <- c("food", "toy", "both")
levels(training_plots_1$Verstaerkerwahl) <- c("food", "toy", "both")
levels(training_plots$Zielobjekt) <- c("KONG", "plant")
levels(training_plots_1$Zielobjekt) <- c("KONG", "plant")
levels(training_plots$Zielart) <- c("A. artem.", "F. spp.", "I. glan.")
levels(training_plots_1$Zielart) <- c("A. artem.", "F. spp.", "I. glan.")

training_plots$Erfahrung <- factor(training_plots$Erfahrung, 
                                   levels = c("0", "1", "2", "3"))
training_plots_1$Erfahrung <- factor(training_plots_1$Erfahrung, 
                                     levels = c("0", "1", "2", "3"))



# and we need to re-run a final models for plotting with the relevant parameters
# but we cannot put all parameters together
# we use the original submodels

lm.t1c.plot1 <- lm(sc.Differenz_Durchschnitt ~ Schritt + Zielobjekt + Stoerreiz + Zielart + Kohorte, 
                  data = training_plots_1)
lm.t1c.plot2 <- lm(sc.Differenz_Durchschnitt ~ Kohorte + Anzeigeverhalten + FCI_Gruppe + Verstaerkerwahl + Alter + Erfahrung,
                   data = training_plots_1)

lm.t2c.plot1 <- glm(Ziel_erreicht ~ Stoerreiz +  sc.Differenz_Durchschnitt + Zielobjekt + Schritt + Zielart + Kohorte, 
                   family = "binomial", data = training_plots)
lm.t2c.plot2 <- glm(Ziel_erreicht ~ Kohorte + FCI_Gruppe + Erfahrung + Verstaerkerwahl + Alter + Anzeigeverhalten, 
                    family = "binomial", data = training_plots)


## Alert behaviour ####

mydf1 <- ggpredict(lm.t1c.plot2, terms = c("Anzeigeverhalten","Kohorte"))
mydf1

p1 <- plot(mydf1, colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
          show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Alert behaviour") +
  xlab("") +
  ylab("Training pace (scaled)")+
  ggtitle("")
#  labs(color="Kohorte")
p1
#ggsave("Q1_alert.png", plot = p1, width = 10, height = 8, dpi = 300)

mydf2 <- ggpredict(lm.t2c.plot2, terms = c("Anzeigeverhalten","Kohorte"))
mydf2

p2 <- plot(mydf2, colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.57, hjust=0.67)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Alert behaviour") +
  ylab("Target goal achievement")+
  ggtitle("")

p2
#ggsave("Q2_alert.png", plot = p2, width = 10, height = 8, dpi = 300)


## Reward type ####
mydf3 <- ggpredict(lm.t1c.plot2, terms = c("Verstaerkerwahl","Kohorte"))
mydf3

p3 <- plot(mydf3, colors= c("#d69105", "#674602"), show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Reward type") +
  xlab("") +
#  ylab("Training pace (scaled)")+
  ylab("")+
  ggtitle("
          ")
#labs(color="Kohorte")
p3
#ggsave("Q1_reward.png", plot = p3, width = 10, height = 8, dpi = 300)

mydf4 <- ggpredict(lm.t2c.plot2, terms = c("Verstaerkerwahl", "Kohorte"))
mydf4

p4 <- plot(mydf4,colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Reward type") +
#  ylab("Target goal achievement")+
  ylab("")+
  ggtitle("
          ")

p4
#ggsave("Q2_reward.png", plot = p4, width = 10, height = 8, dpi = 300)



## Dog age (not used in publication) ####

mydf5 <- ggpredict(lm.t1c.plot2, terms = c("Alter", "Kohorte"))
mydf5

p5 <- plot(mydf5, colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  theme(legend.title = element_text(size = 22)) +
  theme(legend.text = element_text(size = 22))+
  xlab("Dog Age") +
  ylab("Training pace difference from average")+
  ggtitle("
          ")
p5
#ggsave("Q1_age_cohort.png", plot = p5, width = 10, height = 8, dpi = 300)


## Distractions ####
lm.t2b.dist <- lm(Ziel_erreicht ~ Stoerreiz, data = training_plots)

mydf6 <- ggpredict(lm.t1c.plot1, terms = c("Stoerreiz", "Kohorte"))
mydf6

p6 <- plot(mydf6, colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Distraction") +
  xlab("") +
#  ylab("Training pace (scaled)")+
  ylab("")+
  ggtitle("
          ")
#labs(color="Kohorte")
p6

mydf7 <- ggpredict(lm.t2c.plot1, terms = c("Stoerreiz", "Kohorte"))
mydf7

p7 <- plot(mydf7, colors= c("#d69105", "#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  #  theme(legend.title = element_text(size = 22)) +
  #  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Distraction") +
  #  ylab("Target goal achievement")+
  ylab("")+
  ggtitle("
          ")

p7


## Team experience ####
mydf8 <- ggpredict(lm.t1c.plot2, terms = c("Erfahrung","Kohorte"))
mydf8

p8 <- plot(mydf8, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Team experience") +
  xlab("") +
#  ylab("Training pace difference from average")+
  ylab("")+
  ggtitle("")
p8
#ggsave("Q1_texperience.png", plot = p8, width = 10, height = 8, dpi = 300)

mydf9 <- ggpredict(lm.t2c.plot2, terms = c("Erfahrung", "Kohorte"))
mydf9

p9 <- plot(mydf9, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Team experience") +
#  ylab("Target goal achievement")+
  ylab("")+
  ggtitle("")
p9
#ggsave("Q2_texperience.png", plot = p9, width = 10, height = 8, dpi = 300)


## Sample odour (not used in publication) ####
# no cohort differences here, so new dummy model for graph needed
mydf10 <- ggpredict(lm.t1c.plot1, terms = c("Zielobjekt"))
mydf10

p10 <- plot(mydf10, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Sample odour") +
  xlab("") +
#  ylab("Training pace difference from average")+
  ylab("")+
  ggtitle("")
p10
#ggsave("Q1_sample.png", plot = p10, width = 10, height = 8, dpi = 300)

mydf11 <- ggpredict(lm.t2c.plot1, terms = c("Zielobjekt"))
mydf11

p11 <- plot(mydf11, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Sample odour") +
#  ylab("Target goal achievement")+
  ylab("")+
  ggtitle("") 

p11
#ggsave("Q2_sample.png", plot = p11, width = 10, height = 8, dpi = 300)


## Target plant ####
mydf12 <- ggpredict(lm.t1c.plot1, terms = c("Zielart","Kohorte"))
mydf12

p12 <- plot(mydf12, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
#  xlab("Target species") +
  xlab("") +
#  ylab("Training pace difference from average")+
  ylab("")+
#  labs(colour='Cohort') +
  ggtitle("")
p12
#ggsave("Q1_target.png", plot = p12, width = 10, height = 8, dpi = 300)

mydf13 <- ggpredict(lm.t2c.plot1, terms = c("Zielart", "Kohorte"))
mydf13

p13 <- plot(mydf13, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("errorbar"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Target species") +
#  ylab("Target goal achievement")+
  ylab("")+
#  labs(colour='Cohort') +
  ggtitle("")

p13
#ggsave("Q2_target.png", plot = p13, width = 10, height = 8, dpi = 300)


## Training step (not in publication) ####
mydf14 <- ggpredict(lm.t1c.plot1, terms = c("Schritt","Kohorte"))
mydf14

p14 <- plot(mydf14, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
            show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(-2.2, 3.5),          
    breaks = -2:3,                  
    expand = expansion(mult = c(0, 0))  
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  theme(legend.title = element_text(size = 22)) +
  theme(legend.text = element_text(size = 22))+
  #  xlab("Target species") +
  xlab("") +
  #  ylab("Training pace difference from average")+
  ylab("")+
#  labs(colour='Cohort') +
  ggtitle("")
p14
#ggsave("Q1_target.png", plot = p12, width = 10, height = 8, dpi = 300)

mydf15 <- ggpredict(lm.t2c.plot1, terms = c("Schritt", "Kohorte"))
mydf15

p15 <- plot(mydf15, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
            show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  theme(legend.title = element_text(size = 22)) +
  theme(legend.text = element_text(size = 22))+
  xlab("Target species") +
  #  ylab("Target goal achievement")+
  ylab("")+
  labs(colour='Cohort') +
  ggtitle("")

p15
#ggsave("Q2_target.png", plot = p13, width = 10, height = 8, dpi = 300)


## Deviation from average duration ####
mydf16 <- ggpredict(lm.t2c.plot1, terms = c("sc.Differenz_Durchschnitt [all]", "Kohorte"))
mydf16

p16 <- plot(mydf16, colors= c("#d69105","#674602"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
            show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.6)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
#  theme(legend.title = element_text(size = 22)) +
#  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Training pace (scaled)") +
  #  ylab("Target goal achievement")+
  ylab("")+
#  labs(colour='Cohort') +
  ggtitle("")

p16


## five important parameters ####
all.Q1_Q2<-ggarrange(p1,p3,p8,p6,p12,p2,p4,p9,p7,p13 ,labels="AUTO", nrow=2, ncol=5)
#  theme(plot.margin=margin(0.1,0.1,1,0.1,"cm"),
#  plot.title = element_text(size=10),legend.text=element_text(size=50))
all.Q1_Q2
#ggsave("Fig3.tif", plot = all.Q1_Q2, width = 28, height = 14, dpi = 300)

## final plot using patchwork ####
# get legend as separate plot
legend <- get_legend(
  p1 + theme(
    legend.position = "right",
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 22)
    ) + 
    labs(colour="Cohort")
)
legend_plot <- wrap_elements(full = legend)

# manual plot annotations (due to "empty plot" for legend) 
p1 <- p1 + labs(tag = "A") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p3 <- p3 + labs(tag = "B") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p8 <- p8 + labs(tag = "C") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p6 <- p6 + labs(tag = "D") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p12 <- p12 + labs(tag = "E") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p2 <- p2 + labs(tag = "F") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p4 <- p4 + labs(tag = "G") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p9 <- p9 + labs(tag = "H") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p7 <- p7 + labs(tag = "I") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p13 <- p13 + labs(tag = "J") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)
p16 <- p16 + labs(tag = "K") + theme(
  plot.tag = element_text(size = 22, face = "bold")
)


# combine to final plot
top_row <- wrap_plots(
  p1, p3, p8, p6, p12, legend_plot,
  ncol = 6
)

bottom_row <- wrap_plots(
  p2, p4, p9, p7, p13, p16,
  ncol = 6
)

final_plot <- top_row / bottom_row
final_plot
ggsave("Fig3.tif", plot = final_plot, width = 28, height = 14, dpi = 300)



# select the Teams that are present in this analysis ####
names(training)
teams <- training[,c(2,13:20)]
head(teams)
teams2 <- unique(teams)
nrow(teams2)
# one team without video assessment
#write.csv2(teams2,"teams.csv", row.names = FALSE)

############################################################################
########### IGAMon-Dog: Code field test both cohorts, Q5 ############
############################################################################

# required packages
library(GGally)
library(car)
library(MuMIn)
library(ggeffects)
library(effects)
library(dplyr)
library(ggpubr)
library(DHARMa)
library(performance)

# Data preparation ####
## Read data ####
einsatztest <- read.csv2("einsatztest.csv", stringsAsFactors = TRUE)
head(einsatztest)
nrow(einsatztest) # 27 teams included

names(einsatztest)
# dataframe includes
## team ID [,1]
## teams data [,2:9]
## results from online training [,10:12]
## results from practical seminar test [,13:26]
## results from field test [,27:29]
## results from motivation questionaire (evaluation form) [,30:38]
## the cohort [,39]


# Field test detection rate ####
summary(einsatztest$det_rate_field)
# 16 NAs --> results from 11 teams
# from 0 to 1
# mean of 0.68

# excluding the person that did not pass
summary(einsatztest$det_rate_field[einsatztest$det_rate_field!=0])
# from 0.5 to 1
# mean of 0.7471

# summary per cohort
einsatztest %>%
  group_by(cohort) %>%
  summarise(
    n = n(),
    mean = mean(det_rate_field, na.rm = TRUE),
    min = min(det_rate_field, na.rm = TRUE),
    max = max(det_rate_field, na.rm = TRUE)
  )
# mean 71,7 and 63,4 for cohort 1 and 2
einsatztest %>%
  group_by(cohort) %>%
  summarise(
    n = sum(!is.na(det_rate_field) & det_rate_field != 0),
    mean = mean(det_rate_field[det_rate_field!=0], na.rm = TRUE),
    min = min(det_rate_field[det_rate_field!=0], na.rm = TRUE),
    max = max(det_rate_field[det_rate_field!=0], na.rm = TRUE)
  )
# mean 71.7 and 79.3 for both cohorts


# Field test conducted or not? ####
# response: field_participation (0 - not participated, 1 - participated)
# explanatory: 
## rel_goal = target goal reached in online training, percentage across all training steps filmed
## diff_goal = training pace, average across all steps filmed
## final.points = final.points reached in practical seminar test (= Team score)
## det_rate = overall detection rate practical seminar test
## cohort = cohort 1 or 2
## motivation scale

# we are aware of most data from Q1-Q4
# check motivation
summary(einsatztest[,30:38])
# "Dog training": all put a 6, cannot be used for analsyes
# other values range from 1 to 6

## check collinearity ####
ggpairs(einsatztest[,c(10,11,23,24,30:38)]) 
# detection rate and final.points from practical seminar correlated --> we keep final.points as more conclusive
# Responsibility and Conservation positively correlated --> we use conservation 
# Work-life-balance accidentally correlate with many parameters, we leave it out
# for weak correlations we will check vif



## Model 1: performance in previous phases ####
# people that did not complete both the online training documentation and the practical seminar need to be excluded
einsatztest1 <- einsatztest %>%
  filter(!is.na(rel_goal), !is.na(final.points))

nrow(einsatztest1)

# we need to exclude team 79313 which completed the field test with a different dog than the previous tests
einsatztest1 <- einsatztest1 %>%
  filter(team != 79313)

glm_participation<- glm(field_participation ~ final.points + diff_goal*cohort  + rel_goal, 
                    data=einsatztest1, family= binomial) 
AIC(glm_participation) 
vif(glm(field_participation~final.points + diff_goal+cohort  + rel_goal, 
        data=einsatztest1, family= binomial)) #ok
drop1(glm_participation,test="Chisq") #nothing significant
summary(glm_participation)

# model selection
mod_participation<-dredge(glm_participation, rank=AIC) 
head(mod_participation,6) # null model best, not effect
sw(mod_participation) # nothing important
plot(allEffects(glm_participation)) # nothing super exciting

# model validation
deviance(glm_participation) / df.residual(glm_participation)
simulationOutput <- simulateResiduals(fittedModel = glm_participation, plot = T)
testDispersion(glm_participation) 
testQuantiles(glm_participation) 
# all not significant, so ok
check_residuals(glm_participation)
check_convergence(glm_participation)
check_overdispersion(glm_participation)
# no deviations


## alternatively: social factors (not included, just for overview)
einsatztest1$Team_experience <- as.factor(einsatztest1$Team_experience) 
einsatztest1$FCI_group <- as.factor(einsatztest1$FCI_group) 
glm_social <- glm(field_participation~FCI_group + Dog_age + Team_experience + Target_species, 
                  data=einsatztest1, family= binomial) 
AIC(glm_social) # worse than using the results instead of raw data
vif(glm_social) # ok-ish, not perfect
summary(glm_social)
drop1(glm_social,test="Chisq") # nothing
mod_social<-dredge(glm_social, rank=AIC) 
head(mod_social,6) # Null model among best, maybe dog age and team experience slightly
sw(mod_social) # dog age slightly important, nothing else
plot(allEffects(glm_social), ylim=c(-10,10)) 
# standard errors are huge
# not usable

einsatztest1 %>%
  group_by(cohort, Team_experience) %>%
  summarise(
    n = n(),
    mean = mean(field_participation, na.rm = TRUE),
  )

einsatztest1 %>%
  group_by(Team_experience) %>%
  summarise(
    n = n(),
    mean = mean(field_participation, na.rm = TRUE),
  )

table(einsatztest1$Team_experience,
      einsatztest1$field_participation)

# the tendency exists that teams with more experience finish more likely

## Model 2: Motivation ####
# one person did not complete the evaluation form, we need to remove it for this part
einsatztest2 <- einsatztest %>%
  filter(!is.na(Conservation), !is.na(field_participation))

nrow(einsatztest2)

glm_participation2<- glm(field_participation ~ Conservation + Science + Community + 
                       Qualification + Self.efficacy + Career, 
                     data=einsatztest2, family= binomial)
AIC(glm_participation2) #26.56 lower than above
vif(glm_participation2) # ok
drop1(glm_participation2,test="Chisq") #Conservation, Science, Community significant
summary(glm_participation2)

deviance(glm_participation) / df.residual(glm_participation)  # ok

# model selection
mod_participation2<-dredge(glm_participation2, rank=AIC) 
head(mod_participation2,15) 
sw(mod_participation2) #Science, Community, Conservation important
plot(allEffects(glm_participation2)) 

# model validation
deviance(glm_participation) / df.residual(glm_participation)
simulationOutput <- simulateResiduals(fittedModel = glm_participation, plot = T)
testDispersion(glm_participation) 
testQuantiles(glm_participation) 
# all not significant, so ok
check_residuals(glm_participation)
check_convergence(glm_participation)
check_overdispersion(glm_participation)
# no deviations


## check comparison when including cohort ####
glm_participation3<- glm(field_participation ~ Conservation + Science + Community + 
                           Qualification + Self.efficacy + Career + cohort, 
                     data=einsatztest2, family= binomial)
AIC(glm_participation3)
AIC(glm_participation2)
# much worse including cohort

# is there a difference in motivation among cohorts
# Mann-Whitney-U-Test
wilcox.test(Conservation ~ cohort, data=einsatztest2, 
            exact = FALSE, alternative = "less")
wilcox.test(Science ~ cohort, data=einsatztest2, 
            exact = FALSE, alternative = "greater")
wilcox.test(Community ~ cohort, data=einsatztest2, 
            exact = FALSE, alternative = "greater")
wilcox.test(Self.efficacy ~ cohort, data=einsatztest2, 
            exact = FALSE, alternative = "greater")
# no, nothing significant


# graphs ####

## Conservation ####
mydf1 <- ggpredict(glm_participation2, terms = c("Science"))
mydf1

p1 <- plot(mydf1, colors= c("#d69105"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_x_continuous(
    limits = c(0.9, 6.1),   
    breaks = 1:6,    
    expand = expansion(mult = c(0, 0))  
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  #  theme(legend.title = element_text(size = 22)) +
  #  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Motivation score: Science") +
  ylab("Probability of field test")+
  ggtitle("")
#  labs(color="Kohorte")
p1

## Conservation ####
mydf2 <- ggpredict(glm_participation2, terms = c("Conservation"))
mydf2

p2 <- plot(mydf2, colors= c("#d69105"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_x_continuous(
    limits = c(0.9, 6.1),   
    breaks = 1:6,    
    expand = expansion(mult = c(0, 0))  
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  #  theme(legend.title = element_text(size = 22)) +
  #  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Motivation score: Conservation") +
  ylab("Probability of field test")+
  ggtitle("")
#  labs(color="Kohorte")
p2


## Community ####
mydf3 <- ggpredict(glm_participation2, terms = c("Community"))
mydf3

p3 <- plot(mydf3, colors= c("#d69105"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_x_continuous(
    limits = c(0.9, 6.1),   
    breaks = 1:6,    
    expand = expansion(mult = c(0, 0))  
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  #  theme(legend.title = element_text(size = 22)) +
  #  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Motivation score: Community") +
  ylab("")+
  ggtitle("")
#  labs(color="Kohorte")
p3


## Self-sfficacy ####
mydf4 <- ggpredict(glm_participation2, terms = c("Self.efficacy"))
mydf4

p4 <- plot(mydf4, colors= c("#d69105"),show_data = F, jitter=0.01, dot_size = 5, line_size = 1.5, 
           show_ci = T, ci_style = c("ribbon"), show_title = FALSE) + 
  scale_x_continuous(
    limits = c(0.9, 6.1),   
    breaks = 1:6,    
    expand = expansion(mult = c(0, 0))  
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_classic() +
  theme(axis.text = element_text(size = 18)) +
  #  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(axis.title = element_text(size = 22)) +
  theme(strip.text.x = element_text(size=22)) +
  #  theme(legend.title = element_text(size = 22)) +
  #  theme(legend.text = element_text(size = 22))+
  theme(legend.position="none") +
  xlab("Motivation score: Self-efficacy") +
  ylab("")+
  ggtitle("")
#  labs(color="Kohorte")
p4

## final figure ####
all.mot<-ggarrange(p1,p3,p2,p4, labels="AUTO") 
all.mot
ggsave("Fig5.tif", plot = all.mot, width = 28, height = 14, dpi = 300)


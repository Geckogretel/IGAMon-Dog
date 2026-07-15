############################################################################
########### IGAMon-Dog: Code practical seminar, Q3 + Q4 ############
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
require(lme4)
library(DHARMa)
library(performance)
library(patchwork)
library(purrr)


# Data preparation ####
## Read data ####
# df = data from practical seminar
df <- read.csv2("practical_seminar.csv", stringsAsFactors = TRUE)
df$date <- as.Date(df$date, format = "%d.%m.%Y") 
df <- df %>%
  mutate(across(c(FCI, target, team, year, leash, weather, wind, samplecondition,
                  search, sample, grid, alert),
                as.factor))
df <- df[!is.na(df$found_dog),]
str(df)

# size = sample length
# search = test session

#order levels
df$weather <- factor(df$weather, levels = c("sunny", "partly cloudy", "cloudy"))
df$wind <- factor(df$wind, levels = c("no", "slight", "intermittent"))
df$alert <- factor(df$alert, levels = c("light", "medium","strong"))
levels(df$samplecondition) <- c("fresh","thawed")

# fb = data from survey on training habits of the teams
fb <- read.csv2("survey.csv") 
fb <- fb %>%
  mutate(across(c(team, plant_parts, dried, fresh_training, rew_variety),
                as.factor))
str(fb)

# points = point system as overall quality measure for teams
# including training objectives, proportion of targets found in practical seminar
# and proportion of fresh plants in the practical seminar
points <- read.csv2("teams_points.csv",stringsAsFactors = T) 
points <- points[!is.na(points$points),] # delete teams which did not finish
points$team <- as.factor(points$team)

# fresh_test = Percentage of fresh samples in the practical seminar test
# points = Team score

#merge tables for models
data <- merge(df,fb, by="team") #data = survey + practical seminar data
dat <- df %>% # dat =  survey, points + weather conditions at practical seminar
  distinct(team, .keep_all = TRUE) %>%
  select(team, year, temp, weather, wind) %>%
  left_join(
    merge(points, fb, by = "team"),
    by = "team"
  )

#arrange levels
dat$steps_skipped <- factor(dat$steps_skipped, levels = c("none", "few", "many"))
dat$single_blind <- factor(dat$single_blind, levels = c("none", "few", "many","most"))
dat$nr_individuals <- factor(dat$nr_individuals, levels = c("one", "few", "many"))

#one person did the practical seminar twice with two different dogs. The training pace data is only available for one dog. Team 79430 can thus not be part of this analysis
dat <- dat[dat$team != 79430, ]


## Check parameter distribution and correlation ####
# only take parameters in analyses that truely change 
# parameter for analyses indicated by "yes"

table(df$FCI) # FCI (dog breed group): yes
table(df$target) # target species: yes
table(df$leash) # leash: yes
table(df$temp)  # temperature: yes
table(df$humidity) # humidity: yes, but likely correlated with temperature
table(df$weather) # weather: yes
table(df$wind) # wind: yes
table(df$samplecondition) # sample condition (fresh/thawed): yes
table(df$search) # number of search: yes
table(df$grid)  # grid number containing the sample: yes
table(df$size) # sample size in cm: yes
table(df$time) # search time: yes
table(df$alert) #alert behaviour: yes


# check correlation
names(df)
ggpairs(df[,c(2:27)],  cardinality_threshold = 24) 

#temperature and humidity are correlated, keep temperature
#temperature and year are correlated
#plot and t-test within and between years
summary(df$temp[df2$year=="2022"]) #2-17.3°
summary(df$temp[df2$year=="2023"]) #17-28°
par(mfrow=c(2,2))
boxplot(temp~year,data=df[df$found_dog=="0",], main="not found")
boxplot(temp~year,data=df[df$found_dog=="1",], main="found")

boxplot(temp~found_dog,data=df[df$year=="2022",], main="2022")
boxplot(temp~found_dog,data=df[df$year=="2023",], main="2023")
hist(df$temp[df$year=="2022"], main="2022")
hist(df$temp[df$year=="2023"], main="2023")
table(df$temp, df$year)

#paired t-test
shapiro.test(df$temp[df$year=="2022"]) #not normal distributed
shapiro.test(df$temp[df$year=="2023"]) #not normal distributed
df22<-df[df$year=="2022",]
df23<-df[df$year=="2023",]
describeBy(df22$temp,df22$found_dog) #test homogeneity of variance
leveneTest(df22$temp,df22$found_dog)  #p>0.05 -> homogeneity of variance -> t-test
describeBy(df23$temp,df23$found_dog) #test homogeneity of variance
leveneTest(df23$temp,df23$found_dog) #p>0.05 -> t-test

t.test(df22$temp[df22$found_dog == 0], #p>0.05 -> no difference
       df22$temp[df22$found_dog == 1], 
       var.equal = TRUE)
t.test(df23$temp[df23$found_dog == 0], #p>0.05 -> no difference
       df23$temp[df23$found_dog == 1], 
       var.equal = TRUE)

#differences are not temperature related -> keep year

# (3) Which social, training and environmental factors influenced the detection of plants during the practical seminar ####

# we create sub-models to avoid over-parameterisation

## Modal a on-site conditions ####
glm3a <- glm(found_dog ~ FCI + target + year + wind + 
             samplecondition*size + search, 
             data = df, family= binomial)
AIC(glm3a)
vif(glm3a) # fine

deviance(glm3a) / df.residual(glm3a)
simulationOutput <- simulateResiduals(fittedModel = glm3a, plot = T)
testDispersion(glm3a) 
testQuantiles(glm3a) 
# all not significant, so ok
check_residuals(glm3a)
check_convergence(glm3a)
check_overdispersion(glm3a)

summary(glm3a) 
drop1(glm3a, test = "Chisq") # significance
mod3a <- dredge(glm3a, rank = AIC) # model selection
head(mod3a,5) 
sw(mod3a) # parameter importance
get.models(mod3a,1:10)
plot(allEffects(glm3a)) # quick overview

# compare to null model
glm3null <- glm(found_dog ~ 1, 
                data = df, family = binomial)
AIC(glm3null) 
anova(glm3null, glm.3a, test = "Chi") #p<0.001

## Modal b training conditions ####

# check correlation
colSums(is.na(data))
names(data)
ggpairs(data[,c(33:50)],  cardinality_threshold = 24) 

#plant_parts = which/how many parts of the plant were used for training (leaves, stem, roots, flower)

glm3b <- glm(found_dog ~ experience  + participation_trainingwebinar_live +
              training_days_week  + steps_skipped + single_blind +
              log(search_area_qm)  + dried + fresh_training + plant_parts, 
              data = data, family = binomial)

AIC(glm3b) 
vif(glm3b)
deviance(glm3b) / df.residual(glm3b)
simulationOutput <- simulateResiduals(fittedModel = glm3b, plot = T)
testDispersion(glm3b) 
testQuantiles(glm3b) 
# all not significant, so ok
check_residuals(glm3b)
check_convergence(glm3b)
check_overdispersion(glm3b)
summary(glm3b)
drop1(glm3b,test = "Chisq") # significance
mod3b <- dredge(glm3b,rank = AIC) # model selection
sw(mod3b) # parameter importance
head(mod3b,8) 
plot(allEffects(glm3b)) # plot overview

# compare to null model
glm3bnull <- glm(found_dog ~ 1, 
                data = data, family = binomial)
AIC(glm3bnull)
anova(glm3bnull, glm3b, test="Chi") #p 0.67



# (4) Which social, training and environmental factors influenced the overall team performance during the practical seminar ####

## Model a on-site conditions ####

ggpairs(dat[,c(2:5,7:10, 12:24)],  cardinality_threshold = 24, cex=0.2)
lm4a <- lm(points ~ wind + fresh_test + diff_goal + rel_goal + year, # year = cohort
         data = dat)
vif(lm4a)
plot(lm4a)
summary(lm4a) 
drop1(lm4a,test = "Chisq") # significance
mod4a <- dredge(lm4a, rank = AIC) # model selection
head(mod4a,5)
sw(mod4a) # parameter importance
get.models(mod4a,1:10)
plot(allEffects(lm4a)) # plot overview

## Model b training conditions I ####

lm4b <- lm(points ~ reward + dried + fresh_training + 
           log(search_area_qm) + plant_parts, 
           data = dat) 
AIC(lm4b) 
vif(lm4b)
plot(lm4b)
summary(lm4b)
drop1(lm4b,test = "F") # significance
mod4b <- dredge(lm4b, rank=AIC) # model selection
head(mod4b,5) 
sw(mod4b) # parameter importance
plot(allEffects(lm4b)) # plot overview

## Model c training conditions II ####

lm4c <- (lm(points ~ session_duration_min + experience + participation_trainingwebinar_live
          + training_days_week  + steps_skipped, 
          data = dat))
AIC(lm4c) 
vif(lm4c)
plot(lm4c)
summary(lm4c)
drop1(lm4c,test="F") # significance
mod4c <- dredge(lm4c, rank=AIC) # model selection
head(mod4c)
sw(mod4c) # parameter importance
plot(allEffects(lm4c)) # plot overview

# Graphs ####

## 3a ####
mydf3a <- ggpredict(glm3a, terms = c("size", "year","samplecondition"))
mydf3a
new <- c("fresh","thawed")
names(new) <- c("fresh","frozen")
p3a <- plot(mydf3a, show_data = TRUE, jitter = 0.01, dot_size = 2.5, line_size = 1.5, 
          show_ci = T, ci_style = c("ribbon"), show_title = FALSE,colors= c("#d69105","#674602")) + 
  theme_classic() +
  theme(
    axis.text        = element_text(size = 13),
    axis.title       = element_text(size = 18),
    strip.text.x     = element_text(size = 15),
    legend.title     = element_text(size = 18),
    legend.text      = element_text(size = 18),
    plot.title       = element_text(size = 18, hjust = 0.5)
  )+
  xlab("Sample length [cm]") +
  ylab("Detection probability") +
  labs(color = "Cohort", fill = "Cohort") +  # Important: match both color and fill labels
  scale_color_manual(values = c("#d69105", "#674602"), labels = c("C1", "C2")) +
  scale_fill_manual(values = c("#d69105", "#674602")) +
 # theme(legend.position = "none")+
  ggtitle("Sample condition")

p3a
legend_1 <- get_legend(p3a)

## plant parts - data from 3b & 4c ####
# adjusted for few steps skipped, otherwise no difference visible
mydf3b_plants <- ggpredict(glm3b, terms = c("plant_parts","fresh_training[1]","steps_skipped[few]"))
mydf3b_plants$predicted <- mydf3b_plants$predicted*100 
mydf3b_plants$conf.low <- mydf3b_plants$conf.low*100
mydf3b_plants$conf.high <- mydf3b_plants$conf.high*100
mydf3b_plants$x <- factor(mydf3b_plants$x, levels = c("2", "3", "4"))


mydf4b <- ggpredict(lm4b, terms =  c("plant_parts")) 
mydf4b$predicted <- mydf4b$predicted*100/9 #scale (9 = max points)
mydf4b$conf.low <- mydf4b$conf.low*100/9
mydf4b$conf.high <- mydf4b$conf.high*100/9
mydf4b$group <- as.character(mydf4b$group)
mydf4b$group[mydf4b$group == 1] <- 2

mydf3b_plants <- mydf3b_plants[, -7]
df_plants <- rbind(mydf3b_plants, mydf4b)
df_plants$conf.low[df_plants$conf.low < 0] <- 0 #set limit for prediction
df_plants$conf.high[df_plants$conf.high > 100] <- 100
df_plants$group <- factor(df_plants$group,
                            levels = c("1", "2"),  
                            labels = c("Detection probability", "Team score"))

# plot
plants <- ggplot(df_plants, aes(x = x, y = predicted, color = group, group = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 1 ,width = 0.2, position = position_dodge(width = 0.3)) +  
  labs(x = "Number of plant parts", y = "Detection probability") +
  theme_classic() +
  ylim(0, 100) +
  scale_color_manual(values = c("#b7cba3","#85a865")) +
  scale_y_continuous(
    breaks = c(0,25, 50, 75, 100), labels = c("0%", "25%", "50%", "75%", "100%"),
    name = "Detection probability",
    sec.axis = sec_axis(~./100*9, name = NULL, labels = NULL)) +
  theme_classic() +  
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 13, hjust = 1, color = "black"),
    axis.title.y.left = element_text(size = 18, color = "black"),  
    axis.text.y.left = element_text(size = 13, color = "black"),   
    axis.title.y.right = element_text(size = 18, color = "black"),  
    axis.text.y.right = element_text(size = 13, color = "black")   
  )

plants




## steps skipped - data from 3b & 4c ####
# combined plot from two models: 3b and 4c
mydf3b_steps <- ggpredict(glm3b, terms = c("steps_skipped", "fresh_training[1]","plant_parts[3]"))
mydf3b_steps$predicted <- mydf3b_steps$predicted*100 
mydf3b_steps$conf.low <- mydf3b_steps$conf.low*100
mydf3b_steps$conf.high <- mydf3b_steps$conf.high*100
df$wind <- factor(df$wind, levels = c("no", "slight", "intermittent"))
mydf3b_steps$x <- factor(mydf3b_steps$x, levels = c("none", "few", "many"))


mydf4c <- ggpredict(lm4c, terms = c("steps_skipped")) 
mydf4c$predicted <- mydf4c$predicted*100/9 #scale (9 = max points)
mydf4c$conf.low <- mydf4c$conf.low*100/9
mydf4c$conf.high <- mydf4c$conf.high*100/9
mydf4c$x <- factor(c("none", "few", "many"), levels = c("none", "few", "many"))
mydf4c$group <- as.character(mydf4c$group)
mydf4c$group[mydf4c$group == 1] <- 2

# combine dataframes
mydf3b_steps <- mydf3b_steps[, -7]
df_combined <- rbind(mydf3b_steps, mydf4c)
df_combined$conf.low[df_combined$conf.low < 0] <- 0 #set limit for prediction
df_combined$conf.high[df_combined$conf.high > 100] <- 100
df_combined$group <- factor(df_combined$group,
                            levels = c("1", "2"),  
                            labels = c("Detection probability", "Team score"))

# plot
steps <- ggplot(df_combined, aes(x = x, y = predicted, color = group, group = group)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 1 ,width = 0.2, position = position_dodge(width = 0.3)) +  
  labs(x = "Steps skipped", y = NULL) +
  theme_classic() +
  ylim(0, 100) +
  scale_color_manual(values = c("#b7cba3","#85a865")) +
  scale_y_continuous(
    breaks = c(0,25, 50, 75, 100), labels = NULL,
    name = NULL,
    sec.axis = sec_axis(~./100*9, name="Team score [points]")) +
  theme_classic() +  
  theme(
    legend.position = "none", # set to e.g. "right" to export legend
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black"),
    axis.title.y.left = element_text(size = 18, color = "black"),  
    axis.text.y.left = element_text(size = 13, color = "black"),   
    axis.title.y.right = element_text(size = 18, color = "black"),  
    axis.text.y.right = element_text(size = 13, color = "black")   
  )

steps
plot_legend <- get_legend(steps)


# combine figure
p3a <- p3a +
  labs(tag = "A") +
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  )

plants <- plants +
  labs(tag = "B") +
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  )

steps <- steps +
  labs(tag = "C") +
  theme(
    plot.tag = element_text(size = 16, face = "bold"),
    plot.tag.position = c(0.02, 0.98)
  )

panel <- (p3a  |  plot_legend) /
  (plants | steps) 


ggsave("figure_4.tif", plot = panel, width = 12, height = 8, dpi = 500)

# references ####
(.packages()) %>%
  map(citation) %>%
  print(style = "text")


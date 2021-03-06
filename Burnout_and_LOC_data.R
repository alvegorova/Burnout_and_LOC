rm(list = ls())

# This is code to replicate the analyses and figures 
# from my 2020 Burnout and Locus of Control research analysis.
# Code developed by Alena Egorova (@alvegorova)

### DOWNLOAD LIBRARIES

install.packages(c("psych", "ggplot2", "QuantPsyc", "wesanderson"))
require(ggplot2)
require(dplyr)
require(tidyr)
require(foreign) #to use "read.spss"
require(QuantPsyc) #for moderation analysis
require(wesanderson) #for wesanderson palette

### DOWNLOAD DATA

setwd("~/Documents/Data_Analysis/Burnout_and_LOC")
dataset = read.spss("Data.sav", to.data.frame=TRUE)

### PREPARE DATA

#remove empty rows
dataset <- dataset[1:93,]

#make variable names more readable
dataset$male <- factor(dataset$male, labels = c('Male', 'Female'))
names(dataset)[names(dataset) == 'male'] <- 'Gender'
names(dataset)[names(dataset) == 'USK_Io'] <- 'LOC'
names(dataset)[names(dataset) == 'age'] <- 'Age'
names(dataset)[names(dataset) == 'nn'] <- 'Number'
names(dataset)[names(dataset) == 'stage_com'] <- 'Work_experience'
names(dataset)[names(dataset) == 'stage_sch'] <- 'Last_work_experience'
names(dataset)[names(dataset) == 'Em_exhaustion'] <- 'EE'
names(dataset)[names(dataset) == 'Depersonalization'] <- 'DP'
names(dataset)[names(dataset) == 'Successfulness'] <- 'PA'
names(dataset)[names(dataset) == 'stage_sch'] <- 'Last_work_experience'

#edit subject info
names(dataset)[names(dataset) == 'speciality1'] <- 'Subject'
dataset$Subject <- factor(dataset$Subject, labels = c('Math and engineering', 'Natural sciences', 'Humanities', 'Physical'))
levels(dataset$Subject) <- c(levels(dataset$Subject),'Elementary school','Not provided')
dataset$Subject[75] <- 'Humanities'
dataset$Subject[c(3,4,21,22,91)] <- 'Elementary school'
dataset$Subject[c(41,44,45,47,49,52,73,86)] <- 'Not provided'

#edit format of numeric variables
dataset$Number <- as.integer(dataset$Number)
dataset$LOC <- as.integer(dataset$LOC)
dataset$Age <- as.integer(dataset$Age)
dataset$Work_experience <- as.integer(dataset$Work_experience)
dataset$Last_work_experience <- as.integer(dataset$Last_work_experience)
dataset$EE <- as.integer(dataset$EE)
dataset$DP <- as.integer(dataset$DP)
dataset$PA <- as.integer(dataset$PA)

#choose important variables
dataset <- dataset %>% dplyr::select (Number, Age, Gender, Work_experience, Last_work_experience, 
          Subject, LOC, EE, DP, PA)

#to overview the data
str(dataset)
View(dataset)

#Count missing data
colSums(is.na(dataset)) 

### DESCRIPTIVE STATISTICS

## General

#general description of the participants
lapply(dataset[, c("Age", "Gender", "Work_experience", "Last_work_experience", "Subject")], table)
mean(dataset$Age, na.rm = TRUE)
sd(dataset$Age, na.rm = TRUE)

#histogram with age and gender
dataset %>% filter (!is.na(Age)) %>% 
  ggplot(aes(Age, fill=Gender)) + 
  geom_histogram(alpha = 0.8, position = "stack", binwidth=2) + 
  scale_fill_grey() + theme_classic()

#histogram with Work_experience
dataset %>%  
  ggplot(aes(Work_experience)) + 
  geom_histogram(alpha = 0.8, position = "stack", fill=wes_palette(n=1, name="Royal1"), binwidth=2) + 
  theme_classic()

#histogram with Last_work_experience
dataset %>%  
  ggplot(aes(Last_work_experience)) + 
  geom_histogram(alpha = 0.8, position = "stack", fill=wes_palette(n=1, name="Royal1"), binwidth=2) + 
  theme_classic()

#how many participants have been working at current place less then 10 years
dataset %>% filter (Last_work_experience<10) %>% nrow()

#bar with teaching subject
dataset %>%  
  ggplot(aes(Subject)) + 
  geom_bar(alpha = 0.8, position = "stack", fill=wes_palette(n=1, name="Royal1")) + 
  theme_classic()

## Burnout data

#reverse Personal Accomplishment scale to Reduced Personal accomplishment scale
dataset <- dataset %>% mutate (RPA = 48 - PA)

#count scores of emotional exhaustion (EE_score)
dataset <- dataset %>% 
  mutate (EE_score = ifelse((Gender=="Female" & EE %in% c(0:6)) |
                              (Gender=="Male" & EE %in% c(0:5)), 0,
                            ifelse ((Gender=="Female" & EE %in% c(6:16))|
                                      (Gender=="Male" & EE %in% c(5:15)), 1,
                                    ifelse ((Gender=="Female" & EE %in% c(17:25))|
                                              (Gender=="Male" & EE %in% c(16:24)), 2,
                                            ifelse ((Gender=="Female" & EE %in% c(26:34))|
                                                      (Gender=="Male" & EE %in% c(25:34)), 3, 5)))))


#count scores of depersonalization (DP_score)
dataset <- dataset %>% 
  mutate (DP_score = ifelse((Gender=="Female" & DP %in% c(0:1)) |
                              (Gender=="Male" & DP %in% c(0:2)), 0,
                            ifelse ((Gender=="Female" & DP %in% c(1:4))|
                                      (Gender=="Male" & DP %in% c(2:4)), 1,
                                    ifelse ((Gender=="Female" & DP %in% c(5:10))|
                                              (Gender=="Male" & DP %in% c(5:12)), 2,
                                            ifelse ((Gender=="Female" & DP %in% c(11:13))|
                                                      (Gender=="Male" & DP %in% c(13:15)), 3, 5)))))


#count scores of reduction of personal accomplishment (RPA_score)
dataset <- dataset %>% 
  mutate (RPA_score = ifelse((Gender=="Female" & PA %in% c(36:48)) |
                               (Gender=="Male" & PA %in% c(35:48)), 1,
                             ifelse ((Gender=="Female" & PA %in% c(28:35))|
                                       (Gender=="Male" & PA %in% c(28:34)), 2,
                                     ifelse ((Gender=="Female" & PA %in% c(22:27))|
                                               (Gender=="Male" & PA %in% c(23:27)), 3, 5))))

#count integral burnout scores
dataset <- dataset %>% mutate (Burnout = EE_score+DP_score+RPA_score)

#create factor with burnout groups
dataset <- dataset %>% mutate (Burnout_group = ifelse(Burnout %in% c(1:4), 1, 
                                                      ifelse(Burnout %in% c(5:6), 2,
                                                             ifelse(Burnout %in% c(7:9), 3, 4))))
dataset$Burnout_group <- factor(dataset$Burnout_group, 
                                levels = c(1,2,3,4),
                                labels = c('Low', 'Middle', 'High', 'Very_high'))

#summary of GB, EE, DP and RPA
dataset %>% dplyr::select(Burnout, EE, DP, RPA, LOC) %>% summary()
tb <- tibble(Burnout = dataset$Burnout, EE = dataset$EE, DP = dataset$DP, RPA = dataset$RPA, LOC = dataset$LOC)
t(apply(tb, 2, summary))

#histogram with burnout
dataset %>%  
  ggplot(aes(Burnout, fill=Burnout_group)) + 
  geom_histogram(alpha = 0.8, position = "stack", binwidth=1) + 
  scale_fill_grey() + theme_classic()

#histogram with emotional exhaustion
dataset %>%  
  ggplot(aes(EE)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with depersonalization
dataset %>%  
  ggplot(aes(DP)) + 
  geom_histogram(alpha = 0.8, position = "stack", binwidth=1) + 
  scale_fill_grey() + theme_classic()

#histogram with RPA
dataset %>%  
  ggplot(aes(RPA)) + 
  geom_histogram(alpha = 0.8, position = "stack", binwidth=1) + 
  scale_fill_grey() + theme_classic()

##LOC data

#histogram with LOC
dataset %>%  
  ggplot(aes(LOC)) + 
  geom_histogram(alpha = 0.8, position = "stack", binwidth=1) + 
  scale_fill_grey() + theme_classic()

#view LOC range among participants
table(dataset$LOC)

#count LOC scores of all participants with middle or higher level of burnout
high_burnout_dataset <- dataset %>% subset(Burnout_group %in% c("Middle","High", "Very_high"))
table(high_burnout_dataset$LOC)

### ANALYSIS

##General tendencies

#remove outliers
dataset <- dataset %>% filter (DP %in% c(2:20)) %>% filter (LOC %in% c(13:40))

#check normality of LOC
shapiro.test (dataset$LOC)
qqnorm(dataset$LOC, main="")
qqline(dataset$LOC, col=2)

#check normality of EE
shapiro.test (dataset$EE)
qqnorm(dataset$EE, main="")
qqline(dataset$EE, col=2)

#check normality of DP
shapiro.test (dataset$DP)
qqnorm(dataset$DP, main="")
qqline(dataset$DP, col=2)

#check normality of RPA
shapiro.test (dataset$RPA)
qqnorm(dataset$RPA, main="")
qqline(dataset$RPA, col=2)

#check normality of burnout  
shapiro.test (dataset$Burnout)
qqnorm(dataset$Burnout, main="")
qqline(dataset$Burnout, col=2)

#correlation for LOC and burnout
cor.test(dataset$LOC, dataset$Burnout, method="kendall")

#correlation for LOC and EE
cor.test(dataset$LOC, dataset$EE, method="pearson")

#plot with relationships between LOC and EE
dataset %>%
  ggplot(aes(x=LOC, y=EE)) + 
  geom_point() +
  theme_classic()

#correlation for LOC and DP
cor.test(dataset$LOC, dataset$DP, method="pearson")

#plot with relationships between LOC and DP
dataset %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point() +
  theme_classic()

#correlation for LOC and RPA
cor.test(dataset$LOC, dataset$RPA, method="pearson")

#plot with relationships between LOC and RPA
dataset %>%
  ggplot(aes(x=LOC, y=RPA)) + 
  geom_point() +
  theme_classic()

#plot relationships between DP+LOC+RPA
DP_LOC_RP <- data.frame (
  DP=dataset$DP, 
  LOC=dataset$LOC, 
  RPA=dataset$RPA)
plot(DP_LOC_RP)

#correlation for DP and RPA
cor.test(dataset$DP, dataset$RPA, method="pearson")

#correlation for EE and RPA
cor.test(dataset$EE, dataset$RPA, method="pearson")

#correlation for EE and DP
cor.test(dataset$EE, dataset$DP, method="pearson")

#plot relationships between DP, EE and RPA
DP_EE_RP <- data.frame (
  DP=dataset$DP, 
  EE=dataset$EE, 
  RPA=dataset$RPA)
plot(DP_EE_RP)

## Prepare second stage group (middle level of DP+RPA)

#creating subset with the participants on second stage (3:4 level of DP+RPA) 
dataset_mid_DP_RPA <- dataset %>% filter ((DP_score+RPA_score) %in% c(3:4))

#counting number of participants in 3:4 levels of DP+RPA
nrow(dataset_mid_DP_RPA)

#summary of EE, DP and RPA on second stage
dataset_mid_DP_RPA %>% dplyr::select(EE, DP, RPA, Burnout, LOC) %>% summary()

dataset_mid_DP_RPA %>% dplyr::select(EE_score, DP_score, RPA_score) %>% summary()

#check new subset for normality of EE, DP, RPA and LOC

#check normality of EE
shapiro.test (dataset_mid_DP_RPA$EE)
qqnorm(dataset_mid_DP_RPA$EE, main="")
qqline(dataset_mid_DP_RPA$EE, col=2)

#check normality of DP
shapiro.test (dataset_mid_DP_RPA$DP)
qqnorm(dataset_mid_DP_RPA$DP, main="")
qqline(dataset_mid_DP_RPA$DP, col=2)

#check normality of RPA
shapiro.test (dataset_mid_DP_RPA$RPA)
qqnorm(dataset_mid_DP_RPA$RPA, main="")
qqline(dataset_mid_DP_RPA$RPA, col=2)

#check normality of LOC
shapiro.test (dataset_mid_DP_RPA$LOC)
qqnorm(dataset_mid_DP_RPA$LOC, main="")
qqline(dataset_mid_DP_RPA$LOC, col=2)

## Prepare third stage group (high level of DP+RPA)

#creating subset with the participants in 5 and higher levels of DP and RPA development
dataset_high_DP_RPA <- dataset %>% filter ((DP_score+RPA_score)>4)

#counting number of participants in 5 and higher levels of DP+RPA development
nrow(dataset_high_DP_RPA)
nrow(dataset_mid_DP_RPA)

#summary of EE, DP and RPA in 5 and higher levels of DP+RPA
dataset_high_DP_RPA %>% dplyr::select(EE, DP, RPA, Burnout, LOC) %>% summary()

#check new subset for normality of EE, DP, RPA and LOC

#check normality of EE
shapiro.test (dataset_high_DP_RPA$EE)
qqnorm(dataset_high_DP_RPA$EE, main="")
qqline(dataset_high_DP_RPA$EE, col=2)

#check normality of DP
shapiro.test (dataset_high_DP_RPA$DP)
qqnorm(dataset_high_DP_RPA$DP, main="")
qqline(dataset_high_DP_RPA$DP, col=2)

#check normality of RPA
shapiro.test (dataset_high_DP_RPA$RPA)
qqnorm(dataset_high_DP_RPA$RPA, main="")
qqline(dataset_high_DP_RPA$RPA, col=2)

#check normality of LOC
shapiro.test (dataset_high_DP_RPA$LOC)
qqnorm(dataset_high_DP_RPA$LOC, main="")
qqline(dataset_high_DP_RPA$LOC, col=2)

## Moderation analysis 

## H1a. DP~EE*LOC (second stage)

#LM DP~LOC 
summary(lm (scale(dataset_mid_DP_RPA$DP, scale=F) ~ scale(dataset_mid_DP_RPA$LOC, scale=F)))
#plot
dataset_mid_DP_RPA %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#LM DP~EE 
summary(lm (scale(dataset_mid_DP_RPA$DP, scale=F) ~ scale(dataset_mid_DP_RPA$EE, scale=F)))

#plot
dataset_mid_DP_RPA %>%
  ggplot(aes(x=EE, y=DP)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~EE*LOC with QuantPsyc
lm.mod.DP <- moderate.lm(EE, LOC, DP, dataset_mid_DP_RPA, mc=FALSE)
slopes.mod.DP <- sim.slopes(lm.mod.DP,meanCenter(dataset_mid_DP_RPA$LOC))
summary(lm.mod.DP)
slopes.mod.DP
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.DP,EE,DP,dataset_mid_DP_RPA,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Depersonalization")

##plot DP, LOC and EE
#DP_LOC_EE <- data.frame (
#  DP=dataset_mid_DP_RPA$DP, 
#  LOC=dataset_mid_DP_RPA$LOC, 
#  EE=dataset_mid_DP_RPA$EE)
#plot(DP_LOC_EE)

## H2a. RPA~EE*LOC (second stage)

#LM RPA~EE 
summary(lm (scale(dataset_mid_DP_RPA$RPA, scale=F) ~ scale(dataset_mid_DP_RPA$EE, scale=F)))

#plot
dataset_mid_DP_RPA %>%
  ggplot(aes(x=EE, y=RPA)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~EE*LOC with QuantPsyc
lm.mod.RPA <- moderate.lm(EE, LOC, RPA, dataset_mid_DP_RPA, mc=FALSE)
slopes.mod.RPA <- sim.slopes(lm.mod.RPA,meanCenter(dataset_mid_DP_RPA$LOC))
summary(lm.mod.RPA)
slopes.mod.RPA
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.RPA,EE,RPA,dataset_mid_DP_RPA,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Reduction of PA")

## H1b. DP~EE*LOC (third stage) 

#LM DP~EE 
summary(lm (scale(dataset_high_DP_RPA$DP, scale=F) ~ scale(dataset_high_DP_RPA$EE, scale=F)))

#cor DP~EE
cor.test(dataset_high_DP_RPA$DP, dataset_high_DP_RPA$EE, method="pearson")

#plot
dataset_high_DP_RPA %>%
  ggplot(aes(x=EE, y=DP)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~EE*LOC with QuantPsyc
lm.mod.high.DP <- moderate.lm(EE, LOC, DP, dataset_high_DP_RPA, mc=FALSE)
slopes.mod.high.DP <- sim.slopes(lm.mod.high.DP,meanCenter(dataset_high_DP_RPA$LOC))
summary(lm.mod.high.DP)
slopes.mod.high.DP
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.high.DP,EE,DP,dataset_high_DP_RPA,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Depersonalization")

## H2b. RPA~EE*LOC (third stage)

#LM RPA~EE 
summary(lm (scale(dataset_high_DP_RPA$RPA, scale=F) ~ scale(dataset_high_DP_RPA$EE, scale=F)))

#cor RPA~EE
cor.test(dataset_high_DP_RPA$RPA, dataset_high_DP_RPA$EE, method="pearson")

#plot
dataset_high_DP_RPA %>%
  ggplot(aes(x=EE, y=RPA)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis RPA~EE*LOC with QuantPsyc
lm.mod.high.RPA <- moderate.lm(EE, LOC, RPA, dataset_high_DP_RPA, mc=FALSE)
slopes.mod.high.RPA <- sim.slopes(lm.mod.high.RPA,meanCenter(dataset_high_DP_RPA$LOC))
summary(lm.mod.high.RPA)
slopes.mod.high.RPA
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.high.RPA,EE,RPA,dataset_high_DP_RPA,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Reduction of PA")

dataset_mid_DP_RPA %>% 
  filter (DP < 7 & LOC < 30)

## Additional. DP~EE*LOC (all participants)

#LM DP~LOC 
summary(lm (scale(dataset$DP, scale=F) ~ scale(dataset$LOC, scale=F)))
#plot
dataset %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#LM DP~EE 
summary(lm (scale(dataset$DP, scale=F) ~ scale(dataset$EE, scale=F)))

#plot
dataset %>%
  ggplot(aes(x=EE, y=DP)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~EE*LOC with QuantPsyc
lm.mod.DP <- moderate.lm(EE, LOC, DP, dataset, mc=FALSE)
slopes.mod.DP <- sim.slopes(lm.mod.DP,meanCenter(dataset$LOC))
summary(lm.mod.DP)
slopes.mod.DP
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.DP,EE,DP,dataset,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Depersonalization")

##plot DP, LOC and EE
#DP_LOC_EE <- data.frame (
#  DP=dataset$DP, 
#  LOC=dataset$LOC, 
#  EE=dataset$EE)
#plot(DP_LOC_EE)

## Additional. RPA~EE*LOC (all participants)

#LM RPA~LOC 
summary(lm (scale(dataset$RPA, scale=F) ~ scale(dataset$LOC, scale=F)))
#plot
dataset %>%
  ggplot(aes(x=LOC, y=RPA)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#LM RPA~EE 
summary(lm (scale(dataset$RPA, scale=F) ~ scale(dataset$EE, scale=F)))

#plot
dataset %>%
  ggplot(aes(x=EE, y=RPA)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~EE*LOC with QuantPsyc
lm.mod.RPA <- moderate.lm(EE, LOC, RPA, dataset, mc=FALSE)
slopes.mod.RPA <- sim.slopes(lm.mod.RPA,meanCenter(dataset$LOC))
summary(lm.mod.RPA)
slopes.mod.RPA
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.RPA,EE,RPA,dataset,
          title="Interaction in different LOC", xlab="Emotional exhaustion", ylab="Reduction of PA")

##RPA and DP
#LM RPA~DP 
summary(lm (scale(dataset$RPA, scale=F) ~ scale(dataset$DP, scale=F)))

#plot
dataset %>%
  ggplot(aes(x=DP, y=RPA)) + 
  geom_point(color=wes_palette(n=1, name="Royal1")) +
  geom_smooth(method="lm", color=wes_palette(n=1, name="Royal1")) +
  theme_classic()

#moderation analysis DP~DP*LOC with QuantPsyc
lm.mod.RPA <- moderate.lm(DP, LOC, RPA, dataset, mc=FALSE)
slopes.mod.RPA <- sim.slopes(lm.mod.RPA,meanCenter(dataset$LOC))
summary(lm.mod.RPA)
slopes.mod.RPA
# when executing hraph.mod use mouse click to place legend
graph.mod(slopes.mod.RPA,DP,RPA,dataset,
          title="Interaction in different LOC", xlab="Depersonalization", ylab="Reduction of PA")

rm(list = ls())

# This is code to replicate the analyses and figures from my 2020 Burnout and Locus of Control
# paper. Code developed by Alena Egorova (@alvegorova)

### DOWNLOAD LIBRARIES

install.packages(c("psych", "ggplot2", "pspearman", "wesanderson"))
require(ggplot2)
require(dplyr)
require(tidyr)
require(foreign) #to use "read.spss"
require(pspearman) #for Spearman correlation
require(psych) #for moderation analysis
require(wesanderson) #to use wesanderson colors

### DOWNLOAD DATA

setwd("~/Documents/Data_Analysis/Burnout_and_LOC")
dataset = read.spss("Data.sav", to.data.frame=TRUE)

### PREPARE DATA

#remove empty rows
dataset <- dataset[1:93,]

#make variable names more readable
dataset$male <- factor(dataset$male, labels = c('Male', 'Female'))
names(dataset)[names(dataset) == 'male'] <- 'Gender'
#names(dataset)[names(dataset) == 'Burnout_group'] <- 'Burnout_two_groups'
#names(dataset)[names(dataset) == 'Burnout_gr2'] <- 'Burnout_group'
#names(dataset)[names(dataset) == 'LO_gr_1_2'] <- 'LOC_groups'
names(dataset)[names(dataset) == 'USK_Io'] <- 'LOC'
#names(dataset)[names(dataset) == 'USK_Ip'] <- 'LOC_prof'
names(dataset)[names(dataset) == 'age'] <- 'Age'
names(dataset)[names(dataset) == 'nn'] <- 'Number'
names(dataset)[names(dataset) == 'stage_com'] <- 'Work_experience'
names(dataset)[names(dataset) == 'stage_sch'] <- 'Last_work_experience'
names(dataset)[names(dataset) == 'Em_exhaustion'] <- 'EE'
names(dataset)[names(dataset) == 'Depersonalization'] <- 'DP'
names(dataset)[names(dataset) == 'Successfulness'] <- 'PA'
names(dataset)[names(dataset) == 'stage_sch'] <- 'Last_work_experience'

#names(dataset)[names(dataset) == 'Eitimia'] <- 'Euthymia'
#names(dataset)[names(dataset) == 'Distimia'] <- 'Dysthymia'

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

#Why this does not work?!:  
#dataset <- dataset %>% as.integer(N, LOC, LOC_prof,
#           Euthymia, Dysthymia, Depression, Anxiety,
#           EE, Depersonalization, PA, 
#           Burnout)

#keep table with sd
dataset_with_sd <- dataset

#choose important variables
dataset <- dataset %>% 
  select (Number, Age, Gender, Work_experience, Last_work_experience, 
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

#histogram with age and gender
dataset %>% filter (!is.na(Age)) %>% 
  ggplot(aes(Age, fill=Gender)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with Work_experience
dataset %>%  
  ggplot(aes(Work_experience)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with Last_work_experience
dataset %>%  
  ggplot(aes(Last_work_experience)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#how many participants have been working at current place less then 10 years
dataset %>% filter (Last_work_experience<10) %>% nrow()

#bar with teaching subject
dataset %>%  
  ggplot(aes(Subject)) + 
  geom_bar(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#Does not work:
# install.packages("treemap", dependencies = TRUE)
# library(treemap)
# treemap(dataset$Subject, index="cat", vSize="pct", vColor="col", type="color")

## Burnout data

#histogram with emotional exhaustion
dataset %>%  
  ggplot(aes(EE)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with depersonalization
dataset %>%  
  ggplot(aes(DP)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#reverse Personal Accomplishment scale to Reduced Personal accomplishment scale
dataset <- dataset %>% mutate (RPA = 48 - PA)

#histogram with RPA
dataset %>%  
  ggplot(aes(RPA)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

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
dataset <- dataset %>% mutate (Burnout_group = ifelse(Burnout %in% c(3:4), 1, 
                                                       ifelse(Burnout %in% c(5:6), 2,
                                                              ifelse(Burnout %in% c(7:9), 3, 4))))
dataset$Burnout_group <- factor(dataset$Burnout_group, 
                                 levels = c(1,2,3,4),
                                 labels = c('Low', 'Middle', 'High', 'Very_high'))

#histogram with burnout
dataset %>%  
  ggplot(aes(Burnout, fill=Burnout_group)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

##LOC data

#histogram with LOC
dataset %>%  
  ggplot(aes(LOC)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#view LOC range among participants
table(dataset$LOC)

#count how many participants are externals in different burnout scores
xtabs (~ (LOC<22)+ Burnout, data=dataset)


### ANALYSIS

## H1

#check normality of EE   -- I should delete ties?
shapiro.test (dataset$EE)
ks.test (dataset$EE,pnorm)
qqnorm(dataset$EE, main="")
qqline(dataset$EE, col=2)

#check normality of DP
shapiro.test (dataset$DP)
ks.test (dataset$DP,pnorm)
qqnorm(dataset$DP, main="")
qqline(dataset$DP, col=2)

#check normality of RPA  -- I should delete ties?
shapiro.test (dataset$RPA)
ks.test (dataset$RPA,pnorm)
qqnorm(dataset$RPA, main="")
qqline(dataset$RPA, col=2)

#check normality of burnout  -- I should delete ties?
shapiro.test (dataset$Burnout)
ks.test (dataset$Burnout,pnorm)
qqnorm(dataset$Burnout, main="")
qqline(dataset$Burnout, col=2)

#check normality of LOC  -- I should delete ties?
shapiro.test (dataset$LOC)
ks.test (dataset$LOC,pnorm)
qqnorm(dataset$LOC, main="")
qqline(dataset$LOC, col=2)

#H1a

#count LOC scores of all participants with high and very high levels of burnout
high_burnout_dataset <- dataset %>% subset(Burnout_group %in% c("High", "Very_high"))
table(high_burnout_dataset$LOC)

#H1b

#Spearman correlation for LOC and burnout
cor.test(dataset$LOC, dataset$Burnout, method="kendall")

#Plot with relationships between LOC and Burnout
dataset %>%
  ggplot(aes(x=LOC, y=Burnout)) + 
  geom_point() +
  theme_classic()

#Spearman correlation for LOC and EE
spearman.test(dataset$LOC, dataset$EE)

#Plot with relationships between LOC and EE
dataset %>%
  ggplot(aes(x=LOC, y=EE)) + 
  geom_point() +
  theme_classic()

#Spearman correlation for LOC and DP
spearman.test(dataset$LOC, dataset$DP)

#Plot with relationships between LOC and DP
dataset %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point() +
  theme_classic()

#Spearman correlation for LOC and RPA
spearman.test(dataset$LOC, dataset$RPA)

#Plot with relationships between LOC and RPA
dataset %>%
  ggplot(aes(x=LOC, y=RPA)) + 
  geom_point() +
  theme_classic()

#Scatterplots with relationships between DP+LOC+RPA
DP_LOC_RP <- data.frame (
  DP=dataset$DP, 
  LOC=dataset$LOC, 
  RPA=dataset$RPA)
plot(DP_LOC_RP)

#Spearman correlation for EE and RPA
spearman.test(dataset$EE, dataset$RPA)

#Spearman correlation for EE and DP
spearman.test(dataset$EE, dataset$DP)

#Spearman correlation for DP and RPA
spearman.test(dataset$DP, dataset$RPA)

DP_EE_RP <- data.frame (
  DP=dataset$DP, 
  EE=dataset$EE, 
  RPA=dataset$RPA)
plot(DP_EE_RP)

##H2

## FOR 3:4 level
#creating subset with the participants in 3:4 level of DP and RP development
dataset_middle_high_DP_RP_2 <- dataset %>% filter ((DP_score+RPA_score) %in% c(3:4))

#Spearman correlation for LOC and DP in the middle level of DP and RP development
spearman.test(dataset_middle_high_DP_RP_2$LOC, dataset_middle_high_DP_RP_2$DP)

#Creating ggplot with LOC and DP in the teachers with middle level of DP and RP
dataset_middle_high_DP_RP_2 %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point() +
  geom_line() + theme_classic()

#Spearman correlation for LOC and RP in the middle level of DP and RP development
spearman.test(dataset_middle_high_DP_RP_2$LOC, dataset_middle_high_DP_RP_2$RPA)

#Creating ggplot with LOC and RP in the teachers with middle level of DP and RP
dataset_middle_high_DP_RP_2 %>%
  ggplot(aes(x=LOC, y=RPA)) + 
  geom_point() +
  geom_line() + theme_classic()

#FOR 3 level
#creating subset with the participants in 3 level of DP and RP development
dataset_middle_DP_RP_2 <- dataset %>% filter ((DP_score+RPA_score) %in% c(3))

#Spearman correlation for LOC and DP in the middle level of DP and RP development
spearman.test(dataset_middle_DP_RP_2$LOC, dataset_middle_DP_RP_2$DP)

#Creating ggplot with LOC and DP in the teachers with middle level of DP and RP
dataset_middle_DP_RP_2 %>%
  ggplot(aes(x=LOC, y=DP)) + 
  geom_point() +
  geom_line() + theme_classic()

#Spearman correlation for LOC and RP in the middle level of DP and RP development
spearman.test(dataset_middle_DP_RP_2$LOC, dataset_middle_DP_RP_2$RPA)

#Creating ggplot with LOC and RP in the teachers with middle level of DP and RP
dataset_middle_DP_RP_2 %>%
  ggplot(aes(x=LOC, y=RPA)) + 
  geom_point() +
  geom_line() + theme_classic()


### MODERATION  

##WITH DUMMY


?lm()

#_____


install.packages("rockchalk")
library(rockchalk)
plotSlopes((lm(DP ~ EE*LOC, data=dataset_middle_DP_RP_2)), 
           plotx="EE",
           modx="LOC", modxVals="std.dev.")


install.packages("mediation")
require (mediation)

##BY HAND (error when try to use ordered variables)

##IN DATASET (where variables perceived as numeric)
#check regression DP~EE
summary(lm(DP ~ EE, data=dataset))
#check moderation of DP~EE+LOC (where variables perceived as numeric)
summary(lm(DP ~ EE*LOC, data=dataset))

##IN LEVEL 2 
#check regression DP~EE (where variables perceived as numeric)
summary(lm(DP ~ EE, data=dataset_middle_DP_RP_2))
#check moderation of DP~EE+LOC 
summary(lm(DP ~ EE*LOC, data=dataset_middle_DP_RP_2))

#in order to make EE estimation interpretable, here we centering LOC
dataset_middle_DP_RP_2 <- dataset_middle_DP_RP_2 %>% mutate(LOC_centered=LOC-mean(LOC))
#and calculate lm() with LOC centered
summary(lm(DP ~ EE*LOC_centered, data=dataset_middle_DP_RP_2))

#check moderation of RP~EE+LOC (where variables perceived as numeric)
summary(lm(RPA ~ EE*LOC, data=dataset_middle_DP_RP_2))

#why impact from EE on DP here non-significant?
summary(lm(DP ~ EE, data=dataset_middle_DP_RP_2))


##WITH LAVAAN (error when try to use ordered variables)

install.packages("lavaan")
require (lavaan)
lm_D_E_L <- lm(DP ~ EE*LOC_centered, data=dataset_middle_DP_RP_2)
lavaan(model = DP ~ EE + LOC + EE:LOC, data=dataset_middle_DP_RP_2, ordered = TRUE)

summary(lavaan(model=(DP_ordered~EE_ordered+LOC_ordered+EE_ordered:LOC_ordered), data=dataset))
summary(lavaan(model=DP~EE+LOC+EE:LOC, data=dataset))

#check moderation of DP~EE+LOC and RP~EE+LOC, where variables perceived as factors
is.factor(dataset$DP_ordered2)
dataset$EE_ordered <- factor(dataset$EE, ordered=TRUE)
dataset$DP_ordered <- factor(dataset$DP, ordered=TRUE)
dataset$RP_ordered <- factor(dataset$RPA, ordered=TRUE)
dataset$LOC_ordered <- factor(dataset$LOC, ordered=TRUE)

install.packages("MeMoBootR")

##WITH QuantPsyc (CAN MAKE CENTERED, BUT ERRORS WHEN USE ORDERED)

install.packages("QuantPsyc")
require (QuantPsyc)

lm.mod1 <- moderate.lm(EE, LOC, DP, dataset, mc=FALSE)
ss.mod1 <- sim.slopes(lm.mod1,meanCenter(dataset$LOC))
summary(lm.mod1)
ss.mod1
# use mouse click to place legend in graph.mod
graph.mod(ss.mod1,EE,DP,dataset,"Interaction Example")

#the same with ordered
lm.mod2 <- moderate.lm(EE_ordered, LOC_ordered, DP_ordered, dataset, mc=FALSE)
ss.mod1 <- sim.slopes(lm.mod1,meanCenter(dataset$LOC_ordered))
summary(lm.mod1)
ss.mod1
# use mouse click to place legend in graph.mod
graph.mod(ss.mod1,EE_ordered,DP_ordered,dataset,"Interaction Example")


##WITH INTERACTIONS (ERROR)
install.packages("interactions")
require (interactions)

cat_plot(model=lm(DP_ordered ~ EE_ordered*LOC_ordered, data=dataset), pred=EE_ordered, modx=LOC_ordered, data=dataset)

##BY HAND
summary(lm(DP ~ EE*LOC, data=dataset))
#Ordered do not work BY HAND (error)
summary(lm(DP_ordered ~ EE_ordered*LOC_ordered, data=dataset))

##WITH MeMoBootR

install.packages("devtools")
devtools::install_github("doomlab/MeMoBootR")
library(MeMoBootR)

library(diagram)
library(shape)
??MeMoBootR



#multiple regression: Depersonalization ~ Burnout + LOC

dep.mult.reg <- data.frame (
  Depersonalization=dataset$DP, 
  LOC_prof=dataset$LOC_prof, 
  Burnout=dataset$Burnout)
plot(dep.mult.reg)

summary(lm(DP ~ Burnout+LOC_prof, data=dataset))
summary(lm(DP ~ Burnout, data=dataset))
summary(lm(DP ~ LOC_prof, data=dataset))
new_plot <- lm(DP ~ Burnout, data=dataset)
plot(lm(DP ~ Burnout, data=dataset))

dataset %>% 
  ggplot(aes(Burnout,DP, col=LOC_prof_groups)) +
  geom_point() +
  geom_smooth(method = "lm")

#Without middle LOC_prof (taking just ends to detect the difference)
hist(dataset$LOC_prof) #7 is the middle
dataset_2 <- dataset %>% filter (LOC_prof<7 | LOC_prof>7)
xtabs (~ LOC_prof_groups + Burnout_group, data=dataset) #how much we had
xtabs (~ LOC_prof_groups + Burnout_group, data=dataset_2) #how much we have without 7 in LOC_prof

#USING LM compare what has more influence burnout or LOC (when burnout medium)
dataset_3 <- dataset_2 %>% filter (Burnout_group=="Burnout_medium") 
xtabs (~LOC_prof_groups + DP, data=dataset_3)
xtabs (~LOC_prof_groups + RPA, data=dataset_3)
summary(lm(DP ~ EE+LOC_prof, data=dataset_3))  
summary(lm(RPA ~ EE+LOC_prof, data=dataset_3)) 

#regression between RPA and Depersonalization (in all data_)
#when Burnout_medium
dataset_4 <- dataset %>% filter (Burnout_group=="Burnout_medium") 
summary(lm(RPA ~ DP, data=dataset_4)) 
#when Burnout_high
dataset_Burn_high <- dataset %>% filter (Burnout_group=="Burnout_high") 
summary(lm(RPA ~ DP, data=dataset_Burn_high)) 
#when Burnout_low
dataset_Burn_low <- dataset %>% filter (Burnout_group=="Burnout_low") 
summary(lm(RPA ~ DP, data=dataset_Burn_low)) 

### GROUPS OF BURNOUT AND LOC

#creating variable with three levels of Burnout
dataset$Burnout_group <- factor (dataset$Burnout_group,
                                        levels=c(1,2,3),
                                        labels=c("Burnout_low", "Burnout_medium", "Burnout_high"))

#creating variable with External and Internal general LOC
dataset$LOC_groups <- factor (dataset$LOC_groups,
                                  levels=c(1,2),
                                  labels=c("External", "Internal"))

#creating variable with External and Internal LOC in professional domain
dataset <- mutate (dataset, LOC_prof_groups = ifelse(dataset$LOC_prof<7, 1, 2))
dataset$LOC_prof_groups <- factor (dataset$LOC_prof_groups,
                                   levels=c(1,2),
                                   labels=c("External", "Internal"))

#calculate the number of participans in each LOC_prof/Burnout group
#xtabs (~ LOC_prof_groups + Burnout_group, data=dataset)
xtabs (~ LOC_groups + Burnout_group, data=dataset)


### SEMANTIC DIFFERENTIAL

#making SD varidables more readable
names(dataset)[names(dataset) == 'SD_St_Tyajeliy'] <- 'Student_Heavy'
#ADD_THE_REST!

dataset_with_sd_MB <- dataset_with_sd %>% 
  filter (Burnout_group=='Burnout_medium')

dataset_with_sd_MB_Ex <- dataset_with_sd_MB %>% 
  filter (LOC_groups=="External")

dataset_with_sd_MB_In <- dataset_with_sd_MB %>% 
  filter (LOC_groups=="Internal")

SD_col <- colnames(dataset_with_sd_MB) [colnames(dataset_with_sd_MB) %>% startsWith("SD_")]
dataset_with_sd_MB_piv <- pivot_longer(dataset_with_sd_MB, SD_col, names_to = c("Object","Scale"), 
                                       names_prefix = "SD_", names_sep = "_", values_to = "SD_value", values_drop_na =TRUE)

dataset_with_sd_MB_piv %>% 
  ggplot(aes(SD_value, fill=Object)) + 
  geom_density(alpha = 0.8, position = "identity") + 
  facet_grid(Scale~LOC_groups, shrink=TRUE)

dataset_with_sd_MB_piv %>% 
  ggplot(aes(SD_value, fill=Object)) + 
  geom_histogram(alpha = 0.8, position = "identity") + 
  facet_grid(Scale~LOC_groups, shrink=TRUE)


SD_St_cols <- colnames(dataset_with_sd_MB) [colnames(dataset_with_sd_MB) %>% startsWith("SD_St")]

# TODO create empty data frame

for(stcolname in SD_St_cols) {
  
  st <- (dataset_with_sd_MB_Ex  %>% select(!!as.name(stcolname)))
  
  scalename <- strsplit(stcolname, "_")[[1]][3]
  
  chcolname <- paste("SD_Ch_", scalename, sep="")
  ch <- (dataset_with_sd_MB_Ex  %>% select(!!as.name(chcolname)))
  
  res <- wilcox.test(st[,1], ch[,1] , mu=0, paired = TRUE, alternative = "two.sided", conf.int=TRUE)
  
  # TODO replace `print` with adding a row to the resulting data frame
  print(paste(scalename, (res)["p.value"], (res)["estimate"]))
} 

#Internal:
for(stcolname in SD_St_cols) {
  
  st <- (dataset_with_sd_MB_In  %>% select(!!as.name(stcolname)))
  
  scalename <- strsplit(stcolname, "_")[[1]][3]
  
  chcolname <- paste("SD_Ch_", scalename, sep="")
  ch <- (dataset_with_sd_MB_In  %>% select(!!as.name(chcolname)))
  
  res <- wilcox.test(st[,1], ch[,1] , mu=0, paired = TRUE, alternative = "two.sided", conf.int=TRUE)
  
  # TODO replace `print` with adding a row to the resulting data frame
  print(paste(scalename, (res)["p.value"], (res)["estimate"]))
}


student <- dataset (SD_St_Holodniy, SD_St_Molodoy, )
?wilcox.test(x, y, paired = TRUE, alternative = "two.sided")

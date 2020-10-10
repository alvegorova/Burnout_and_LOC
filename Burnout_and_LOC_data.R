rm(list = ls())

# This is code to replicate the analyses and figures from my 2020 Burnout and Locus of Control
# paper. Code developed by Alena Egorova (@alvegorova)

### DOWNLOADING LIBRARIES

install.packages(c("psych", "ggplot2"))
install.packages("wesanderson") #to use wesanderson colors
install.packages("pspearman") 
require(pspearman)
require(psych)
require(ggplot2)
require(dplyr)
require(tidyr)
require(foreign) #to use "read.spss"
require(wesanderson)


### DOWNLOADING DATA

setwd("~/Documents/Data_Analysis/Burnout_and_LOC")
dataset = read.spss("Data.sav", to.data.frame=TRUE)

### PREPARING DATA

#removing empty rows
dataset <- dataset[1:93,]

#making variable names more readable
dataset$male <- factor(dataset$male, labels = c('Male', 'Female'))
names(dataset)[names(dataset) == 'male'] <- 'Gender'
names(dataset)[names(dataset) == 'Burnout_groups'] <- 'Burnout_two_groups'
names(dataset)[names(dataset) == 'Burnout_gr2'] <- 'Burnout_three_groups'
names(dataset)[names(dataset) == 'LO_gr_1_2'] <- 'LOC_gen_groups'
names(dataset)[names(dataset) == 'USK_Io'] <- 'LOC_gen'
names(dataset)[names(dataset) == 'USK_Ip'] <- 'LOC_prof'
names(dataset)[names(dataset) == 'age'] <- 'Age'
names(dataset)[names(dataset) == 'nn'] <- 'N'
names(dataset)[names(dataset) == 'stage_com'] <- 'Experience_gen'
names(dataset)[names(dataset) == 'stage_sch'] <- 'Experience_last'
names(dataset)[names(dataset) == 'Eitimia'] <- 'Euthymia'
names(dataset)[names(dataset) == 'Distimia'] <- 'Dysthymia'

#edditing subject info
dataset$Subject <- factor(dataset$speciality1, labels = c('Math and engineering', 'Natural sciences', 'Humanities', 'Physical'))
levels(dataset$Subject) <- c(levels(dataset$Subject),'Elementary school','Not provided')
dataset$Subject[75] <- 'Humanities'
dataset$Subject[c(3,4,21,22,91)] <- 'Elementary school'
dataset$Subject[c(41,44,45,47,49,52,73,86)] <- 'Not provided'

#Editing format of numeric variables
dataset$N <- as.integer(dataset$N)
dataset$LOC_gen <- as.integer(dataset$LOC_gen)
dataset$LOC_prof <- as.integer(dataset$LOC_prof)
dataset$Euthymia <- as.integer(dataset$Euthymia)
dataset$Dysthymia <- as.integer(dataset$Dysthymia)
dataset$Depression <- as.integer(dataset$Depression)
dataset$Anxiety <- as.integer(dataset$Anxiety)
dataset$Em_exhaustion <- as.integer(dataset$Em_exhaustion)
dataset$Depersonalization <- as.integer(dataset$Depersonalization)
dataset$Successfulness <- as.integer(dataset$Successfulness)
dataset$Burnout <- as.integer(dataset$Burnout)

#Why this does not work?!:  
#dataset <- dataset %>% as.integer(N, LOC_gen, LOC_prof,
#           Euthymia, Dysthymia, Depression, Anxiety,
#           Em_exhaustion, Depersonalization, Successfulness, 
#           Burnout)

#keeping table with sd
dataset_with_sd <- dataset

#choosing important variables
dataset <- dataset %>% 
  select (N, Age, Gender, Experience_gen, Experience_last, 
          LOC_gen, LOC_gen_groups, 
          LOC_prof, LOC_prof_groups,
          Euthymia, Dysthymia, Depression, Anxiety,
          Em_exhaustion, Depersonalization, Successfulness, 
          Burnout, Burnout_two_groups, Burnout_three_groups)

#to overview the data
str(dataset)
View(dataset)

#Count missing data
colSums(is.na(dataset)) 

### DESCRIPTIVE STATISTICS

#general description of the participants
lapply(dataset[, c("Age", "Gender", "Experience_gen", "Subject", "LOC_gen", "Burnout")], table)

#histogram with age and gender
dataset %>% filter (!is.na(Age)) %>% 
  ggplot(aes(Age, fill=Gender)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with work experience
dataset %>%  
  ggplot(aes(Experience_gen)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#bar with teaching subject
dataset %>%  
  ggplot(aes(Subject)) + 
  geom_bar(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()
#Does not work:
# install.packages("treemap", dependencies = TRUE)
# library(treemap)
# treemap(dataset$Subject, index="cat", vSize="pct", vColor="col", type="color")

#histogram with burnout
dataset %>%  
  ggplot(aes(Burnout, col=Burnout_three_groups)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with Depersonalization
dataset %>%  
  ggplot(aes(Depersonalization)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#histogram with Successfulness
dataset %>%  
  ggplot(aes(Successfulness)) + 
  geom_histogram(alpha = 0.8, position = "stack") + 
  scale_fill_grey() + theme_classic()

#How many participants are Externals
xtabs (~ (LOC_gen<22)+ Burnout_three_groups, data=dataset)

#counting scores of emotional exhaustion (EE_score)
dataset <- dataset %>% 
  mutate (EE_score = ifelse((Gender=="Female" & Em_exhaustion %in% c(0:6)) |
                              (Gender=="Male" & Em_exhaustion %in% c(0:5)), 0,
                            ifelse ((Gender=="Female" & Em_exhaustion %in% c(6:16))|
                                      (Gender=="Male" & Em_exhaustion %in% c(5:15)), 1,
                                    ifelse ((Gender=="Female" & Em_exhaustion %in% c(17:25))|
                                              (Gender=="Male" & Em_exhaustion %in% c(16:24)), 2,
                                            ifelse ((Gender=="Female" & Em_exhaustion %in% c(26:34))|
                                                      (Gender=="Male" & Em_exhaustion %in% c(25:34)), 3, 5)))))


#counting scores of depersonalization (DP_score)
dataset <- dataset %>% 
  mutate (DP_score = ifelse((Gender=="Female" & Depersonalization %in% c(0:1)) |
                              (Gender=="Male" & Depersonalization %in% c(0:2)), 0,
                            ifelse ((Gender=="Female" & Depersonalization %in% c(1:4))|
                                      (Gender=="Male" & Depersonalization %in% c(2:4)), 1,
                                    ifelse ((Gender=="Female" & Depersonalization %in% c(5:10))|
                                              (Gender=="Male" & Depersonalization %in% c(5:12)), 2,
                                            ifelse ((Gender=="Female" & Depersonalization %in% c(11:13))|
                                                      (Gender=="Male" & Depersonalization %in% c(13:15)), 3, 5)))))

#counting scores of reduction of personal accomplishment (RP_score)
dataset <- dataset %>% 
  mutate (RP_score = ifelse((Gender=="Female" & Successfulness %in% c(36:48)) |
                              (Gender=="Male" & Successfulness %in% c(35:48)), 1,
                            ifelse ((Gender=="Female" & Successfulness %in% c(28:35))|
                                      (Gender=="Male" & Successfulness %in% c(28:34)), 2,
                                    ifelse ((Gender=="Female" & Successfulness %in% c(22:27))|
                                              (Gender=="Male" & Successfulness %in% c(23:27)), 3, 5))))

#counting integral burnout scores
dataset <- dataset %>% mutate (Burnout_score = EE_score+DP_score+RP_score)


#Create new dataset with Standardized Dep and Standardized Suc (and Suc is inverted)
dataset_new <- dataset
dataset_new <- dataset_new %>% mutate (Dep_stand = scale(Depersonalization))
dataset_new <- dataset_new %>% mutate (Suc_stand = scale(Successfulness))
dataset_new$Successfulness <- dataset_new$Successfulness*(-1)

#How in Internals changes Dep in dif levels of burnout
dataset %>% filter (LOC_gen>22) %>%
ggplot(aes(Depersonalization))+
  geom_boxplot() + 
  facet_wrap(~Burnout_three_groups)
  scale_color_grey() + theme_classic()

#How in Internals changes Suc in dif levels of burnout
  dataset %>% filter (LOC_gen>22) %>%
    ggplot(aes(Successfulness))+
    geom_boxplot() + 
    facet_wrap(~Burnout_three_groups)
  scale_color_grey() + theme_classic()
  
### SEM  
  install.packages("mediation")
  require (mediation)
  .libPaths 
### ANALYSIS
 

## H1

#Spearman correlation for LOC and burnout
spearman.test(dataset$LOC_gen, dataset$Burnout)

#Plot with relationships between LOC and Burnout
dataset %>%
  ggplot(aes(x=LOC_gen, y=Burnout)) + 
  geom_point() +
  theme_classic()

#Scatterplots with relationships between DP+LOC+RP
DP_LOC_RP <- data.frame (
  Depersonalization=dataset$Depersonalization, 
  LOC=dataset$LOC_gen, 
  Successfulness=dataset$Successfulness)
plot(DP_LOC_RP)

##H2

## FOR 3:4 level
#creating subset with the participants in 3:4 level of DP and RP development
dataset_middle_high_DP_RP_2 <- dataset %>% filter ((DP_score+RP_score) %in% c(3:4))

#Spearman correlation for LOC and DP in the middle level of DP and RP development
spearman.test(dataset_middle_high_DP_RP_2$LOC_gen, dataset_middle_high_DP_RP_2$Depersonalization)

#Creating ggplot with LOC and DP in the teachers with middle level of DP and RP
dataset_middle_high_DP_RP_2 %>%
  ggplot(aes(x=LOC_gen, y=Depersonalization)) + 
  geom_point() +
  geom_line() + theme_classic()

#Spearman correlation for LOC and RP in the middle level of DP and RP development
spearman.test(dataset_middle_high_DP_RP_2$LOC_gen, dataset_middle_high_DP_RP_2$Successfulness)

#Creating ggplot with LOC and RP in the teachers with middle level of DP and RP
dataset_middle_high_DP_RP_2 %>%
  ggplot(aes(x=LOC_gen, y=Successfulness)) + 
  geom_point() +
  geom_line() + theme_classic()

#FOR 3 level
#creating subset with the participants in 3 level of DP and RP development
dataset_middle_DP_RP_2 <- dataset %>% filter ((DP_score+RP_score) %in% c(3))

#Spearman correlation for LOC and DP in the middle level of DP and RP development
spearman.test(dataset_middle_DP_RP_2$LOC_gen, dataset_middle_DP_RP_2$Depersonalization)

#Creating ggplot with LOC and DP in the teachers with middle level of DP and RP
dataset_middle_DP_RP_2 %>%
  ggplot(aes(x=LOC_gen, y=Depersonalization)) + 
  geom_point() +
  geom_line() + theme_classic()

#Spearman correlation for LOC and RP in the middle level of DP and RP development
spearman.test(dataset_middle_DP_RP_2$LOC_gen, dataset_middle_DP_RP_2$Successfulness)

#Creating ggplot with LOC and RP in the teachers with middle level of DP and RP
dataset_middle_DP_RP_2 %>%
  ggplot(aes(x=LOC_gen, y=Successfulness)) + 
  geom_point() +
  geom_line() + theme_classic()



#Burnout in Internals vs burnout in Internals
ggplot(dataset, 
       aes(x=Burnout, col=LOC_gen_groups))+ 
  geom_density()+ 
  scale_colour_brewer(palette='Set3')

#Compare Internals and Externals
#Depersonalization in different LOC_prof level vs. Burnout level
dataset %>% filter (!is.na(LOC_gen_groups) & (!is.na(Depersonalization)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Depersonalization, fill = LOC_gen_groups)) + 
  geom_density(alpha = 0.4) + facet_grid (Burnout_three_groups~.)

#Successfulness in different LOC_prof level vs. Burnout level
dataset %>% filter (!is.na(LOC_gen_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Successfulness, fill = LOC_gen_groups)) + 
  geom_density(alpha = 0.4) + facet_grid (Burnout_three_groups~.)

## Plots with Depers in dif LOC vs dif Burnout
ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Depersonalization, col=LOC_gen_groups))+
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.2) +
  scale_color_grey() + theme_classic()

ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Successfulness, col=LOC_gen_groups))+
  geom_boxplot() + 
  scale_color_grey() + theme_classic()

#Plots with dinamics of Successfulness and Depers during Burnout in dif LOC
##Successfulness in Internal
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups)) & (LOC_prof_groups=='Internal')) %>% 
  ggplot(aes(Burnout, Successfulness)) + 
  geom_point()

##Successfulness in External
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups)) & (LOC_prof_groups=='External')) %>% 
  ggplot(aes(Burnout, Successfulness)) + 
  geom_point()

##Depersonalization in Internal
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups)) & (LOC_prof_groups=='Internal')) %>% 
  ggplot(aes(Burnout, Depersonalization)) + 
  geom_point()

##Depersonalization in External
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups)) & (LOC_prof_groups=='External')) %>% 
  ggplot(aes(Burnout, Depersonalization)) + 
  geom_point()

dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(LOC_prof, Depersonalization)) + 
  geom_point(alpha=0.5) +
  facet_wrap(Burnout_three_groups ~., scales="free_x") +
  geom_smooth()

dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(LOC_prof, Depersonalization)) + 
  geom_point(alpha=0.5) +
  facet_wrap(Burnout_three_groups ~. , scales="free_x") +
  geom_smooth()

##Dep in Ex/In in dif Burn
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Burnout, Depersonalization, color=LOC_gen_groups)) + 
  geom_point(alpha=0.5) +
  facet_wrap(Burnout_three_groups ~. , scales='free') +
  geom_smooth()

dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Burnout, Depersonalization, color=LOC_gen_groups)) + 
  geom_point(alpha=0.5) +
  facet_wrap(Burnout_three_groups ~. , scales='free') +
  geom_abline(intercept = mean(cor(dataset$Burnout, dataset$Depersonalization)), slope = sqrt((1-mean(cor(dataset$Burnout, dataset$Depersonalization))^2)/(91)))


##Succ in Ex/In in dif Burn
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Burnout, Successfulness, color=LOC_gen_groups)) + 
  geom_point(alpha=0.5) +
  facet_wrap(Burnout_three_groups ~. , scales='free') +
  geom_smooth()

#Analysis: Correlation

#Analysis: Two groups
#Для сравнения групп по Стьюденту проверить 
#(нормальность распределения (shapiro.test (2.2.4)) и гомогенность дисперсии)

#calculating groups' means in Depersonalization
mean (dataset$Depersonalization [dataset$LOC_prof<7 & dataset$Burnout_three_groups == 'Burnout_low'])
mean (dataset$Depersonalization [dataset$LOC_prof>6 & dataset$Burnout_three_groups == 'Burnout_low'])

mean (dataset$Depersonalization [dataset$LOC_prof<7 & dataset$Burnout_three_groups == 'Burnout_medium'])
mean (dataset$Depersonalization [dataset$LOC_prof>6 & dataset$Burnout_three_groups == 'Burnout_medium'])

mean (dataset$Depersonalization [dataset$LOC_prof<7 & dataset$Burnout_three_groups == 'Burnout_high'])
mean (dataset$Depersonalization [dataset$LOC_prof>6 & dataset$Burnout_three_groups == 'Burnout_high'])

#calculating groups' sizes
sum (dataset$LOC_prof<7 & dataset$Burnout_three_groups == 2)
sum (dataset$LOC_prof>6 & dataset$Burnout_three_groups == 2)

sum(dataset$LOC_prof<7 & dataset$Burnout_three_groups == 1)
sum(dataset$LOC_prof>6 & dataset$Burnout_three_groups == 1)

sum (dataset$LOC_prof<7 & dataset$Burnout_three_groups == 3)
sum (dataset$LOC_prof>6 & dataset$Burnout_three_groups == 3)

#Groups PROF
External_Burn_1 <- subset (dataset, 
                           dataset$LOC_prof<7 & dataset$Burnout_three_groups == 1)
Internal_Burn_1 <- subset (dataset, 
                           dataset$LOC_prof>6 & dataset$Burnout_three_groups == 1)

External_Burn_2 <- subset (dataset, 
                           dataset$LOC_prof<7 & dataset$Burnout_three_groups == 2)
Internal_Burn_2 <- subset (dataset, 
                           dataset$LOC_prof>6 & dataset$Burnout_three_groups == 2)

External_Burn_3 <- subset (dataset, 
                           dataset$LOC_prof<7 & dataset$Burnout_three_groups == 3)
Internal_Burn_3 <- subset (dataset, 
                           dataset$LOC_prof>6 & dataset$Burnout_three_groups == 3)

#or this way PROF
dataset %>% mutate (Group = ifelse(dataset$LOC_prof<7 & dataset$Burnout_three_groups == 1, External_Burn_1,
                                   ifelse(dataset$LOC_prof>6 & dataset$Burnout_three_groups == 1, Internal_Burn_1,
                                          ifelse(dataset$LOC_prof<7 & dataset$Burnout_three_groups == 2, External_Burn_2,
                                                 ifelse(dataset$LOC_prof>6 & dataset$Burnout_three_groups == 2, Internal_Burn_2,
                                                        ifelse(dataset$LOC_prof<7 & dataset$Burnout_three_groups == 3, External_Burn_3, "Internal_Burn_3"))))))

#!no, I should create groups by merging names in columns Burnout_three_groups and LOC_prof

#check if normally distributed
shapiro.test (dataset$Depersonalization)
qqnorm(dataset$Depersonalization, main="")
qqline(dataset$Depersonalization, col=2)

shapiro.test (External_Burn_1$Depersonalization)
ks.test (External_Burn_1$Depersonalization,pnorm)
qqnorm(External_Burn_2$Depersonalization, col="blue")
qqline(External_Burn_2$Depersonalization,col="red")

shapiro.test (dataset$Successfulness)
qqnorm(dataset$Successfulness, main="")
qqline(dataset$Successfulness, col=2)

shapiro.test (dataset$Burnout)
ks.test (dataset$Burnout,pnorm)
qqnorm(dataset$Burnout, main="")
qqline(dataset$Burnout, col=2)

#Mann-Whitney
wilcox.test(External_Burn_2$Depersonalization, Internal_Burn_2$Depersonalization, paired=FALSE)
wilcox.test(External_Burn_1$Depersonalization, Internal_Burn_1$Depersonalization, paired=FALSE)
wilcox.test(External_Burn_3$Depersonalization, Internal_Burn_3$Depersonalization, paired=FALSE)

wilcox.test(External_Burn_2$Successfulness, Internal_Burn_2$Successfulness, paired=FALSE)
wilcox.test(External_Burn_1$Successfulness, Internal_Burn_1$Successfulness, paired=FALSE)
wilcox.test(External_Burn_3$Successfulness, Internal_Burn_3$Successfulness, paired=FALSE)

#Groups GEN
GExternal_Burn_1 <- subset (dataset, 
                           dataset$LOC_gen_groups=="External" & dataset$Burnout_three_groups == "Burnout_low")
GInternal_Burn_1 <- subset (dataset, 
                           dataset$LOC_gen_groups=="Internal" & dataset$Burnout_three_groups == "Burnout_low")

GExternal_Burn_2 <- subset (dataset, 
                           dataset$LOC_gen_groups=="External" & dataset$Burnout_three_groups == "Burnout_medium")
GInternal_Burn_2 <- subset (dataset, 
                           dataset$LOC_gen_groups=="Internal" & dataset$Burnout_three_groups == "Burnout_medium")

GExternal_Burn_3 <- subset (dataset, 
                           dataset$LOC_gen_groups=="External" & dataset$Burnout_three_groups == "Burnout_high")
GInternal_Burn_3 <- subset (dataset, 
                           dataset$LOC_gen_groups=="Internal" & dataset$Burnout_three_groups == "Burnout_high")


#or this way GEN
dataset2 <- dataset %>% mutate (Group = ifelse(dataset$LOC_gen<31 & dataset$Burnout_three_groups == 1, GExternal_Burn_1,
                                   ifelse(dataset$LOC_gen>30 & dataset$Burnout_three_groups == 1, GInternal_Burn_1,
                                          ifelse(dataset$LOC_gen<31 & dataset$Burnout_three_groups == 2, GExternal_Burn_2,
                                                 ifelse(dataset$LOC_gen>30 & dataset$Burnout_three_groups == 2, GInternal_Burn_2,
                                                        ifelse(dataset$LOC_gen<31 & dataset$Burnout_three_groups == 3, GExternal_Burn_3, "GInternal_Burn_3"))))))

#Mann-Whitney GEN
wilcox.test(GExternal_Burn_2$Depersonalization, GInternal_Burn_2$Depersonalization, paired=FALSE)
wilcox.test(GExternal_Burn_1$Depersonalization, GInternal_Burn_1$Depersonalization, paired=FALSE)
wilcox.test(GExternal_Burn_3$Depersonalization, GInternal_Burn_3$Depersonalization, paired=FALSE)

wilcox.test(GExternal_Burn_2$Successfulness, GInternal_Burn_2$Successfulness, paired=FALSE)
wilcox.test(GExternal_Burn_1$Successfulness, GInternal_Burn_1$Successfulness, paired=FALSE)
wilcox.test(GExternal_Burn_3$Successfulness, GInternal_Burn_3$Successfulness, paired=FALSE)


#multiple regression: Depersonalization ~ Burnout + LOC

dep.mult.reg <- data.frame (
  Depersonalization=dataset$Depersonalization, 
  LOC_prof=dataset$LOC_prof, 
  Burnout=dataset$Burnout)
plot(dep.mult.reg)

summary(lm(Depersonalization ~ Burnout+LOC_prof, data=dataset))
summary(lm(Depersonalization ~ Burnout, data=dataset))
summary(lm(Depersonalization ~ LOC_prof, data=dataset))
new_plot <- lm(Depersonalization ~ Burnout, data=dataset)
plot(lm(Depersonalization ~ Burnout, data=dataset))

dataset %>% 
  ggplot(aes(Burnout,Depersonalization, col=LOC_prof_groups)) +
  geom_point() +
  geom_smooth(method = "lm")

#Without middle LOC_prof (taking just ends to detect the difference)
hist(dataset$LOC_prof) #7 is the middle
dataset_2 <- dataset %>% filter (LOC_prof<7 | LOC_prof>7)
xtabs (~ LOC_prof_groups + Burnout_three_groups, data=dataset) #how much we had
xtabs (~ LOC_prof_groups + Burnout_three_groups, data=dataset_2) #how much we have without 7 in LOC_prof

#USING LM compare what has more influence burnout or LOC (when burnout medium)
dataset_3 <- dataset_2 %>% filter (Burnout_three_groups=="Burnout_medium") 
xtabs (~LOC_prof_groups + Depersonalization, data=dataset_3)
xtabs (~LOC_prof_groups + Successfulness, data=dataset_3)
summary(lm(Depersonalization ~ Em_exhaustion+LOC_prof, data=dataset_3))  
summary(lm(Successfulness ~ Em_exhaustion+LOC_prof, data=dataset_3)) 

#regression between Successfulness and Depersonalization (in all data_)
#when Burnout_medium
dataset_4 <- dataset %>% filter (Burnout_three_groups=="Burnout_medium") 
summary(lm(Successfulness ~ Depersonalization, data=dataset_4)) 
#when Burnout_high
dataset_Burn_high <- dataset %>% filter (Burnout_three_groups=="Burnout_high") 
summary(lm(Successfulness ~ Depersonalization, data=dataset_Burn_high)) 
#when Burnout_low
dataset_Burn_low <- dataset %>% filter (Burnout_three_groups=="Burnout_low") 
summary(lm(Successfulness ~ Depersonalization, data=dataset_Burn_low)) 

### GROUPS OF BURNOUT AND LOC

#creating variable with three levels of Burnout
dataset$Burnout_three_groups <- factor (dataset$Burnout_three_groups,
                                        levels=c(1,2,3),
                                        labels=c("Burnout_low", "Burnout_medium", "Burnout_high"))

#creating variable with External and Internal general LOC
dataset$LOC_gen_groups <- factor (dataset$LOC_gen_groups,
                                  levels=c(1,2),
                                  labels=c("External", "Internal"))

#creating variable with External and Internal LOC in professional domain
dataset <- mutate (dataset, LOC_prof_groups = ifelse(dataset$LOC_prof<7, 1, 2))
dataset$LOC_prof_groups <- factor (dataset$LOC_prof_groups,
                                   levels=c(1,2),
                                   labels=c("External", "Internal"))

#calculate the number of participans in each LOC_prof/Burnout group
#xtabs (~ LOC_prof_groups + Burnout_three_groups, data=dataset)
xtabs (~ LOC_gen_groups + Burnout_three_groups, data=dataset)


### SEMANTIC DIFFERENTIAL

#making SD varidables more readable
names(dataset)[names(dataset) == 'SD_St_Tyajeliy'] <- 'Student_Heavy'
#ADD_THE_REST!

dataset_with_sd_MB <- dataset_with_sd %>% 
  filter (Burnout_three_groups=='Burnout_medium')

dataset_with_sd_MB_Ex <- dataset_with_sd_MB %>% 
  filter (LOC_gen_groups=="External")

dataset_with_sd_MB_In <- dataset_with_sd_MB %>% 
  filter (LOC_gen_groups=="Internal")

SD_col <- colnames(dataset_with_sd_MB) [colnames(dataset_with_sd_MB) %>% startsWith("SD_")]
dataset_with_sd_MB_piv <- pivot_longer(dataset_with_sd_MB, SD_col, names_to = c("Object","Scale"), 
             names_prefix = "SD_", names_sep = "_", values_to = "SD_value", values_drop_na =TRUE)

dataset_with_sd_MB_piv %>% 
  ggplot(aes(SD_value, fill=Object)) + 
  geom_density(alpha = 0.8, position = "identity") + 
  facet_grid(Scale~LOC_gen_groups, shrink=TRUE)

dataset_with_sd_MB_piv %>% 
  ggplot(aes(SD_value, fill=Object)) + 
  geom_histogram(alpha = 0.8, position = "identity") + 
  facet_grid(Scale~LOC_gen_groups, shrink=TRUE)


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


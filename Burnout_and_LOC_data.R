rm(list = ls())

# This is code to replicate the analyses and figures from my 2020 Burnout and Locus of Control
# paper. Code developed by Alena Egorova (@alvegorova)

## DOWNLOADING LIBRARIES

install.packages(c("psych", "ggplot2"))
require(psych)
require(ggplot2)
require(dplyr)
require(foreign) #to use "read.spss"

## DOWNLOADING DATA

setwd("~/Documents/Data_Analysis/Burnout_and_LOC")
dataset = read.spss("Data.sav", to.data.frame=TRUE)

## PREPARING DATA

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
#choosing important variables
dataset <- dataset %>% 
  select (N, Age, Gender, Experience_gen, Experience_last, 
          LOC_gen, LOC_gen_groups, 
          LOC_prof, LOC_prof_groups,
          Euthymia, Dysthymia, Depression, Anxiety,
          Em_exhaustion, Depersonalization, Successfulness, 
          Burnout, Burnout_two_groups, Burnout_three_groups)

as.integer(N, LOC_gen, LOC_prof,
           Euthymia, Dysthymia, Depression, Anxiety,
           Em_exhaustion, Depersonalization, Successfulness, 
           Burnout)

#to overview the data
str(dataset)
View(dataset)

## DESCRIPTIVE STATISTICS

#general description of the participants
lapply(dataset[, c("Age", "Gender", "Experience_gen", "LOC_gen", "LOC_prof", "Burnout")], table)

#calculate the number of participans in each LOC_prof/Burnout group
xtabs (~ LOC_prof_groups + Burnout_three_groups, data=dataset)

#Depersonalization in different LOC_prof level vs. Burnout level
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Depersonalization)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Depersonalization, fill = LOC_prof_groups)) + 
  geom_density(alpha = 0.4) + facet_grid (Burnout_three_groups~.)

#Successfulness in different LOC_prof level vs. Burnout level
dataset %>% filter (!is.na(LOC_prof_groups) & (!is.na(Successfulness)) & 
                      (!is.na(Burnout_three_groups))) %>% 
  ggplot(aes(Successfulness, fill = LOC_prof_groups)) + 
  geom_density(alpha = 0.4) + facet_grid (Burnout_three_groups~.)

#наверно это будет не нужно
#calculate mean Depers in dif LOC
aggregate(dataset$Depersonalization, by = list(dataset$LOC_prof), FUN=mean)

#наверно это будет не нужно
#means in Dep in dif LOC vs dif Burnout
table (dataset$LOC_prof, dataset$Burnout_three_groups)
as.data.frame (table (dataset$LOC_prof, dataset$Burnout_three_groups))
aggregate(dataset$Depersonalization, 
          by = list(dataset$LOC_prof, dataset$Burnout_three_groups), FUN=mean) 

table (dataset$LOC_prof, dataset$Depersonalization, dataset$Burnout_three_groups)
mosaicplot (table 
            (dataset$LOC_prof, dataset$Burnout_three_groups))

#Count missing data
colSums(is.na(dataset)) 


## Plots with Depers in dif LOC vs dif Burnout

ggplot(dataset, 
       aes(x=LOC_prof_groups, y=Depersonalization, col=Burnout_three_groups))+
  geom_boxplot()

ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Depersonalization, col=LOC_prof_groups))+
  geom_boxplot()

ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Depersonalization, col=LOC_prof_groups))+
  geom_point()

ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Depersonalization, col=LOC_gen_groups))+
  geom_boxplot()

ggplot(dataset, 
       aes(x=Burnout, col=LOC_prof_groups))+ 
  geom_density()+ 
  scale_colour_brewer(palette='Set1')

ggplot(dataset, 
       aes(x=Burnout_three_groups, y=Successfulness, col=LOC_prof_groups))+
  geom_boxplot()

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

#Groups
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

#or this way
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



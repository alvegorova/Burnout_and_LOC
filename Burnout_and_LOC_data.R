rm(list = ls())

# This is code to replicate the analyses and figures from my 2020 Burnout and Locus of Control
# paper. Code developed by Alena Egorova (@alvegorova)

## DOWNLOADING LIBRARIES

install.packages(c("psych", "ggplot2"))
require(psych)
require(ggplot2)
require(foreign) #to use "read.spss"

## DOWNLOADING DATA

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
names(dataset)[names(dataset) == 'USK_Ip'] <- 'LOC_prof'


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

## DESCRIPTIVE STATISTICS

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

#сколько пропущенных значений
colSums(is.na(dataset)) 
getwd()
## Pots with Depers in dif LOC vs dif Burnout

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

#Группы
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

#нормальность распределения
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

#Тест Мана-Уитни
wilcox.test(External_Burn_2$Depersonalization, Internal_Burn_2$Depersonalization, paired=FALSE)
wilcox.test(External_Burn_1$Depersonalization, Internal_Burn_1$Depersonalization, paired=FALSE)
wilcox.test(External_Burn_3$Depersonalization, Internal_Burn_3$Depersonalization, paired=FALSE)

wilcox.test(External_Burn_2$Successfulness, Internal_Burn_2$Successfulness, paired=FALSE)
wilcox.test(External_Burn_1$Successfulness, Internal_Burn_1$Successfulness, paired=FALSE)
wilcox.test(External_Burn_3$Successfulness, Internal_Burn_3$Successfulness, paired=FALSE)



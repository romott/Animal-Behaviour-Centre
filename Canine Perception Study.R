## CANINE PERCEPTION STUDY

# load packages

library(nlme)
library(dplyr)
library(ggplot2)
library(car)
library(patchwork)
library(psych)
library(esquisse)
library(pwr)
library(ggfortify)

# clear memory

rm(list = ls())

# read data file

#Synthetic Data
SynthD <- read.csv("C:/Users/2428785M/OneDrive - University of Glasgow/Richard Mott/Music and Voice Study/Synthetic Data2.csv")
View(SynthD)

# Set as Factor/Numeric
SynthD$Dog<-factor(SynthD$Dog)
SynthD$Trial.No.<-factor(SynthD$Trial.No.)
SynthD$Correct<-factor(SynthD$Correct)
SynthD$Tempo<-factor(SynthD$Tempo)
SynthD$Orientation<-factor(SynthD$Orientation)
SynthD$Beat<-factor(SynthD$Beat)

SynthD$RT..s.<- as.numeric(SynthD$RT..s.)

# ggplot2 tool

esquisser(SynthD)

# t test

t.test(Selection~Correct, paired=T, data=SynthD)


# LME

fit <- lm(Selection~Correct, data = SynthD)

fit <- lme(Selection~Correct, data = SynthD, random = ~1|Dog)

fit <- lme(Beat.Correct~Beat, data = SynthD, random = ~1|Dog)
summary(fit)
anova(fit)

# Check assumptions

autoplot(fit)
hist(rstandard(fit), breaks=20)

# Means
means<-aggregate(Selection~Correct, SynthD, mean)
View(means)

#Logistic Regression
mylogit<-glm(formula = Correct ~ Tempo + Orientation, family = "binomial", data = SynthD)
summary(mylogit)

# Power / Effect Size
# https://www.statmethods.net/stats/power.html
# n=sample size
# r=effect size (correlation)
# power = % probability of detecting effect size in sample size
# Given 3, will calculate 4th.

# pwr.r.test(n=15, r=0.9, sig.level=0.05, power = 0.80) - correlation

pwr.r.test(n=14, r=0.7, sig.level=0.05)
pwr.r.test(r=0.7, sig.level=0.05, power = 0.80)

# pwr.f2.test - GLM (f2 0.02=small, 0.15=med, 0.35=large)
# u=numorator df
# v=denominator df
# f2=effect size
pwr.f2.test(u =, v = , f2 = , sig.level = , power = )
pwr.f2.test(u =1, v =18 , sig.level =0.05 , power =0.8)
pwr.f2.test(u =1, f2 =0.35 , sig.level =0.05 , power =0.8 )
pwr.f2.test(u =1, v =18 , f2 =0.15 , sig.level =0.05)
# pwr.t.test - paired t test
pwr.t.test(n = , d = , sig.level = , power = , type = c("two.sample", "one.sample", "paired"))

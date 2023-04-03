library("languageserver")
library("httpgd")
library(MASS)
library(ggplot2)
library(VGAM)

setwd("C:/Users/olivi/OneDrive - Universite de Montreal/UdeM/STT2400-H23_Regression Lineaire/Projet/")

df <- read.csv("maquereau.csv")
df <- df[,-1]
attach(df)
View(df)
View(cor(df[, -c(2, 3, 4, 5, 8, 10, 11)]))
plot(df)

lm <- lm(egg.dens ~ b.depth
    + lat
    + lon
    + time
    + flow
    + s.depth
    + temp.surf
    + temp.20m
    + net.area
    + c.dist)
summary(lm)
plot(resid(lm) ~ net.area)
qqnorm(resid(lm))
qqline(resid(lm))
plot(resid(lm) ~ seq(1,length(resid(lm)),1))
View(resid(lm))
boxplot(egg.dens)

a <- data.frame(lon,lat)
p <- ggplot(a, aes(lon, lat))
p <- p + stat_bin2d(bins = 20)
p
boxplot(df[, c(7,9)])


lm2 <- lm(egg.dens
    ~ flow
    + s.depth
    + temp.20m)
summary(lm2)

boxcox(lm2, seq(0, 0.5, 0.01))

plot(resid(lm2) ~ predict(lm2))
qqnorm(resid(lm2))
qqline(resid(lm2))


lm3 <- lm((sqrt(egg.dens) - 0.5)/0.5
    ~ flow
    + s.depth
    + temp.20m)
summary(lm3)

plot(resid(lm3) ~ predict(lm3))
abline(h = 0)

y = egg.dens[-220]
x = seq(0,max(y),1)
theta = mean(y * log(y)) - mean(y)*mean(log(y))
k = mean(y)/theta 

hist(y, breaks = 20, probability = TRUE)
lines(x, dexp(x, rate = 1/mean(y)))
lines(x, dgamma(x, shape = k, scale = theta), col = "red")
lines(x, dpareto(x, scale = min(y), shape = length(y)/(sum(log(y/min(y))))), col = "blue")

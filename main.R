library(ggplot2)
library(cowplot)
library(MASS)

spotify_data <- spotify.2023_finalni_uprava
summary(spotify.2023_finalni_uprava)
spotify_data_1 <- spotify_data[-555, ]

streams <- spotify.2023_finalni_uprava$streams
in_spotify_playlists <- spotify.2023_finalni_uprava$in_spotify_playlists
in_deezer_playlists <- spotify.2023_finalni_uprava$in_deezer_playlists

model1 <- lm(streams  ~ in_spotify_playlists,  data = spotify.2023_finalni_uprava)
summary(model1)

model_log <- lm(log(streams) ~ in_spotify_playlists)

summary(model_log)

which(log(streams)<15)

hist(log(streams))

hist(rstandard(model1), probability = T,
     breaks = ceiling(2*length(rstandard(model1))^(1/3)),
     col = 'skyblue3')
xx <- seq(min(rstandard(model1)), max(rstandard(model1)),
          length = 501)

plot(model1,  which = 1:6)

shapiro.test(rstandard(model1))
shapiro.test(sample(rstandard(model1), size=600))

hist(rstandard(model1), probability = T,
     breaks = ceiling(2*length(rstandard(model1))^(1/3)),
     col = 'skyblue3')
xx <- seq(min(rstandard(model1)), max(rstandard(model1)),
          length = 501)



model2 <- lm(streams  ~ in_spotify_playlists + in_deezer_playlists)
summary(model2)
shapiro.test(sample(rstandard(model2), size=50))
plot(model2, which = 1:6)

hist(rstandard(model2), probability = T,
     breaks = ceiling(2*length(rstandard(model2))^(1/3)),
     col = 'skyblue3')
xx <- seq(min(rstandard(model2)), max(rstandard(model2)),
          length = 501)

model3 <- model2 <- lm(streams  ~ in_spotify_playlists * in_deezer_playlists )
summary(model3)
shapiro.test(sample(rstandard(model3), size=))
plot(model3)




model6 <- lm(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data)
summary(model6) #0.68
AIC(model6)
BIC(model6)
plot(model6)

anova(model6)

mod_anova_6 <- aov(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data)
summary(mod_anova_6)

plot(model6, which = 1:6)

plot(studres(model6)~c(1:829),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 829), df=821), col="red")
abline(h = -qt(1-0.05/(2 * 829), df=821), col="red")

indices <- c(527, 528, 42, 721, 555, 136)

abline(v=indices, lty=2)
points(indices, studres(model6)[indices], pch = 16, col = 'darkblue')

# VYHOZENÍ HODNOT

spotify_data_bez_hodnot_model6 <- spotify_data[c(-42, -527, -528, -721), ]

model6_1 <- lm(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data_bez_hodnot_model6)
summary(model6_1) #0.68
AIC(model6_1)
BIC(model6_1)
plot(model6_1)

anova(model6_1)

mod_anova_6_1 <- aov(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data_bez_hodnot_model6)
summary(mod_anova_6_1)

plot(model6_1, which = 1:6)

plot(studres(model6_1)~c(1:825),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 825), df=817), col="red")
abline(h = -qt(1-0.05/(2 * 825), df=817), col="red")







model8 <- lm(streams ~ in_spotify_playlists*in_spotify_charts + in_apple_playlists* in_apple_charts + in_deezer_charts *in_deezer_playlists + in_spotify_playlists*in_apple_playlists + in_apple_playlists* in_deezer_playlists + in_spotify_playlists *in_deezer_playlists + in_spotify_playlists* in_deezer_playlists*in_apple_playlists,  data = spotify_data)
summary(model8) #0.69#
anova(model8)

plot(studres(model8)~c(1:829),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 829), df=815), col="red")
abline(h = -qt(1-0.05/(2 * 829), df=815), col="red")

indices <- c(527, 528, 42, 721, 555, 136)

abline(v=indices, lty=2)
points(indices, studres(model6)[indices], pch = 16, col = 'darkblue')







model9 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_apple_playlists* in_apple_charts + in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model9) # 0.6922
anova(model9)
AIC(model9) # 34100
BIC(model9) # 34161


plot(model9, which = 1:6)
plot(studres(model9)~c(1:829),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 829), df=817), col="red")
abline(h = -qt(1-0.05/(2 * 829), df=817), col="red")

indices <- c(527, 528, 42, 37, 136, 721)

abline(v=indices, lty=2)
points(indices, studres(model9)[indices], pch = 16, col = 'darkblue')

# vyhození hodnot
spotify_data_bez_hodnot_model9 <- spotify_data[c(-527, -42, -528, -721),]
model9_1 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_apple_playlists* in_apple_charts + in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data_bez_hodnot_model9)
summary(model9_1) # 0.7233
anova(model9_1)
AIC(model9_1) # 33806.12
BIC(model9_1) # 33867.42

plot(model9_1, which = 1:6)
plot(studres(model9_1)~c(1:825),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 825), df=813), col="red")
abline(h = -qt(1-0.05/(2 * 825), df=813), col="red")
indices <- c(125, 107, 145, 38, 37)

abline(v=indices, lty=2)
points(indices, studres(model9_1)[indices], pch = 16, col = 'darkblue')

# vyhození hodnot
spotify_data_bez_hodnot2_model9 <- spotify_data_bez_hodnot_model9[c(-125, -145),]
model9_2 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_apple_playlists* in_apple_charts + in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data_bez_hodnot2_model9)
summary(model9_2) # 0.7324
anova(model9_2)
AIC(model9_2) # 33687.87
BIC(model9_2) # 33749.14

plot(model9_2, which = 1:6)







model11 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model11) # 0.6922
anova(model11)
AIC(model11) # 34097
BIC(model11) # 34149.11

plot(model11, which = 1:6)
plot(studres(model11)~c(1:829),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 829), df=819), col="red")
abline(h = -qt(1-0.05/(2 * 829), df=819), col="red")

indices <- c(527, 528, 42, 37, 136, 721)

abline(v=indices, lty=2)
points(indices, studres(model11)[indices], pch = 16, col = 'darkblue')


# vyhození hodnot
spotify_data_bez_hodnot_model11 <- spotify_data[c(-527, -42, -528, -721),]
model11_1 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data_bez_hodnot_model11)
summary(model11_1) # 0.7221
anova(model11_1)
AIC(model11_1) # 33807.87
BIC(model11_1) # 33859.74




















################################################### LUCČIN FINÁLNÍ KÓD ##############################################################




#### Modely do projektu ####

model3a <- lm(streams ~ in_spotify_charts * in_apple_charts + in_apple_charts* in_deezer_charts + in_spotify_charts *in_deezer_charts + in_spotify_charts* in_deezer_charts * in_apple_charts, data = spotify_data )
summary(model3a) # 0.1325
AIC(model3a) #34954.95
BIC(model3a) #34997.43

model3b <- lm(streams ~ in_spotify_playlists*in_spotify_charts + in_apple_playlists* in_apple_charts + in_deezer_charts *in_deezer_playlists, data = spotify_data)
summary(model3b) #0.6513
AIC(model3b) # 34201.31
BIC(model3b) # 34253.23

model6 <- lm(streams ~ in_spotify_playlists*in_apple_playlists + in_apple_playlists* in_deezer_playlists + in_spotify_playlists *in_deezer_playlists + in_spotify_playlists* in_deezer_playlists*in_apple_playlists, data = spotify_data )
summary(model6) #0.6776
AIC(model6) #34134.38
BIC(model6) #34176.86
shapiro.test(sample(rstandard(model6), size=50)) #0.001118

shapiro.test(sample(rstandard(model9), size=50))
shapiro.test(sample(rstandard(model9_1), size=50))
shapiro.test(sample(rstandard(model9_2), size=50))

model8 <- lm(streams ~ in_spotify_playlists*in_spotify_charts + in_apple_playlists* in_apple_charts + in_deezer_charts *in_deezer_playlists + in_spotify_playlists*in_apple_playlists + in_apple_playlists* in_deezer_playlists + in_spotify_playlists *in_deezer_playlists + in_spotify_playlists* in_deezer_playlists*in_apple_playlists,  data = spotify_data )
summary(model8) #0.6921
anova(model8)
AIC(model8) #34102.16
BIC(model8) #34172.96
shapiro.test(sample(rstandard(model8), size=50)) #0.001466



model9 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_apple_playlists* in_apple_charts + in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model9)#0.6922
anova(model9)
AIC(model9)#34100.04
BIC(model9)#34161.4
shapiro.test(sample(rstandard(model9), size=50)) #0.0006929


model11 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model11)#0.6922
anova(model11)
AIC(model11)#34097.86
BIC(model11)#34149.11
shapiro.test(sample(rstandard(model11), size=50)) #3.079e-05

#### Diagnostika modelu 9 ####
plot(model9)

#### Upravené modely cez diagnostiky ####

#### Predikčné a confidenčné intervaly modeluxx ####


#### Nejaká hypotéza ####




# Model 3A
model3a <- lm(streams ~ in_spotify_charts * in_apple_charts + in_apple_charts* in_deezer_charts + in_spotify_charts *in_deezer_charts + in_spotify_charts* in_deezer_charts * in_apple_charts, data = spotify_data )
summary(model3a) # 0.1325
AIC(model3a) #34954.95
BIC(model3a) #34997.43

# Model 3B
model3b <- lm(streams ~ in_spotify_playlists*in_spotify_charts + in_apple_playlists* in_apple_charts + in_deezer_charts *in_deezer_playlists, data = spotify_data)
summary(model3b) #0.6513
AIC(model3b) # 34201.31
BIC(model3b) # 34253.23

# Model 6
model6 <- lm(streams ~ in_spotify_playlists*in_apple_playlists + in_apple_playlists* in_deezer_playlists + in_spotify_playlists *in_deezer_playlists + in_spotify_playlists* in_deezer_playlists*in_apple_playlists, data = spotify_data )
summary(model6) # 0.6776
AIC(model6) # 34134.38
BIC(model6) # 34176.86

# Model 8
model8 <- lm(streams ~ in_spotify_playlists*in_spotify_charts + in_apple_playlists* in_apple_charts + in_deezer_charts *in_deezer_playlists + in_spotify_playlists*in_apple_playlists + in_apple_playlists* in_deezer_playlists + in_spotify_playlists *in_deezer_playlists + in_spotify_playlists* in_deezer_playlists*in_apple_playlists, data = spotify_data)
summary(model8) # 0.6921
anova(model8)
AIC(model8) # 34102.16
BIC(model8) # 34172.96

# Model 9
model9 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_apple_playlists* in_apple_charts + in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model9)# 0.6922
anova(model9)
AIC(model9) # 34100.04
BIC(model9) # 34161.4

# Model 11
model11 <- lm(streams ~ in_spotify_playlists* in_deezer_playlists*in_apple_playlists + in_spotify_playlists *in_deezer_playlists+ in_spotify_playlists*in_spotify_charts + in_apple_playlists +in_spotify_playlists+in_spotify_charts,  data = spotify_data)
summary(model11) # 0.6922
anova(model11)
AIC(model11) # 34097.86
BIC(model11) # 34149.11

data <- data.frame(matrix(
  c(0.1325, 0.6513, 0.6776, 0.6921, 0.6922, 0.6922, AIC(model3a), AIC(model3b), AIC(model6), AIC(model8),
             AIC(model9), AIC(model11), BIC(model3a), BIC(model3b), BIC(model6), BIC(model8), BIC(model9), BIC(model11)),
  ncol = 6,
  nrow = 3,
  byrow = TRUE
)
)
rownames(data) <- c("Adjusted R squared", "AIC", "BIC")
colnames(data) <- c("Model 3A", "Model 3B", "Model 6", "Model 8", "Model 9", "Model 11")




########## 95% prediction intervals for the fitted regression ############


model1 <- lm(formula = streams ~ in_spotify_playlists, data = spotify_2023_finalni_uprava)

plot(spotify_2023_finalni_uprava$streams ~ spotify_2023_finalni_uprava$in_spotify_playlists)
xx <- seq(min(spotify_2023_finalni_uprava$in_spotify_playlists), max(spotify_2023_finalni_uprava$in_spotify_playlists), length=501)
novadata1 <- data.frame(in_spotify_playlists = xx)
yy1 <- predict(model1, newdata = novadata1, interval = "prediction")

lines(xx, yy1[, 1], col="navyblue", lwd=2)
lines(xx, yy1[, 2], col="navyblue", lwd=2, lty=3)
lines(xx, yy1[, 3], col="navyblue", lwd=2, lty=3)


###model2
model2 <- lm(formula = streams ~ in_apple_playlists, data = spotify_2023_finalni_uprava)

plot(spotify_2023_finalni_uprava$streams ~ spotify_2023_finalni_uprava$in_apple_playlists)
xx2 <- seq(min(spotify_2023_finalni_uprava$in_apple_playlists), max(spotify_2023_finalni_uprava$in_apple_playlists), length=501)
novadata2 <- data.frame(in_apple_playlists = xx2)
yy2 <- predict(model2, newdata = novadata2, interval = "prediction")

lines(xx2, yy2[, 1], col="navyblue", lwd=2)
lines(xx2, yy2[, 2], col="navyblue", lwd=2, lty=3)
lines(xx2, yy2[, 3], col="navyblue", lwd=2, lty=3)

##model3 nehezkÃ½
model3 <- lm(formula = streams ~ in_deezer_playlists, data = spotify_2023_finalni_uprava)

plot(spotify_2023_finalni_uprava$streams ~ spotify_2023_finalni_uprava$in_deezer_playlists)
xx3 <- seq(min(spotify_2023_finalni_uprava$in_deezer_playlists), max(spotify_2023_finalni_uprava$in_deezer_playlists), length=501)
novadata3 <- data.frame(in_deezer_playlists = xx2)
yy3 <- predict(model3, newdata = novadata3, interval = "prediction")

lines(xx3, yy3[, 1], col="navyblue", lwd=2)
lines(xx3, yy3[, 2], col="navyblue", lwd=2, lty=3)
lines(xx3, yy3[, 3], col="navyblue", lwd=2, lty=3)
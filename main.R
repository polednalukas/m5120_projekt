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
summary(model11) # 0.6956
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


library(ggplot2)
library(cowplot)
library(MASS)

c <- 12


model6 <- lm(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data)
summary(model6) #0.68
AIC(model6)
BIC(model6)
plot(model6)

anova(model6)

mod_anova_6 <- aov(streams ~ in_spotify_playlists * in_apple_playlists + in_apple_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists + in_spotify_playlists * in_deezer_playlists * in_apple_playlists, data = spotify_data)
summary(mod_anova_6)

TukeyHSD(mod_anova_6)

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

TukeyHSD(mod_anova_6_1)

plot(model6_1, which = 1:6)

plot(studres(model6_1)~c(1:825),
     ylab="Jackknife residuals", xlab="Index", main="Jackknife residuals")
abline(h=0, col="gray", lwd=2)
abline(h = qt(1-0.05/(2 * 825), df=817), col="red")
abline(h = -qt(1-0.05/(2 * 825), df=817), col="red")








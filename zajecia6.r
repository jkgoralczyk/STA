# ZAD 1 -------------------------------------------------------------------

epois(Centrala$Liczba, method = "mle", ci = TRUE, ci.type = "two-sided", ci.method = "exact", conf.level = 0.95)$interval$limits

epois(Centrala$Liczba, method = "mme", ci = TRUE, ci.type = "two-sided", ci.method = "exact", conf.level = 0.95)$interval$limits

epois(Centrala$Liczba, method = "mvue", ci = TRUE, ci.type = "two-sided", ci.method = "exact", conf.level = 0.95)$interval$limits

epois(Centrala$Liczba, ci = TRUE, ci.method = "pearson",conf.level = 0.9)$interval$limits

epois(Centrala$Liczba, ci = TRUE, ci.method = "normal.approx",conf.level = 0.9)$interval$limits


# ZAD 2 -------------------------------------------------------------------
library(fitdistrplus)

hist(awarie$V1, 
     xlab = "awarie", 
     main = "Rozk³ad empiryczny awarie",
     probability = TRUE, 
     col = "lightgreen")
lines(density(awarie$V1), col = "red", lwd = 2)

descdist(awarie$V1, discrete = FALSE)


# ZAD 3 -------------------------------------------------------------------





# ZAD 1 -------------------------------------------------------------------
library(EnvStats)

mle <- EnvStats::eunif(czas_oczek_tramwaj, method = "mle")
mme <- EnvStats::eunif(czas_oczek_tramwaj, method = "mme")

hist(czas_oczek_tramwaj, 
     xlab = "Czas oczekiwania na tramwaj", 
     main = "Rozk³ad empiryczny i teoretyczny czasu oczekiwania na tramwaj",
     probability = TRUE, 
     col = "white")
lines(density(czas_oczek_tramwaj), col = "red", lwd = 2)
curve(dunif(x, mle$parameters[1], mle$parameters[2]), 
      add = TRUE, col = "blue", lwd = 2)
curve(dunif(x, mme$parameters[1], mme$parameters[2]), 
      add = TRUE, col = "green", lwd = 2)
legend(x = 5, y = 0.04, legend = c("empiryczny", "teoretyczny ENW", "teoretyczny EMM"), col = c("red", "blue", "green"), lwd = 2)

# ZAD 2 -------------------------------------------------------------------

barplot(table(Centrala$Liczba),
        xlab = "Liczba po³¹czeñ", ylab = "Prawdopodobieñstwo",
        main= "Rozk³ad empiryczny")

#fitdistcheck
library(fitdistrplus)

descdist(Centrala$Liczba, discrete = TRUE)

dist <- fitdist(Centrala$Liczba, "pois")
plot(dist)

#rest
epios <- EnvStats::epois(Centrala$Liczba, method = "mle")

p_est <- epios$parameters[1]

probs <- dpois(sort(unique(Centrala$Liczba)), epios$parameters[1])

counts <- matrix(c(prop.table(table(Centrala$Liczba)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empiryczny", "teoretyczny")
colnames(counts) <- sort(unique(Centrala$Liczba))
counts

barplot(counts, 
        xlab = "Liczba b³êdów", ylab = "Prawdopodobieñstwo",
        main = "Rozk³ady empiryczny i teoretyczny liczby b³êdów",
        col = c("red", "blue"), legend = rownames(counts), beside = TRUE)

EnvStats::qqPlot(Centrala$Liczba,
                 distribution = "pois",
                 param.list = list(lambda = p_est),
                 add.line = TRUE)

mean(Centrala$Liczba < 4)

ppois(3, p_est)

# ZAD 4 -------------------------------------------------------------------

wiater <- c(0.9, 6.2, 2.1, 4.1, 7.3,1.0,4.6,6.4,3.8,5.0,2.7,9.2,5.9,7.4,3.0,4.9,8.2,5.0,1.2,10.1,12.2,2.8,5.9,8.2,0.5)

hist(wiater, 
     xlab = "wiater", 
     main = "Rozk³ad empiryczny wiateru",
     probability = TRUE, 
     col = "lightgreen")
lines(density(wiater), col = "red", lwd = 2)

#fitdistcheck
descdist(wiater, discrete = TRUE)

plot(fitdist(wiater, "norm"))

mlewiater <- EnvStats::enorm(wiater, method = "mle")

hist(wiater, 
     xlab = "wiater szybki", 
     main = "Rozk³ad empiryczny i teoretyczny wiater",
     probability = TRUE, 
     col = "lightgreen")
lines(density(wiater), col = "red", lwd = 2)
curve(dnorm(x, mlewiater$parameters[1], mlewiater$parameters[2]), 
      add = TRUE, col = "blue", lwd = 2)
legend(x = 5, y = 0.04, legend = c("empiryczny", "teoretyczny"), col = c("red", "blue"), lwd = 2)

EnvStats::qqPlot(wiater,
                 distribution = "norm",
                 param.list = list(mean = mlewiater$parameters[1], sd = mlewiater$parameters[2]), add.line = TRUE)

mean(wiater > 4 & wiater < 8)

((pnorm(7, mlewiater$parameters[1], mlewiater$parameters[2]))+
(1 - pnorm(4, mlewiater$parameters[1], mlewiater$parameters[2])))-1

# ZAD 1 -------------------------------------------------------------------

ankieta <- read.table("ankieta.txt", header = TRUE)

data.frame(cbind(liczebnosc = table(ankieta$wynik),procent = prop.table(table(ankieta$wynik))))

data.frame(cbind(liczebnosc = table(ankieta[ankieta$szkola == 'p', ]$wynik),procent = prop.table(table(ankieta[ankieta$szkola == 'p', ]$wynik))))

barplot(table(ankieta$wynik),
        xlab = "Odpowiedzi", ylab = "Liczebnoœæ",
        main = "Rozk³ad empiryczny zmiennej wynik",
        col=c(1:5))

barplot(prop.table(table(ankieta$wynik)),
        xlab = "Odpowiedzi", ylab = "Prawdopodobieñstwo",
        main = "Rozk³ad empiryczny liczby b³êdów",
        col=c(1:5))

pie(table(ankieta$wynik))

m <- table(ankieta[ankieta$plec == 'm', ]$wynik)
k <- table(ankieta[ankieta$plec == 'k', ]$wynik)
test <- cbind(k,m)
barplot(test,beside=T,col=c(1:5))

barplot(table(ankieta$wynik, ankieta$plec), beside = TRUE, col = 1:5, legend = levels(ankieta$wynik))

# ZAD 2 -------------------------------------------------------------------

load("Centrala.RData")

data.frame(cbind(liczebnosc = table(Centrala),procent = prop.table(table(Centrala))))

barplot(table(Centrala),
        xlab = "Liczba zg³oszeñ", ylab = "Liczebnoœæ",
        main = "Rozk³ad empiryczny liczby zg³oszeñ",
        col=c(1:6))

barplot(prop.table(table(Centrala)),
        xlab = "Liczba zg³oszeñ", ylab = "Prawdopodobieñstwo",
        main = "Rozk³ad empiryczny liczby zg³oszeñ",
        col=c(1:6))

pie(table(Centrala))

mean(Centrala$Liczba)
median(Centrala$Liczba)
sd(Centrala$Liczba)
sd(Centrala$Liczba)/mean(Centrala$Liczba)*100

# ZAD 3 -------------------------------------------------------------------

wiatr <- c(0.9,6.2,2.1,4.1,7.3,1.0,4.6,6.4,3.8,5.0,2.7,9.2,5.9,7.4,3.0,4.9,8.2,5.0,1.2,10.1,12.2,2.8,5.9,8.2,0.5)

(wiatr_break <- hist(wiatr, plot = FALSE)$breaks)

data.frame(cbind(liczebnosc = table(cut(wiatr, breaks = wiatr_break)),
                 procent = prop.table(table(cut(wiatr, breaks = wiatr_break)))))

hist(wiatr, 
     xlab = "Srednia szybkoœæ wiatru", 
     main = "Rozk³ad empiryczny œredniej szybkoœci wiatru")
rug(jitter(wiatr))

hist(wiatr, 
     xlab = "Srednia szybkoœæ wiatru", 
     main = "Rozk³ad empiryczny œredniej szybkoœci wiatru",
     probability = TRUE, 
     col = "lightblue")
lines(density(wiatr), col = "green", lwd = 2)

boxplot(wiatr, 
        ylab = "Srednia szybkoœæ wiatru", 
        main = "Rozk³ad empiryczny œredniej szybkoœci wiatru")

mean(wiatr)
median(wiatr)
sd(wiatr)
sd(wiatr)/mean(wiatr)*100

library(e1071)
skewness(wiatr)
kurtosis(wiatr)

# ZAD 4 -------------------------------------------------------------------

wspolczynnik_zmiennosci <- function(x, narm=FALSE) {
  if (!is.numeric(x)) { 
    stop("argument nie jest liczb¹")
  }
  ss <- sd(x, narm) / mean(x, na.rm = narm) * 100
  return(ss)
}

x <- c(1, NA, 3)

wspolczynnik_zmiennosci(x)

wspolczynnik_zmiennosci(x, narm = TRUE)

wspolczynnik_zmiennosci()

wspolczynnik_zmiennosci(c("x", "y"))
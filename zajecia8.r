# ZAD 1 -------------------------------------------------------------------

dane <- read.table("http://ls.home.amu.edu.pl/data_sets/kontekst.txt")
colnames(dane) <- c("number", "context")

summary(dane)

aggregate(dane$number, list(CONTEXT = dane$context), FUN = mean)

boxplot(number ~ context, data = dane)

summary(aov(number ~ context, data = dane))

# za³o¿enia
shapiro.test(lm(number ~ context, data = dane)$residuals)

bartlett.test(number ~ context, data = dane)

fligner.test(number ~ context, data = dane)

library(car)
leveneTest(number ~ context, data = dane)

leveneTest(number ~ context, data = dane, center = "mean")

# testy post hoc
attach(dane)
pairwise.t.test(number, context, data = dane)

model_aov <- aov(number ~ context, data = dane)
TukeyHSD(model_aov)

plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "context", console = TRUE)

SNK.test(model_aov, "context", console = TRUE)

LSD.test(model_aov, "context", p.adj = "holm", console = TRUE)

scheffe.test(model_aov, "context", console = TRUE)

# analiza kontrastów
model.2 <- aov(number ~ context, data = dane)
summary(model.2, 
        split = list(context = list('C1' = 1, 'C2' = 2, 'C3' = 3, 'C4' = 4)))

# ZAD 2 -------------------------------------------------------------------

Eysenck$Nr <- NULL

summary(Eysenck)

aggregate(Eysenck$Wynik, 
          list(Instrukcja = Eysenck$Instrukcja), 
          FUN = mean)

boxplot(Wynik ~ Instrukcja, data = Eysenck)

summary(aov(Wynik ~ Instrukcja, data = Eysenck))

# za³o¿enia
shapiro.test(lm(Wynik ~ Instrukcja, data = Eysenck)$residuals)

bartlett.test(Wynik ~ Instrukcja, data = Eysenck)

fligner.test(Wynik ~ Instrukcja, data = Eysenck)

library(car)
leveneTest(Wynik ~ Instrukcja, data = Eysenck)

leveneTest(Wynik ~ Instrukcja, data = Eysenck, center = "mean")

# testy post hoc
attach(Eysenck)
pairwise.t.test(Wynik, Instrukcja, data = Eysenck)

model_aov <- aov(Wynik ~ Instrukcja, data = Eysenck)
TukeyHSD(model_aov)

plot(TukeyHSD(model_aov))

library(agricolae)
HSD.test(model_aov, "Instrukcja", console = TRUE)

SNK.test(model_aov, "Instrukcja", console = TRUE)

LSD.test(model_aov, "Instrukcja", p.adj = "holm", console = TRUE)

scheffe.test(model_aov, "Instrukcja", console = TRUE)

# analiza kontrastów
model.2 <- aov(Wynik ~ Instrukcja, data = Eysenck)
summary(model.2, 
        split = list(Instrukcja = list('C1' = 1, 'C2' = 2, 'C3' = 3, 'C4' = 4)))
# ZAD 1 -------------------------------------------------------------------

rok <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002)
przypadki <- c(39.7, 38.2, 34.7, 33.1, 30.1, 28.4, 26.3, 24.7)
gruz <- data.frame(rok = rok, przypadki = przypadki)
head(gruz)

plot(gruz, main = "Wykres rozrzutu", pch = 16)

model <- lm(przypadki ~ rok, data = gruz)
model

plot(gruz, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)

# estymacja parametrów
coef(model)

confint(model)

# podsumowanie modelu 
summary(model)

# wartoœci dopasowane przez model
fitted(model)

# reszty
residuals(model)

# przedzia³y ufnoœci dla predykcji
temp_rok <- data.frame(rok = seq(min(gruz$rok) - 10, 
                                             max(gruz$rok) + 10, 
                                             length = 100))
pred <- predict(model, temp_rok, interval = "prediction")
plot(gruz, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_rok$rok, pred[, 2], lty = 2, col = "red")
lines(temp_rok$rok, pred[, 3], lty = 2, col = "red")

# predykcja przypadków gruŸlicy uk³adu oddechowego w latach 2003-2007
nowy <- data.frame(rok = c(2003:2007))
predict(model, nowy, interval = 'prediction')

temp_rok <- data.frame(rok = seq(min(gruz$rok) - 10, 
                                 max(gruz$rok) + 10, 
                                 length = 100))
pred <- predict(model, temp_rok, interval = "prediction")
plot(gruz, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_rok$rok, pred[, 2], lty = 2, col = "red")
lines(temp_rok$rok, pred[, 3], lty = 2, col = "red")

# ZAD 2 -------------------------------------------------------------------

head(braking)

plot(braking, main = "Wykres rozrzutu", pch = 16)

# Model dla pe³nych danych

# model
model <- lm(distance ~ speed, data = braking)

plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)

# estymacja parametrów
coef(model)

confint(model)

summary(model)

# wartoœci dopasowane przez model
fitted(model)

# reszty
residuals(model)

temp_speed <- data.frame(speed = seq(min(braking$speed) - 10, 
                                             max(braking$speed) + 10, 
                                             length = 100))
pred <- predict(model, temp_speed, interval = "prediction")
plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "red")

nowy <- data.frame(speed = c(30:50))
predict(model, nowy, interval = 'prediction')

# wykres rozrzutu z predykcja idk how to do
pred <- predict(model, temp_speed, interval = "prediction")
plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "red")

# model bez ww
model_bez_ww <- lm(distance ~ speed - 1, data = braking)

plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model_bez_ww, col = "red", lwd = 2)

coef(model_bez_ww)

confint(model_bez_ww)

summary(model_bez_ww)

fitted(model_bez_ww)

residuals(model_bez_ww)

temp_speed <- data.frame(speed = seq(min(braking$speed) - 10, 
                                     max(braking$speed) + 10, 
                                     length = 100))
pred1 <- predict(model_bez_ww, temp_speed, interval = "prediction")
plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
abline(model_bez_ww, col = "green", lwd = 2, lty = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 2], lty = 2, col = "green")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "green")

nowy <- data.frame(speed = c(30:50))
predict(model_bez_ww, nowy, interval = 'prediction')

#wykres rozrzutu z predykcja idk how to do
temp_speed <- data.frame(speed = seq(min(braking$speed) - 10, 
                                     max(braking$speed) + 10, 
                                     length = 100))
pred1 <- predict(model_bez_ww, temp_speed, interval = "prediction")
plot(braking, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "red", lwd = 2)
abline(model_bez_ww, col = "green", lwd = 2, lty = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "red")
lines(temp_speed$speed, pred[, 2], lty = 2, col = "green")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "green")

# Model dla zbioru danych bez obserwacji odstaj¹cej

# del obs
braking2 <- braking
braking2 <- braking[-c(27),]

# model
model <- lm(distance ~ speed, data = braking2)

plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "blue", lwd = 2)

# estymacja parametrów
coef(model)

confint(model)

summary(model)

# wartoœci dopasowane przez model
fitted(model)

# reszty
residuals(model)

temp_speed <- data.frame(speed = seq(min(braking2$speed) - 10, 
                                     max(braking2$speed) + 10, 
                                     length = 100))
pred <- predict(model, temp_speed, interval = "prediction")
plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "blue", lwd = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "blue")

nowy <- data.frame(speed = c(30:50))
predict(model, nowy, interval = 'prediction')

# wykres rozrzutu z predykcja idk how to do
pred <- predict(model, temp_speed, interval = "prediction")
plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "blue", lwd = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "blue")

# model bez ww
model_bez_ww <- lm(distance ~ speed - 1, data = braking2)

plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model_bez_ww, col = "blue", lwd = 2)

coef(model_bez_ww)

confint(model_bez_ww)

summary(model_bez_ww)

fitted(model_bez_ww)

residuals(model_bez_ww)

temp_speed <- data.frame(speed = seq(min(braking2$speed) - 10, 
                                     max(braking2$speed) + 10, 
                                     length = 100))
pred1 <- predict(model_bez_ww, temp_speed, interval = "prediction")
plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "blue", lwd = 2)
abline(model_bez_ww, col = "green", lwd = 2, lty = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 2], lty = 2, col = "green")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "green")

nowy <- data.frame(speed = c(30:50))
predict(model_bez_ww, nowy, interval = 'prediction')

#wykres rozrzutu z predykcja idk how to do
temp_speed <- data.frame(speed = seq(min(braking2$speed) - 10, 
                                     max(braking2$speed) + 10, 
                                     length = 100))
pred1 <- predict(model_bez_ww, temp_speed, interval = "prediction")
plot(braking2, main = "Wykres rozrzutu", pch = 16)
abline(model, col = "blue", lwd = 2)
abline(model_bez_ww, col = "green", lwd = 2, lty = 2)
lines(temp_speed$speed, pred[, 2], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "blue")
lines(temp_speed$speed, pred[, 2], lty = 2, col = "green")
lines(temp_speed$speed, pred[, 3], lty = 2, col = "green")



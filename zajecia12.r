# ZAD 1 -------------------------------------------------------------------

data(mtcars)

plot(mtcars$mpg, mtcars$wt, xlab = "mpg", ylab = "wt", pch = 16)

shapiro.test(mtcars$mpg)

qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")

shapiro.test(mtcars$wt)

qqnorm(mtcars$wt)
qqline(mtcars$wt, col = "red")

# testy
cor.test(mtcars$mpg, mtcars$wt, method = "pearson")

cor.test(mtcars$mpg, mtcars$wt, method = "kendall")

cor.test(mtcars$mpg, mtcars$wt, method = "spearman")

# ZAD 1 -------------------------------------------------------------------

morze <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872) 

shapiro.test(morze)

qqnorm(morze)
qqline(morze)

mean(morze)

t.test(morze, mu = 870, alternative = "less")


# ZAD 2 -------------------------------------------------------------------

a <- c(78.2,78.5,75.6,78.5,78.5,77.4,76.6)
b <- c(76.1,75.2,75.8,77.3,77.3,77.0,74.4,76.2,73.5,77.4)

boxplot(a,b)

shapiro.test(a)

qqnorm(a)
qqline(a)

shapiro.test(b)

qqnorm(b)
qqline(b)

var(a)
var(b)

var.test(a, b, alternative = "less")

mean(a)
mean(b)

t.test(a, b, var.equal = TRUE, alternative = 'greater')

# ZAD 3 -------------------------------------------------------------------

c <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)
d <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)

boxplot(c, d)

shapiro.test(c)
qqnorm(c)
qqline(c)

shapiro.test(d)
qqnorm(d)
qqline(d)

mean(c)
mean(d)

t.test(c, d, alternative = 'less', paired = TRUE)


# ZAD 4 -------------------------------------------------------------------

m <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)
k <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)

boxplot(m, k)

shapiro.test(m)
qqnorm(m)
qqline(m)

shapiro.test(k)
qqnorm(k)
qqline(k)

var(m)
var(k)

var.test(m, k, var.equal = TRUE, alternative = "greater")

mean(m)
mean(k)

t.test(m, k, alternative = 'greater')

# ZAD 5 -------------------------------------------------------------------



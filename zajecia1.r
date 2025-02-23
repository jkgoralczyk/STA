# ZAD 2 --------------------------------------------------------------------

x <- c(rep(TRUE,3),rep(FALSE,4),rep(TRUE,2),rep(FALSE,5))

as.numeric(x)

# ZAD 3 -------------------------------------------------------------------

y <- c(1:20,rep(0,10),seq(2,by=2,len=20))

z <- c(y,rev(y))

# ZAD 4 -------------------------------------------------------------------

letters[c(5, 10, 15, 20, 25)]

# ZAD 5 -------------------------------------------------------------------

a <- c(1:1000)
a[a %% 2 == 0] <-  1/(a[a %% 2 == 0])

# ZAD 6 -------------------------------------------------------------------

b <- c(6,3,4,5,2,3)
d <- b[order(-b)]

# ZAD 7 ------------------------------------------------------------------

hi <- c(-1.876, -1.123, -0.123, 0, 0.123, 1.123, 1.876)
sign(hi)
round(hi, digits = 2)
floor(hi)

# ZAD 8 -------------------------------------------------------------------

start.time <- Sys.time()

uf <- c(1:100000000)
uf2 <- sqrt(uf)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# ZAD 9 -------------------------------------------------------------------

library(schoolmath)
data(primlist)

max(primlist[primlist < 1000])

length(primlist[100 < primlist & primlist < 500])

# ZAD 10 ------------------------------------------------------------------

ab <- c("a","b")
nu <- c(1,2,3)

paste(ab, rep(nu, length(ab)), sep = "")

# ZAD 11 ------------------------------------------------------------------

paste(c(1:30), c("X","Y","Z"), sep = ".")

# ZAD 12 ------------------------------------------------------------------

library(carData)

test <- sample(c("a", "b", "c", "d", "e"), 100, replace = TRUE)

recode(test, "c('a','b')= 1; c('c','d')= 2; c('e')= 3")
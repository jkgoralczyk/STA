# ZAD 1 --------------------------------------------------------------------

moja_lista <- list(c("Jakub","Goralczyk"), pi, x <-function(x)
{x*x}, c(seq(0.02,by=0.02,len=50)))

str(moja_lista)

moja_lista[c(1,3)] <- NULL

elo <- lapply(moja_lista,gamma)

# ZAD 2 -------------------------------------------------------------------

mat <- matrix(c(1,2,1,5,0,2,3,5,1), nrow = 3, ncol = 3)

library(Matrix)

rankMatrix(mat)

det(mat)

solve(mat)

eigen(mat)

rowSums(mat)

rowMeans(mat)

colSums(mat)

colMeans(mat)

matt <- solve(mat)

mat %*% matt

# ZAD 3 -------------------------------------------------------------------

a <- c(1:100)
a <- a*a
a <- a %% 10
a <- table(a)

# ZAD 4 -------------------------------------------------------------------

outer(1:10, 1:10, function(x,y) paste(x, "*", y, "=", x*y))

# ZAD 5 -------------------------------------------------------------------

table1 <- read.table("dane1.csv", header = TRUE, sep = ";", dec = ",")

info1 <- table1[c(seq(2, by=2, len=50)),]

info2 <- subset(table1, Wezly.chlonne == "1" & Wiek > "50")

# ZAD 6 -------------------------------------------------------------------

temp <- data.frame(
  '##' = c('Styczeñ','Luty','Marzec','Kwiecieñ','Maj','Czerwiec','Lipiec','Sierpieñ','Wrzesieñ','PaŸdziernik','Listopad','Grudzieñ'),
  NY_F = c(32,33,41,52,62,72,77,75,68,58,47,35)
)

NY_C <- round(((temp["NY_F"] - 32) * 5/9), digits = 2) 

names(NY_C)[names(NY_C) == "NY_F"] <- ""

colnames(NY_C)

temp$NY_C = NY_C

names(temp)[names(temp) == "NY_C"] <- "NY_Celsiusz"
names(temp)[names(temp) == "NY_F"] <- "NY_Fahrenheit"

temp$NY_Fahrenheit <- NULL

save(temp, file = "NY_temp.RData")

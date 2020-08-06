# ZAD 1 -------------------------------------------------------------------

x <- 1:5

#while
k <- 1
c <- 1
while (k <= length(x)) {
  c <- c * x[k]
  k <- k + 1
}
print(c)

#repeat
i <- 0
b <- 1
repeat {
  i <- i + 1
  b <- b * x[i]
  if (i == length(x)) break
}
print(b)

#for
a <- 1
for (j in x){
  a <- a * j
}
print(a)

# ZAD 2 -------------------------------------------------------------------



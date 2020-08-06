# ZAD 1 -------------------------------------------------------------------

usa <- USArrests
usa$UrbanPop <- NULL

usa_scale <- scale(usa)

pca <- prcomp(usa, scale = TRUE)
pca

summary(pca)

head(pca$x)

pca$rotation

plot(pca)

biplot(pca)

library(ape)
plot(mst(dist(usa_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

# ZAD 2 -------------------------------------------------------------------

library(dplyr)
cars <- select(mtcars, mpg, disp, hp, drat, wt, qsec)

cars_scale <- scale(cars)

pca <- prcomp(cars, scale = TRUE)
pca

summary(pca)

head(pca$x)

pca$rotation

plot(pca)

biplot(pca)

plot(mst(dist(cars_scale)), x1 = pca$x[, 1], x2 = pca$x[, 2])

#noscaling
pca2 <- prcomp(cars)
pca2

summary(pca2)

head(pca2$x)

pca2$rotation

plot(pca2)

biplot(pca2)

plot(mst(dist(cars)), x1 = pca$x[, 1], x2 = pca$x[, 2])


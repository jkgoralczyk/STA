# ZAD 1 -------------------------------------------------------------------

head(wojewodztwa)

dim(wojewodztwa)

scale(wojewodztwa, center = TRUE, scale = TRUE)

(skupienia_1 <- hclust(dist(wojewodztwa, method = 'euclidean'),  method = "average"))

plot(skupienia_1, hang = -1)

plot(skupienia_1, hang = -1)
(podzial_1 <- rect.hclust(skupienia_1, k = 4))

(skupienia_2 <- hclust(dist(wojewodztwa, method = 'manhattan'), method = 'average'))

(skupienia_3 <- hclust(dist(wojewodztwa, method = 'minkowski'), method = 'average'))

par(mfrow = c(1, 3))
plot(skupienia_1, hang = -1)
rect.hclust(skupienia_1, k = 4)
plot(skupienia_2, hang = -1)
rect.hclust(skupienia_2, k = 4)
plot(skupienia_3, hang = -1)
rect.hclust(skupienia_3, k = 4)

(skupienia_4 <- hclust(dist(wojewodztwa, method = 'euclidean'),  method = "single"))

(skupienia_5 <- hclust(dist(wojewodztwa, method = 'euclidean'),  method = "complete"))

(skupienia_6 <- hclust(dist(wojewodztwa, method = 'euclidean'),  method = "average"))

(skupienia_7 <- hclust(dist(wojewodztwa, method = 'euclidean'),  method = "ward.D"))

par(mfrow = c(1, 4))
plot(skupienia_4, hang = -1)
plot(skupienia_5, hang = -1)
plot(skupienia_6, hang = -1)
plot(skupienia_7, hang = -1)

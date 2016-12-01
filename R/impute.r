iris$Species[c(1,5,7,3)] <- NA
iris$Species <- as.character(iris$Species)
head(iris)
iris$Species <- replace(iris[,5],which(is.na(iris[,5])),"Blank")
head(iris)
rm(iris)
require(FD)
require(reshape)
require(vegan)

hydro_to1999 <- read.csv("data/hydro_1975-1999.csv")
hydro_IQQM <- read.csv("data/hydro_IQQM.csv")

# remove Teviot @ Croftby and Burnett at U/S Maroon Dam, as neither have IQQM correlates

hydro_to1999a <- hydro_to1999[-16,]
hydro_to1999a <- hydro_to1999a[-15,]
#hydro_IQQMa <- hydro_IQQM[-17,]
#hydro_IQQMa <- hydro_IQQM[-15,]

hydro_compare <- rbind(hydro_to1999a, hydro_IQQM)

blah <- hydro_compare[order(hydro_compare$gaugeID),] 
blah <- hydro_compare[1:40,]
rownames(blah) <- blah$gaugeName
blah <- blah[,3:36]
blah.dis <- vegdist(blah, method="gower")
#blah.dis <- daisy(blah, metric="manhattan", stand=TRUE)
View(as.matrix(blah.dis))

write.csv(as.matrix(blah.dis), file="gower.csv")


blah <- subset(hydro_compare, gaugeID == "138001A")
blah <- rbind(blah, subset(hydro_compare, gaugeID == "138004AB"))
blah <- rbind(blah, subset(hydro_compare, gaugeID == "145099A"))

blahx <- blah[,3:36]
#blahx <- data.frame(t(blahx))
blahx.gowdis <- vegdist(blahx, method="gower")
blahx.man <- daisy(blahx, metric="manhattan", stand=TRUE)
blahx.gowdis

blahx.dis <- daisy(blahx, metric = "euclidean", stand = TRUE)
blahx.dis

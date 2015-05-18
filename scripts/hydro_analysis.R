#require(FD)
#require(reshape)
#require(vegan)

hydro_to1999 <- read.csv("data/hydro_1975-1999.csv")
hydro_IQQM <- read.csv("data/hydro_IQQM.csv")

# remove Teviot @ Croftby and Burnett at U/S Maroon Dam, as neither have IQQM correlates

hydro_to1999a <- hydro_to1999[-16,]
hydro_to1999a <- hydro_to1999a[-15,]
#hydro_IQQMa <- hydro_IQQM[-17,]
#hydro_IQQMa <- hydro_IQQM[-15,]

hydro_to1999a <- hydro_to1999a[order(hydro_to1999a$gaugeID),]
hydro_IQQM <- hydro_IQQM[order(hydro_IQQM$gaugeID),]


hydro_compare <- rbind(hydro_to1999a, hydro_IQQM)
hydro_compare <- hydro_compare

#hydro_compare <- hydro_compare[order(hydro_compare$gaugeID),] 
#hydro_compare <- hydro_compare[1:40,]
rownames(hydro_compare) <- hydro_compare$gaugeName
hydro_compare <- hydro_compare[,4:36]
hydro_compare.dis <- vegdist(hydro_compare, method="gower")
#hydro_compare.dis <- daisy(hydro_compare, metric="manhattan", stand=TRUE)
View(as.matrix(hydro_compare.dis))

write.csv(as.matrix(hydro_compare.dis), file="output/gower.csv")




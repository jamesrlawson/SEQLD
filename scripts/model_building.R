## variance partioning analysis ##

require(vegan)
require(FD)
require(MuMIn)
require(nlme)

alldata1$replicate <- sites$replicate
alldata1.naomit <- na.omit(alldata1)
CWM$regulation <- alldata1$regulation
CWM.naomit <- na.omit(CWM[,1:6])

#clim.dist <- gowdis(alldata1.naomit[,2:20])
#soil.dist <- gowdis(alldata1.naomit[,21:32])
#hydro.dist <- gowdis(alldata1.naomit[,33:66])
#hydrochange.dist <- gowdis(alldata1.naomit[,85:116])

#clim.pca <- prcomp(alldata1.naomit[,2:20], center=TRUE, scale=TRUE)
#soil.pca <- prcomp(alldata1.naomit[,21:32], center=TRUE, scale=TRUE)
#hydro.pca <- prcomp(alldata1.naomit[,33:66], center=TRUE, scale=TRUE)
#hydrochange.pca <- prcomp(alldata1.naomit[,85:116], center=TRUE, scale=TRUE)

#summary(clim.pca)
#summary(soil.pca)
#summary(hydro.pca)
#summary(hydrochange.pca)

# get PC's that represent > 0.1 of variance

#clim.pc1 <- clim.pca$x[,1]
#clim.pc2 <- clim.pca$x[,2]

#soil.pc1 <- soil.pca$x[,1]
#soil.pc2 <- soil.pca$x[,2]
#soil.pc3 <- soil.pca$x[,3]

#hydro.pc1 <- hydro.pca$x[,1]
#hydro.pc2 <- hydro.pca$x[,2]
#hydro.pc3 <- hydro.pca$x[,3]


# what about hydrochange?

allPC <- data.frame(cbind(alldata1.naomit["clim.pc1"],
                          alldata1.naomit["clim.pc2"],
                          alldata1.naomit["soil.pc1"],
                          alldata1.naomit["soil.pc2"],
                          alldata1.naomit["soil.pc3"],
                          alldata1.naomit["soil.pc4"],                          
                          alldata1.naomit["hydro.pc1"],
                          alldata1.naomit["hydro.pc2"],
                          alldata1.naomit["hydro.pc3"],
                          alldata1.naomit["hydro.pc4"],
                          alldata1.naomit["regulation"],
                          alldata1.naomit["FDis"],
                          alldata1.naomit["FRic"],
                          alldata1.naomit["FDiv"],
                          alldata1.naomit["FEve"],
                          alldata1.naomit["richness"],
                          alldata1.naomit["exotics"]),
                          alldata1.naomit["replicate"])

plot(allPC)

getAllStats(allPC, allPC$FDis, FD)
getAllStats(allPC, allPC$FRic, FD)
getAllStats(allPC, allPC$FEve, FD)
getAllStats(allPC, allPC$FDiv, FD)

getAllStats(allPC, allPC$exotics, FD)
getAllStats(allPC, allPC$richness, FD)



FDis.lm <- lme(FDis ~ regulation + hydro.pc2 + soil.pc1, random = ~1|replicate, data = allPC)
FDis.dredge <- dredge(FDis.lm)
subset(FDis.dredge, delta < 4)
summary(get.models(FDis.dredge, 2)[[1]])

FDis.varpart <- varpart(allPC$FDis, ~ regulation, ~ hydro.pc2, ~ soil.pc1, data = allPC)
FDis.varpart
plot(FDis.varpart)



exotics.lm <- lme(exotics ~ hydro.pc1 + I(hydro.pc1^2) + hydro.pc2 + I(hydro.pc2^2) + clim.pc1 + soil.pc2 + soil.pc3 + regulation, random = ~1|replicate, data = allPC)
summary(exotics.lm)
exotics.dredge <- dredge(exotics.lm)
subset(exotics.dredge, delta < 4)
summary(get.models(exotics.dredge, 1)[[1]])

exotics.varpart <- varpart(allPC$exotics, ~ hydro.pc1 + I(hydro.pc1^2) + hydro.pc2 + I(hydro.pc2^2), ~ clim.pc1 , ~ regulation + I(regulation^2),  ~ soil.pc2 + soil.pc3, data = allPC)
exotics.varpart
plot(exotics.varpart)

hydro.pc1.quad <- hydro.pc1 + I(hydro.pc1^2)
hydro.regulation.quad <- allPC$regulation + I(allPC$regulation^2)

exotics.lm2 <- lm(exotics ~ hydro.pc1 + I(hydro.pc1^2) + regulation + I(regulation^2), data = allPC) # !!!
exotics.lm2a <- lm(exotics ~ hydro.pc1.quad * hydro.regulation.quad, data = allPC)
AICc(exotics.lm2,exotics.lm2a)


richness.lm <- lme(richness ~ clim.pc1 + soil.pc1 + soil.pc2, random = ~1|replicate, data = allPC)
richness.dredge <- dredge(richness.lm)
subset(richness.dredge, delta < 4)
summary(get.models(richness.dredge, 1)[[1]])
summary(lm(richness ~ soil.pc1 + soil.pc2, allPC))
plot(richness ~ soil.pc1, allPC)
plot(richness ~ soil.pc2, allPC)

richness.varpart <- varpart(allPC$richness, ~clim.pc1, ~ soil.pc1 + soil.pc2, data = allPC)
richness.varpart  
plot(richness.varpart)


  



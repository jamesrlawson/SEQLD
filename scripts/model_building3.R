alldata_reduced <- read.csv("data/alldata_naomit_reduced.csv", header=T)

# FDis #

getAllStats(alldata_reduced, alldata_reduced$FDis, FD)

C_MaxM.x + I(C_MaxM.x^2) 
LSPeak.x + I(LSPeak.x^2)

soil_soc

FDis <- lme(FDis ~ C_MaxM.x + I(C_MaxM.x^2) + LSPeak.x + I(LSPeak.x^2) + soil_soc, random = ~1|replicate, data = alldata_reduced)
FDis.1 <- lm(FDis ~ C_MaxM.x + I(C_MaxM.x^2) + LSPeak.x + I(LSPeak.x^2) + soil_soc, data = alldata_reduced)
FDis.2 <- lme(FDis ~ C_MaxM.x + I(C_MaxM.x^2) + LSPeak.x + I(LSPeak.x^2), random = ~1|replicate, data = alldata_reduced)
FDis.3 <- lm(FDis ~ C_MaxM.x + I(C_MaxM.x^2) + LSPeak.x + I(LSPeak.x^2), data = alldata_reduced)
FDis.4 <- lm(FDis ~ C_MaxM.x + I(C_MaxM.x^2), data = alldata_reduced)
FDis.4a <- lm(log10(FDis) ~ C_MaxM.x + I(C_MaxM.x^2), data = alldata_reduced)

FDis.5 <- lme(FDis ~ C_MaxM.x + I(C_MaxM.x^2), random = ~1|replicate, data = alldata_reduced)
FDis.6 <- lm(FDis ~  C_MaxM.x + I(C_MaxM.x^2) + regulation + I(regulation^2), data = alldata_reduced)

AICc(FDis,FDis.1,FDis.2,FDis.3,FDis.4, FDis.4a,FDis.5, FDis.6)

# exotics #

getAllStats(alldata_reduced, alldata_reduced$exotics, FD)

exotics.x.lme <- lme(exotics ~ CVAnnBFI.x + I(CVAnnBFI.x^2)
                 + CVAnnLSMeanDur.x
                 + C_MinM.x + I(C_MinM.x^2)
                 + LSMeanDur.x + I(LSMeanDur.x^2)
                 + C_MaxM.x
                 + CVMDFDry.x + I(CVMDFDry.x^2)
                 + CVAnnHSMeanDur.x
                 + HSPeak.x + I(HSPeak.x^2), 
                 random = ~1|replicate,
                 data = alldata_reduced)
exotics.x.lm.dredge <- dredge(exotics.x.lme)
summary(model.avg(exotics.x.lme.dredge))

subset(exotics.x.lme.dredge, delta < 4)

exotics.x.lm1 <- lme(exotics ~ CVAnnBFI.x + I(CVAnnBFI.x^2) + C_MinM.x + I(C_MinM.x^2), random = ~1|replicate, data = alldata_reduced)
exotics.x.lm1a <- lm(exotics ~ CVAnnBFI.x + I(CVAnnBFI.x^2) + C_MinM.x + I(C_MinM.x^2), data = alldata_reduced)
AICc(exotics.x.lm1, exotics.x.lm1a)


exotics.x.lm <- lm(exotics ~ CVAnnBFI.x + I(CVAnnBFI.x^2)
                     + CVAnnLSMeanDur.x
                     + C_MinM.x + I(C_MinM.x^2)
                     + LSMeanDur.x + I(LSMeanDur.x^2)
                     + C_MaxM.x
                     + CVMDFDry.x + I(CVMDFDry.x^2)
                     + CVAnnHSMeanDur.x
                     + HSPeak.x + I(HSPeak.x^2), 
                     data = alldata_reduced)

exotics.x.lm.dredge <- dredge(exotics.x.lm)

subset(exotics.x.lm.dredge, delta < 4)

exotics.x.lm.avg <- model.avg(exotics.x.lm.dredge)
summary(exotics.x.lm.avg)


exotics.varpart <- varpart(alldata_reduced$exotics,  
                            ~ CVAnnBFI.x + I(CVAnnBFI.x^2),
                         # ~ C_MinM.x + I(C_MinM.x^2),
                          ~ LSMeanDur.x + I(LSMeanDur.x^2),
                         # ~ C_MaxM.x,
                         # ~ CVMDFDry.x + I(CVMDFDry.x^2),
                         # ~ CVAnnHSMeanDur.x,
                          ~ HSPeak.x + I(HSPeak.x^2), 
                          data = alldata_reduced)
exotics.varpart
plot(exotics.varpart)


exotics.landuse.lm <- lme(exotics ~ production_irrigated_w + I(production_irrigated_w^2)
                 +production_natural_w
                 +conservation_w + I(conservation_w^2)
                 +production_dryland_w, 
                 random = ~1|replicate,
                 data = alldata_reduced)
exotics.landuse.lm.dredge <- dredge(exotics.landuse.lm)


exotics.y.lm <- lme(exotics ~ C_MinM.y + I(C_MinM.y^2), random = ~1|replicate, data = alldata_reduced)
exotics.y.lm.dredge <- dredge(exotics.y.lm)

exotics.soil.lm <- lme(exotics ~ soil_phc + soil_der, , random = ~1|replicate,  data = alldata_reduced)
exotics.soil.lm.dredge <- dredge(exotics.soil.lm)

exotics.clim.lm <- lme(exotics ~ clim_tsea + clim_pdry + I(clim_pdry^2) + clim_pwet + I(clim_pwet^2), random = ~1|replicate, data = alldata_reduced)
exotics.clim.lm.dredge <- dredge(exotics.clim.lm)


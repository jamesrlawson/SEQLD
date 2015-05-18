# CLIMATE #

climate <- read.csv("data/sites_clim_soil.csv", header=T) 

hydrosites2 <- merge(hydro, sites, all.y=TRUE, by = c("gaugeID"))
hydrosites <- hydrosites[order(hydrosites$site),]

hydrosites2 <- hydrosites2[,4:39]

blah <- merge(climate, hydrosites2, by=c("site"))

blah <- blah[,7:72]

blah$site <- siteNums
blah$FDis <- FD$FDis
blah$FDiv <- FD$FDiv
blah$FRic <- FD$FRic
blah$FEve <- FD$FEve
blah$RaoQ <- FD$RaoQ
blah$FGR <- FD$FGR
blah$nbsp <- FD$nbsp
blah$simpson <- FD.redun$Simpson
blah$FunRao <- FD.redun$FunRao
blah$redun <- FD.redun$FunRedundancy
blah$nbsp <- FD$nbsp
blah$richness <- richness$richness
blah$exotics <- exotics.x$proportionExotic


getStats(blah, blah$FDis, FD)
getStats(blah, blah$FDiv, FD)
getStats(blah, blah$FRic, FD)
getStats(blah, blah$FEve, FD)
getStats(blah, blah$RaoQ, FD)
getStats(blah, blah$simpson, FD)
getStats(blah, blah$FunRao, FD)
getStats(blah, blah$redun, FD)
getStats(blah, blah$richness, FD)
getStats(blah, blah$regulation, FD)
getStats(blah, blah$exotics, FD)

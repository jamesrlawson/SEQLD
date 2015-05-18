SPATIAL AUTOCORRELATION TEST ##

require(ade4)
spatial <- read.csv("data/spatial.csv", header=T)

# for hydro metrics #

hydrosites1 <- merge(hydro, sites, all.y=TRUE)[,4:37]

spatial.dist <- dist(spatial[,2:3])
hydro.dist <- dist(hydrosites1[,1:34])
#hydro.signif.dist <- dist(hydro.signif)

mantel.rtest(spatial.dist, hydro.dist, nrepet=9999)
#mantel.rtest(spatial.dist, hydro.signif.dist, nrepet=9999)

# for CWM #

spatial.dist.inv <- as.matrix(1/spatial.dist)
diag(spatial.dist.inv) <- 0

Moran.I(hydrosites$exotics, spatial.dist.inv)
Moran.I(hydrosites$regulation, spatial.dist.inv, na.rm=TRUE)
Moran.I(hydrosites$richness, spatial.dist.inv)


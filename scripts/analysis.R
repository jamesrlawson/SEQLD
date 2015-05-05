source("scripts/functions.R")

alltraits <- read.csv("data/alltraits.csv", header=T)
sites <- read.csv("data/sites.csv", header=T)
vegSurveys <- read.csv("data/vegSurveys.csv", header=T)

alltraits$X <- NULL

# wrangling to convert transect counts -> site avg # per hectare

vegSurveys <- melt(vegSurveys, id.vars = c("site", "transect", "transect.area"))

colnames(vegSurveys)[4] <- c("Taxon")
colnames(vegSurveys)[5] <- c("count")

vegSurveys$Taxon <- as.factor(trim(vegSurveys$Taxon)) # trim white spaces
levels(vegSurveys$Taxon) <- capitalise(levels(vegSurveys$Taxon)) # make sure spp names are properly capitalised

vegSurveys$perHa <- vegSurveys$count * 10000 / vegSurveys$transect.area

vegSurveys <- ddply(vegSurveys, .(site, Taxon), summarise, avgPerHa = mean(perHa))

vegSurveys_all <- vegSurveys

# find total cover in stems/Ha for each site

vegSurveys.totalcover <- ddply(vegSurveys, .(site), summarise, totalcover = sum(avgPerHa, na.rm=TRUE))

vegSurveys <- merge(vegSurveys, vegSurveys.totalcover)

vegSurveys <- merge(vegSurveys, alltraits, all.y=TRUE)
#vegSurveys <- merge(vegSurveys, alltraits)


vegSurveys <- vegSurveys[order(vegSurveys$site),]


# find proportion of cover for which trait data is available

vegSurveys.representedcover  <- merge(ddply(vegSurveys, .(site), summarise, representedcover = sum(avgPerHa, na.rm=TRUE)),
                                      vegSurveys.totalcover)

vegSurveys.representedcover$proportion <- vegSurveys.representedcover$representedcover / vegSurveys.representedcover$totalcover


abun <- cast(vegSurveys, site ~ Taxon, value="avgPerHa", fill=0)


abun <- abun[order(abun$site),]
abun <- abun[-46,] 
abun$site <- NULL
abun <- data.frame(abun)

Taxon <- alltraits$Taxon 
alltraits$Taxon <- NULL
rownames(alltraits) <- Taxon # dbFD requires this format
rm(Taxon)


# calculate FD

blah <- dbFD(alltraits, 
             abun,
             w.abun = TRUE,  # use presence - absence converted data?
             stand.x = FALSE,
             corr = c("cailliez"),
             #                calc.FGR = TRUE, 
             #                clust.type = c("kmeans"),
             #                km.inf.gr = c(2),
             #                km.sup.gr = c(10),
             #                km.iter = (100),
             #                calc.FDiv = TRUE, 
             #                calc.FRic = TRUE,
             m = "max",
             calc.CWM=TRUE, 
             print.pco=TRUE, 
             #                scale.RaoQ=TRUE, 
             #               stand.FRic=TRUE
)

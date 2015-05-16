
withoutExotics <- cbind(hydrosites["FDis"], hydrosites["FRic"], hydrosites["FEve"], hydrosites["FDiv"])

withExotics <- cbind(hydrosites["FDis"], hydrosites["FRic"], hydrosites["FEve"], hydrosites["FDiv"])


plot(withExotics$FDiv,withoutExotics$FDiv)
plot(withExotics$FDis,withoutExotics$FDis)
plot(withExotics$FEve,withoutExotics$FEve)
plot(withExotics$FRic,withoutExotics$FRic)


# FDwithout/FDwith vs percentage exotics - need abundance weighted FD metrics


vegSurveys.ex <- subset(vegSurveys, source == "exotic")

blah <- ddply(vegSurveys.ex, .(site), summarise, proportionExotic = sum(avgPerHa) / totalcover)
blah <- unique(blah)

exotics <- data.frame(cbind(blah["site"],
                            blah["proportionExotic"],
                            "FDis.proportion" = withExotics$FDis / withoutExotics$FDis,
                            "FDiv.proportion" = withExotics$FDiv / withoutExotics$FDiv,
                            "FRic.proportion" = withExotics$FRic / withoutExotics$FRic,
                            "FEve.proportion" = withExotics$FEve / withoutExotics$FEve))


#write.csv(exotics, "output/exotics.csv")

#read.csv("output/exotics.csv", header=T)

#plot(FDis.proportion ~ proportionExotic, data = exotics)
plot(FDis.proportion ~ proportionExotic, data = subset(exotics, FDis.proportion < 3))
plot(FRic.proportion ~ proportionExotic, data = exotics)
plot(FEve.proportion ~ proportionExotic, data = exotics)
plot(FDiv.proportion ~ proportionExotic, data = exotics)


blaj <- lm(FDis.proportion ~ proportionExotic + I(proportionExotic^2), subset(exotics, FDis.proportion < 3))
summary(blaj)

hydrosites$exotics <- exotics$proportionExotic
plot(FDis ~ exotics, data = hydrosites)
plot(FEve ~ exotics, data = hydrosites)
plot(richness ~ exotics, data = hydrosites)
plot(FDiv ~ exotics, data = hydrosites)
plot(FRic ~ exotics, data = hydrosites)

getAllStats(hydrosites, hydrosites$exotics, FD)

plot(exotics ~ MDFMDFDry, data = hydrosites)
plot(redun ~ exotics, data = hydrosites)
plot(redun ~ MDFMDFDry, data = hydrosites)


plot.quad(hydrosites, hydrosites$exotics, FD)




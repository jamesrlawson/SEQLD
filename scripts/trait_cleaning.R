source("scripts/functions.R")

library(plyr)

traits <- read.csv("data/traits/RF_trait_data2.csv", header=T)

####### CLEAN SLA AND LMA DATA #######


  # traits <- subset(traits, source != "AUSTRAITS_dataset_52") # 52 is TRY data. activate this line to remove.

  # trim white spaces in units
  
  traits$SLA.units <- trim(traits$SLA.units)
  traits$LMA.units <- trim(traits$LMA.units)
  
  # convert SLA units 
  
  traits.SLA <- units.SLA(traits)
  
  #hist(subset(traits.SLA, SLA.units == "cm2/g")$SLA) 
  #hist(subset(traits.SLA, SLA.units == "m2/kg")$SLA)  # reference - distribution should centre around 10
  
  # convert LMA units
  
  traits.SLA <- units.LMA(traits.SLA)
  traits.SLA$SLAfromLMA <- as.numeric(traits.SLA$SLAfromLMA)
  
  # combine native SLA with LMA-derived SLA
  
  traits.SLA <- data.frame(cbind(traits.SLA["Taxon"], 
                              traits.SLA["SLA"], 
                              traits.SLA["SLAfromLMA"], 
                              traits.SLA["source"]))
  traits.SLA <- SLA_LMA.combine(traits.SLA)
  
  # remove duplicate entries of SLA
  
  traits.SLA <- na.omit(traits.SLA[!duplicated(traits.SLA[,c("Taxon","SLA")]),][,-3])
  traits.SLA <- traits.SLA[order(traits.SLA$Taxon),]
  
  # summarise intraspecies variation in SLA
  
  traits.SLA.CV <- ddply(traits.SLA, 
                         .(Taxon), 
                         summarise, 
                           CV = CV(SLA),
                           sd = sd(SLA),
                           mean = mean(SLA),
                           count = length(SLA))
  
  # find species with CV of > 0.3 for SLA
  
  dodgy.SLA <- traits.SLA[traits.SLA$Taxon %in% as.character(subset(traits.SLA.CV, CV > 0.3)$Taxon), ]
  dodgy.SLA <- dodgy.SLA[order(dodgy.SLA$Taxon), ]
  
  # output
  
  write.csv(traits.SLA, "output/traits_SLA.csv")
  write.csv(dodgy.SLA, "output/dodgy_SLA.csv")


  # identify SLA records only available in the TRY database (52, see top of section)

  #withTRY.SLA <- traits.SLA # run with this line, including TRY above
  #sansTRY.SLA <- traits.SLA # run with this line, excluding TRY above
  
  #sansTRY.SLA.records <- withTRY[!withTRY.SLA$SLA %in% sansTRY.SLA$SLA,] 
  #sansTRY.SLAspecies <- withTRY[!withTRY.SLA$Taxon %in% sansTRY.SLA$Taxon,] 

  # results are: 17 unique records and 1 unique species (Macadamia tetraphylla)







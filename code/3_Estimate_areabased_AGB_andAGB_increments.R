################################################################################
# Reconstuct area-based aboveground biomass (AGB) and 
# aboveground biomass increments (AGBi)
# bootstrap for uncertainties 
# save .csv files for the period 1980-2015
################################################################################



######################
# Cumulative biomass # ---------------------------------------------------------
######################

# forest thinning and reconstruct of historical stand density ------------------
OBS_historical_densities = data.frame(Year=c(1994, 2015),
                                      StemsHa=c(6132,5921))                          
# rate of change
d = (5921-6132)/(2015-1994) # stems ha-1 yr-1, i.e., ~ 0.17 % change per year
# model lineraly density decrease with  time
mod = lm(StemsHa~Year, data=OBS_historical_densities)
# predict historical stand densities
SD_ts = data.frame(Year=2015:1893, Density = predict(mod, data.frame(Year=2015:1893)))
SD_ts = SD_ts[order(SD_ts$Year),]


# load data from individual trees ----------------------------------------------
load("AGB4eachTree.RData")

# [g C m-2]
CumBiomass = matrix(NA, nrow=length(SD_ts$Year), ncol=100)
rownames(CumBiomass) = SD_ts$Year
for (it in 1:100){ # 100 iterations for the bootstraping

 AGB_reconst=data.frame(Year = SD_ts$Year, Biomass = NA)
 
 for (i in 1:length(SD_ts$Year)){ # change the stand density evry year
  trees = sample(colnames(AbvG_Carbon_Trees), SD_ts$Density[i]/100)
  # forest stand totals [g C m-2]
  dummy = rowSums(AbvG_Carbon_Trees[i,trees], na.rm=T) * 1000 /100
  AGB_reconst[i, "Biomass"] = dummy
 }

 CumBiomass[,it] = AGB_reconst$Biomass # [g C m-2]

}


CumBiomass_overview = data.frame(
Year = SD_ts$Year,
Bmean = rowMeans(CumBiomass),
B5q = apply(CumBiomass, 1, quantile, probs = 0.05),
B95 = apply(CumBiomass, 1, quantile, probs = 0.95),
Bstatic = rowSums(AbvG_Carbon_Trees, na.rm=T) * 1000/(2 * pi * 5^2),
sd = apply(CumBiomass, 1, sd,na.rm=T)
)


##############
# Increments # -----------------------------------------------------------------
##############
            
# Aboveground C biomass INCREMENTS for individual trees 
AbvG_Carbon_Increment_Trees = apply(AbvG_Carbon_Trees, 2, diff, lag = 1, differences = 1) 

# [g C m-2]
Increments = matrix(NA, nrow=nrow(AbvG_Carbon_Increment_Trees), ncol=100)
rownames(Increments) = rownames(AbvG_Carbon_Increment_Trees)

for (it in 1:100){ # 100 iterations for the bootstraping

 Incr_reconst=data.frame(Year = nrow(AbvG_Carbon_Increment_Trees), Increment = NA)
 
 for (i in 1:nrow(AbvG_Carbon_Increment_Trees)){ # change the stand density evry year
  trees = sample(colnames(AbvG_Carbon_Increment_Trees), SD_ts$Density[i+1]/100)
  dummy = sum(AbvG_Carbon_Increment_Trees[i,trees], na.rm=T) * 1000 /100
  Incr_reconst[i, "Increment"] = dummy
 }

 Increments[,it] = Incr_reconst$Increment # [g C m-2]

}



Increments_overview = data.frame(
Year = as.numeric(rownames(AbvG_Carbon_Increment_Trees)),
Imean = rowMeans(Increments),
I5q = apply(Increments, 1, quantile, probs = 0.05),
I95 = apply(Increments, 1, quantile, probs = 0.95),
Istatic = rowSums(AbvG_Carbon_Increment_Trees, na.rm=T) * 1000/(2 * pi * 5^2),
sd = apply(AbvG_Carbon_Increment_Trees, 1, sd,na.rm=T)
)

# Save data --------------------------------------------------------------------
save(CumBiomass_overview, Increments_overview, SD_ts, file = "StandBiomassReconstructions.RData")  

# Save data --------------------------------------------------------------------

# cumulative aboveground live biomass for the period 1980-2015
write.csv(subset(CumBiomass_overview, rownames(CumBiomass_overview) %in% c(1980:2015)),
          file="AGB.csv",
          quote=F, row.names=F)
          
# aboveground biomass increments for the period 1980-2015
write.csv(subset(Increments_overview, rownames(Increments_overview) %in% c(1980:2015)),
          file="AGB_increments.csv",
          quote=F, row.names=F)


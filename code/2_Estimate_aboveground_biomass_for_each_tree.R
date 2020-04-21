################################################################################
# Estimate aboveground biomass for each tree
################################################################################

rm(list=ls())

# load the historical DBH time series
load("DBH_timeseries.RData")


# Allometric equations

# LALA
# Reference: Lambert 2005
# Species: Tamarack
# aboveground biomass = Bark(a1,b1) + Branches(a2,b2) + Foliages(a3,b3) + Wood(a4,b4) [kg]
# DBH = diameter at breast height [cm]
# a, b = estimated parameters 
LALA_AGD = function(DBH){
  a1 = 0.0174
  b1 = 2.1109
  a2 = 0.0196
  b2 = 2.2652
  a3 = 0.0801
  b3 = 1.4875
  a4 = 0.0625
  b4 = 2.4475
  biomass = (a1*(DBH**b1)) + (a2*(DBH**b2)) + (a3*(DBH**b3)) + (a4*(DBH**b4))   
  return(biomass)
}

# PIMA
# Reference: Ung 2008
# Species: Black Spruce
# aboveground biomass = Bark(a1,b1) + Branches(a2,b2) + Foliages(a3,b3) + Wood(a4,b4) [kg]
# DBH = diameter at breast height [cm]
# a, b = estimated parameters 
PIMA_AGD = function(DBH){
  a1 = 0.0148
  b1 = 2.2494
  a2 = 0.0291
  b2 = 2.0751
  a3 = 0.1631
  b3 = 1.4222
  a4 = 0.0494
  b4 = 2.5025
  biomass = (a1*(DBH**b1)) + (a2*(DBH**b2)) + (a3*(DBH**b3)) + (a4*(DBH**b4))   
  return(biomass)
}

# PIMA -------------------------------------------------------------------------
PIMA_A = DBH_plot_A[,plotA$ID[plotA$Species == "Black spruce"]]
PIMA_B = DBH_plot_B[,plotB$ID[plotB$Species == "Black spruce"]]

AbvG_DM_PIMA_plotA = PIMA_AGD(PIMA_A)
AbvG_DM_PIMA_plotB = PIMA_AGD(PIMA_B)

# combine plot A and B
f = matrix(NA, nrow=nrow(AbvG_DM_PIMA_plotA)-nrow(AbvG_DM_PIMA_plotB), 
               ncol=ncol(AbvG_DM_PIMA_plotB))
colnames(f)=colnames(AbvG_DM_PIMA_plotB)
rownames(f)=1894:1896  
AbvG_DM_PIMA = cbind(AbvG_DM_PIMA_plotA, rbind(f, AbvG_DM_PIMA_plotB))

# convert to carbon content PIMA 0.51 kg C (kg dry mass)-1 
AbvG_Carbon_PIMA = AbvG_DM_PIMA * 0.51 


# LALA -------------------------------------------------------------------------
LALA_A = DBH_plot_A[,plotA$ID[plotA$Species == "Larch"]]
LALA_B = DBH_plot_B[,plotB$ID[plotB$Species == "Larch"]]

AbvG_DM_LALA_plotA = LALA_AGD(LALA_A)
AbvG_DM_LALA_plotA = data.frame(A01 = AbvG_DM_LALA_plotA)
AbvG_DM_LALA_plotB = LALA_AGD(LALA_B)

# combine plot A and B
f = matrix(NA, nrow=nrow(AbvG_DM_LALA_plotA)-nrow(AbvG_DM_LALA_plotB), 
               ncol=ncol(AbvG_DM_LALA_plotB))
colnames(f)=colnames(AbvG_DM_LALA_plotB)
rownames(f)=1894:1896  
AbvG_DM_LALA = cbind(AbvG_DM_LALA_plotA, rbind(f, AbvG_DM_LALA_plotB))

# convert to carbon content LALA 0.47 kg C (kg dry mass)-1 
AbvG_Carbon_LALA = AbvG_DM_LALA * 0.47 

# Forest stand -----------------------------------------------------------------

# Aboveground C biomass for individual trees 
AbvG_Carbon_Trees = cbind(AbvG_Carbon_PIMA, AbvG_Carbon_LALA)

# Save data --------------------------------------------------------------------
save(AbvG_Carbon_Trees, file = "AGB4eachTree.RData")   

write.csv(AbvG_Carbon_Trees,
          file="AbvG_Carbon_Trees.csv",
          quote=F, row.names=T)
          
              
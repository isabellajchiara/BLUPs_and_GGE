
library(metan)
library(dplyr)

data = read.csv("KelvinData.csv")

data$Adjusted.Yield <- as.numeric(as.character(data$Adjusted.Yield))  # yield was character, must be numeric
data$Rep <- as.numeric(as.character(data$Rep)) 
data$Location <- as.factor(as.character(data$Location)) 
data$Genotype <- as.factor(as.character(data$Genotype)) 

inspect(data)

mixed_mod <- gamem_met(data,
            env = Location,
            gen = Genotype,
            rep = Rep,
            resp = Adjusted.Yield,
            random = "env", #genotypes are fixed. this could also be "gen", making env fixed
            verbose = TRUE) #Default

BLUPS <- print(mixed_mod$Adjusted.Yield$BLUPint)
 #will create table of BLUPs
 

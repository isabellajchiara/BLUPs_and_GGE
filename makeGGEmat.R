library(GGEBiplots)
library(metan)

data = read.csv("KelvinData.csv")

#data has 5 columns: Genotype, Location, Year, Rep, Yield

## add a 6th column naming all 7 environments by location and year

env = list()
for (x in 1:nrow(data)){

  newcol = paste(data[x,2],data[x,3],sep = "")
  env[[x]] = newcol
  
}

env = as.data.frame(env)
env = t(env)

data = cbind(data,env)
data = data[,-c(2,3)]

# find mean of each genotype at each location
means = list() # mean yield across reps at for each geno
genotypes = list() # will hold geno id at hand
env = list() # will hold environment id at hand 

data$Adjusted.Yield <- as.numeric(as.character(data$Adjusted.Yield))  # yield was character, must be numeric
data$Rep <- as.factor(as.factor(data$Rep)) 
data$env <- as.factor(as.factor(data$env)) 


i = 1
x = 1 
while (i < nrow(data)){
  yields = data[(i:(i+2)),3] # yield is 3th column
  yieldMean = mean(yields) # mean yield for each rep 
  means[[x]] = yieldMean # assign mean
  genotypes[[x]] = data[i,1] # take down genotype id
  env[[x]] = data[i,4] # take down env id
  i = i + 3 # move on to next set of 3 obs 
  x = x + 1 #fill in next spot in list
}

means = as.data.frame(t(means))
genotypes = as.data.frame(t(genotypes))
env = as.data.frame(t(env))

dataUpdated = as.data.frame(t(rbind(genotypes, means, env)))
colnames(dataUpdated) = c("geno","yield","env") #new dataUpdated gives genotype, mean yield, and location
dataUpdated <- as.data.frame(lapply(dataUpdated, unlist))


### Next we will make a unique data frame for each environment 

envlist = c("GART2021","KABWE2021","MPIKA2021","MPIKA2018","MPIKA2020","UNZA2017","UNZA2019")
datalist = list()
x = 1

for (i in envlist){
  df = dataUpdated[dataUpdated$env==i,] #pull out one env
  rownames = as.character(df[,1]) #must keep genotype names 
  yield = as.data.frame(df[,2]) #we just need yield
  rownames(yield) = rownames #assign genotypes to yields via rownames 
  colnames(yield) = i #colname is the env
  yield = as.data.frame(yield) #must be DF
  assign(paste0(i),yield) #write to global env
}


merge = merge(GART2021,KABWE2021, by='row.names') #look at genotypes present in both environments 
rownames = merge[,1]
merge = merge[,-1]
rownames(merge) = rownames

newEnvList = c(MPIKA2021,MPIKA2018,MPIKA2020,UNZA2017,UNZA2019)

for (env in newEnvList) {
  merge = merge(merge,UNZA2019 ,by='row.names') #look at genotypes that are present in all environments 
  rownames = merge[,1]
  merge = merge[,-1]
  rownames(merge) = rownames
}

GGEmat = merge

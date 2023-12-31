# load packages 
install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")

install.packages("/Users/justin_folders/Downloads/asreml-4.1.0.176-macOS-10.13.2-R4.1.tar.gz",
repos = NULL, type = "source") #this step is for downloading asreml - the path will be specifict to your computer so you need to change the pathway for it to work  

library(asreml)
library(data.table)
library(ggplot2)
library(jsonlite)

asreml.license.activate() 

# load and organize data
data = read.csv("KelvinData.csv")
#data has four columns: Genotype, Location, Year, Rep, Adjusted Yield 

env = list()
for (x in 1:nrow(data)){
    newcol = paste(data[x,2],data[x,3],sep = "")
    env[[x]] = newcol
    }

env = as.data.frame(env)
env = t(env)

data = cbind(data,env)
colnames(data) = c("name","loc","year","rep","yield","year_loc")
cols = c("name","loc","year","rep","yield","year_loc")
data[cols] <- lapply(data[cols], factor)
data$yield = as.numeric(data$yield)

#Estimate BLUEs for each location adjusting by year

Envs = levels(data$loc)
stgI_list = matrix(data=list(),nrow=length(Envs), ncol=1,dimnames=list(Envs,c("BLUES")))

for (i in Envs){
    Edat = droplevels(subset(data,loc==i))

    mod.1 <- asreml(fixed = yield ~ name:rep,
                random = ~ year + name:year,
                sparse = ~ year + name:year + name:rep, # full variance-covariance matrix is not required.
                data = Edat,
                predict = predict.asreml(classify="name:rep",vcov=TRUE,aliased = T, fill=TRUE),
                ai.sing=FALSE,
                trace = F,
                maxit = 500)

blue.1 <- data.table((mod.1$predictions$pvals[1:4]))
names(blue.1) <- c("name","rep","yield","se")
blue.1$loc = i
stgI_list[[i,"BLUES"]] <- blue.1
blues_stage1 <<- do.call(rbind, stgI_list)
}

#

data = blues_stage1
Envs = levels(data$loc)
stgI_list = matrix(data=list(),nrow=length(Envs), ncol=1,dimnames=list(Envs,c("BLUPS")))

# Use BLUEs to determine variation caused by G,E, & GE (across locations)
for (i in Envs){
  Edat = droplevels(subset(data, loc==i))
  
  mod.2 <- asreml(fixed = yield ~ loc,
                  random = ~ name + name:loc,
                  sparse = ~ name + name:loc,
                  data = Edat,
                  predict = predict.asreml(classify="name",vcov=TRUE,aliased = T, fill=TRUE),
                  trace = F,
                  maxit = 500,
                  ai.sing=FALSE)
  
  blup.1 <- data.table((mod.2$predictions$pvals[1:3]))
  names(blup.1) <- c("name","yield","se")
  blup.1$loc = i
  stgI_list[[i,"BLUPS"]] <- blup.1
  blups_stage2 <<- do.call(rbind, stgI_list)
}

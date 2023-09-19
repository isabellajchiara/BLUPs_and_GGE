install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")

install.packages("/Users/justin_folders/Downloads/asreml-4.1.0.176-macOS-10.13.2-R4.1.tar.gz",
repos = NULL, type = "source")

library(asreml)
library(data.table)
library(ggplot2)
library(jsonlite)

asreml.license.activate() 

data = read.csv("KelvinDataCommon.csv")
data = data[,-1]

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

Envs = levels(data$loc)
stgI_list = matrix(data=list(),nrow=length(Envs), ncol=1,dimnames=list(Envs,c("BLUPS")))

for (i in Envs){
    Edat = droplevels(subset(data, loc==i))

    mod.1 <- asreml(fixed = yield ~ loc + rep + loc:rep,
                random = ~ name + name:loc,
                sparse = ~ name + name:loc,
                data = Edat,
                predict = predict.asreml(classify="name",vcov=TRUE,aliased = T, fill=TRUE),
                trace = F,
                maxit = 500,
                ai.sing=FALSE)

blup.1 <- data.table((mod.1$predictions$pvals[1:3]))
names(blup.1) <- c("name","yield","se")
blup.1$loc = i
stgI_list[[i,"BLUPS"]] <- blup.1
blups_stage1 <<- do.call(rbind, stgI_list)
}

#

blups_stage1 = as.data.frame(blups_stage1)


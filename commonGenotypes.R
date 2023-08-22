data = read.csv("KelvinData.csv")
commonGeno = read.csv("commonGeno.csv")


x = 1 
obs = nrow(data)

while (x < obs){
  geno = data[x,1]
  if ((geno %in% commonGeno$commonGeno) == FALSE){
    data[x,1:5] = NA}
    x = x+1
  }
  
data <- na.omit(data)
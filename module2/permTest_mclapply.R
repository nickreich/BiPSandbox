## locally parallelized permutation test
## BiP HPC workshop 2014
## Nicholas Reich

require(parallel)
require(alr3)
require(ggplot2)
data(heights)
nSim <- 100 ## number of permutations
nCores <- 10


########################################################
#### YOU WILL NEED TO CHANGE THIS WORKING DIRECTORY ####

#setwd("/Users/gregorymatthews/Dropbox/BiPSandbox/code")
setwd("/Users/nick/Documents/code_versioned/BiP/2013HPCwithR/modules/module2/labs/")

## Use mclapply
permTest<-function(x){
  permDhts <- sample(heights$Dheight, replace=FALSE)
  mdl <- lm(permDhts ~ heights$Mheight)
  output <- coef(mdl)
  lineText <- paste(round(output,4), collapse=",")
  #cat(c(lineText, "\n"), file="coefsDistr.csv", append=TRUE)
  output
}
out <- mclapply(as.list(c(1:nSim)),permTest,mc.cores=2)
tic <- Sys.time()
mat <- do.call(rbind,out)
toc <- Sys.time()
mat <- cbind(c(1:dim(mat)[1]), mat)
colnames(mat) <- c("iter", "b0", "b1")

## fit the data to the real data
realDataModel <- lm(Dheight ~ Mheight, data=heights)
realData_beta1 <- coef(realDataModel)[2]

(pval <- sum(abs(mat[,"b1"]) > realData_beta1)/nrow(mat))


# Import libraries
library(rjags)
library(ggplot2)

set.seed(34539403)

# -------------
# Data set-up
# -------------
# Read in the dye data
satData <- read.table("SAT.txt",header=T)

# number of schools
nSchools <- dim(satData)[1] 

# estimated coaching effects
y <- satData$y  

# standard error of the coaching effects
s2 <- satData$sigma.sq

# school labels
school <- satData$school

# -------------------------------------------
# MODEL 1:  sigma2 is known
# -------------------------------------------

# set s2
sigma2 <- 100

# -------------
# JAGS Set-up
# -------------

# create a data list
dataList <- list("y" = y,
                 "s2" = s2,
                 "sigma2" = sigma2,
                 "nSchools" = nSchools)

# list of parameters to be monitored  
parameters <- c("theta", 
                "mu")

# set initial values
initsValues <- list("theta" = rep(0, nSchools), 
                    "mu" = 0)

# number of iteration for "tuning" 
adaptSteps <- 5000 

# number of iterations for "burn-in" 
burnInSteps <- 5000   

# number of chains to run
nChains <- 2          

# total number of iterations to save
numSavedSteps <- 10000           

# "thinning" (1 = keep every interation)
thinSteps <- 1                  

# iterations per chain
ITER <- ceiling((numSavedSteps * thinSteps )/ nChains) 

# -------------
# Run JAGS
# -------------

# create, initialize, and adapt the model
jagsModel <- jags.model("jags-SAT-model-1.txt", 
                        data = dataList, 
                        inits = initsValues, 
                        n.chains = nChains, 
                        n.adapt = adaptSteps)

# burn-in the algorithm
update(jagsModel, 
       n.iter = burnInSteps)

# run algorithm to get interations for inference
codaSamples <- coda.samples(jagsModel, 
                            variable.names = parameters, 
                            n.iter = ITER, 
                            thin = thinSteps)

# -------------
# Look at posterior samples
# -------------

# make a dataframe with the posterior samples
mcmcChainDF <- data.frame(as.matrix(codaSamples, 
                                    iters = T, 
                                    chains = T))

# create a vector with the variable names
varNames <- names(mcmcChainDF)[3:(dim(mcmcChainDF)[2])]

# number of variables
nVars <- length(varNames)

mcmcChainDF$CHAIN <- as.factor(mcmcChainDF$CHAIN)

# construct trace plots
par(ask = T)
for( i in 1:nVars )
{
  print(ggplot(mcmcChainDF, 
               aes( x = ITER, 
                    y = mcmcChainDF[ ,varNames[i]])) +
          geom_line(aes(color = CHAIN)) + 
          labs(y = varNames[i]))
  flush.console()
}

# summarize the posterior distributions
# of the thetas and mu

postDFreshape <- melt( mcmcChainDF, 
                       id.vars = "ITER",
                       measure.vars = c("theta.1.",
                                        "theta.2.",
                                        "theta.3.",
                                        "theta.4.",
                                        "theta.5.",
                                        "theta.6.",
                                        "theta.7.",
                                        "theta.8.",
                                        "mu"))

par(ask = F)
cols <- hue_pal()(4)

ggplot(postDFreshape, 
       aes(x = variable, y = value )) +
  geom_boxplot( fill = c(rep(cols[3], nSchools), 
                          cols[1]),
                color = "darkgrey") +
  scale_x_discrete( labels = c( as.character(satData$school), 
                                "mu" )) +
  ylab( "posterior" ) +
  xlab( "theta" ) +
  theme(axis.title.x = element_text(colour = cols[3]))

# -------------
# Try a different sigma2: maybe 1?
# -------------

# -------------------------------------------
# MODEL 2:  sigma2 is unknown
# -------------------------------------------

# -------------
# JAGS Set-up
# -------------

# create a data list
dataList <- list("y" = y,
                 "s2" = s2,
                 "nSchools" = nSchools)

# list of parameters to be monitored  
parameters <- c("theta", 
                "mu",
                "sigma",
                "sigma2")

# set initial values
initsValues <- list("theta" = rep(0, nSchools), 
                    "mu" = 0,
                    "sigma" = 1)

# number of iteration for "tuning" 
adaptSteps <- 5000 

# number of iterations for "burn-in" 
burnInSteps <- 5000   

# number of chains to run
nChains <- 2          

# total number of iterations to save
numSavedSteps <- 10000           

# "thinning" (1 = keep every interation)
thinSteps <- 1                  

# iterations per chain
ITER <- ceiling((numSavedSteps * thinSteps )/ nChains) 

# -------------
# Run JAGS
# -------------

# create, initialize, and adapt the model
jagsModel <- jags.model("jags-SAT-model-2.txt", 
                        data = dataList, 
                        inits = initsValues, 
                        n.chains = nChains, 
                        n.adapt = adaptSteps)

# burn-in the algorithm
update(jagsModel, 
       n.iter = burnInSteps)

# run algorithm to get interations for inference
codaSamples <- coda.samples(jagsModel, 
                            variable.names = parameters, 
                            n.iter = ITER, 
                            thin = thinSteps)

# -------------
# Look at posterior samples
# -------------

# make a dataframe with the posterior samples
mcmcChainDF <- data.frame(as.matrix(codaSamples, 
                                    iters = T, 
                                    chains = T))

# create a vector with the variable names
varNames <- names(mcmcChainDF)[3:(dim(mcmcChainDF)[2])]

# number of variables
nVars <- length(varNames)

mcmcChainDF$CHAIN <- as.factor(mcmcChainDF$CHAIN)

# construct trace plots
par(ask = T)
for( i in 1:nVars )
{
  print(ggplot(mcmcChainDF, 
               aes( x = ITER, 
                    y = mcmcChainDF[ ,varNames[i]])) +
          geom_line(aes(color = CHAIN)) + 
          labs(y = varNames[i]))
  flush.console()
}

# summarize the posterior distributions
# of the thetas and mu

postDFreshape <- melt( mcmcChainDF, 
                       id.vars = "ITER",
                       measure.vars = c("theta.1.",
                                        "theta.2.",
                                        "theta.3.",
                                        "theta.4.",
                                        "theta.5.",
                                        "theta.6.",
                                        "theta.7.",
                                        "theta.8.",
                                        "mu"))
par(ask = F)
cols <- hue_pal()(4)

ggplot(postDFreshape, 
       aes(x = variable, y = value )) +
  geom_boxplot( fill = c(rep(cols[3], nSchools), 
                         cols[1]),
                color = "darkgrey") +
  scale_x_discrete( labels = c( as.character(satData$school), 
                                "mu" )) +
  ylab( "posterior" ) +
  xlab( "theta" ) +
  theme(axis.title.x = element_text(colour = cols[3]))

# posterior mean of sigma2
mean(mcmcChainDF$sigma2)

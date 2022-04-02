### Packages
library(tidyverse)
library(ctsem)
library(rstan)

### Raw Data
df = read.csv("https://www.dropbox.com/s/ew8k1jgwxju88x1/test3latent.csv?dl=1")

df = df[which(!(df$Time %% 1) %in% c(0.25)),]

### Set Seed
set.seed(3710)


# Choose Manifest Variables 
regressors = c("Autonomy", "Resources", "Expertise")


## Make Strings for the model
## based on the number of regressors
lambdas = c(1)
manifest = c(0)
for (i in 2:length(regressors)) {
  
  temp = paste0("lambda", i)
  temp2 = paste0("manifestmean", i)
  lambdas[i] = temp
  manifest[i] = temp2
}


# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 1), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                n.latent=1,
                
                T0MEANS = 0,
                
                MANIFESTVAR = "free",
                
                latentNames=c('Capacity'),
                
                CINT = matrix('cint'),
                
                id = "Agency",
                
                time = "Time")


model = ctStanFit(datalong = df,
                  ctstanmodel = model,
                  iter = 2000,
                  chains = 4,
                  cores = 4, 
                  optimize = FALSE,
                  nopriors = FALSE,
                  control = list(max_treedepth = 13, adapt_delta = .99))

save(model, file = "test3latent")
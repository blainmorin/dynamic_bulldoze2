### Packages
library(tidyverse)
library(ctsem)
library(rstan)

### Raw Data
fed = read.csv("https://www.dropbox.com/s/xtt4emmz6txtbun/fed_agency_capacity_autonomy.csv?dl=1")

### Set Seed
set.seed(3710)


# Choose Year Range
startyear = 1981 # Needs to be >=1974
endyear = 2010 # Needs to be <=2019

# Choose Agency Type
agencytype = c("Natural Resources and Environment",
               "Health",
               "Education")

# Choose Manifest Variables 
regressors = c("appt_pct", "expert_pct")
# names(fed)

# Choose Minimum Employee Size
minemployee = 3000


# Data Clean
df = fed %>%
  filter(yr >= 1974) %>%
  filter(med_sal_ > 0) %>% 
  filter(n > minemployee) %>%
  drop_na(med_sal_) %>% ### Am dropping NA here, but may not need to with ctsem
  filter(AGYSUB != "TOTL") %>% ### Removes Total Rows 
  filter(!grepl("NET", agy_full)) ### Removes any total agency counts, ie only individual agencies are left

# Data Process
years = startyear:endyear
dff = df %>%
  filter(agy_typ %in% agencytype) %>%
  filter(yr %in% startyear:endyear) %>%
  select(regressors, yr, AGYSUB, agy_full) %>%
  mutate_at(regressors, scale) %>%
  drop_na()


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

dff$yr = dff$yr - 1980

# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 1), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                MANIFESTMEANS = matrix(manifest,
                                       nrow = length(regressors), 
                                       ncol = 1), # Forcing the first mean to be 0
                
                n.latent=1,
                
                T0MEANS = 0,
                
                MANIFESTVAR = "free",
                
                latentNames=c('Autonomy'),
                
                CINT = matrix('cint'),
                
                id = "agy_full",
                
                time = "yr")


model = ctStanFit(datalong = dff,
                  ctstanmodel = model,
                  iter = 2000,
                  chains = 4,
                  cores = 4, 
                  optimize = FALSE,
                  nopriors = FALSE,
                  control = list(max_treedepth = 10, adapt_delta = .9))
save(model, file = "model13")
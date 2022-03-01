### Packages
library(tidyverse)
library(ctsem)
library(rstan)

### Raw Data
fed = read.csv("https://www.dropbox.com/s/wlhuq8mdyidxdk3/fed_agency_capacity_autonomy.csv?dl=1")


### Make "top" level variables for appt and advs
top.level = paste0(unique(fed$agy_code), "00")

appendage = fed %>%
  select(agy_code, AGYSUB, yr, appt_pct, advs_pct, expert_pct) %>%
  filter(AGYSUB %in% top.level) %>%
  select(-AGYSUB) %>%
  rename(top_appt_pct = appt_pct, 
         top_advs_pct = advs_pct,
         top_expert_pct = expert_pct)

fed = fed %>%
  left_join(appendage)


drop.agy = fed %>%
  group_by(yr, agy_code) %>%
  summarise(n_in_code = n()) %>%
  filter(n_in_code > 1)

drop.agy = paste0(unique(drop.agy$agy_code), "00")

fed = fed %>%
  filter(!AGYSUB %in% drop.agy)


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
regressors = c("med_sal_", "LOSavg", "ma_pct",
               "b18", "n_index")
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

lambdas = c(1, "lambda21", "lambda31", 0, 0,
            1, "lambda23", "lambda33", "lambda42", "lambda52")

dff$yr = dff$yr - 1980

# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 2), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                n.latent=2,
                
                T0MEANS = 0,
                
                MANIFESTVAR = "free",
                
                latentNames=c('Expertise', 'Capacity'),
                
                CINT = c('cint1', 'cint2'),
                
                id = "agy_full",
                
                time = "yr")


model = ctStanFit(datalong = dff,
                  ctstanmodel = model,
                  iter = 4000,
                  chains = 4,
                  cores = 4, 
                  optimize = FALSE,
                  nopriors = FALSE,
                  control = list(max_treedepth = 13, adapt_delta = .99))

save(model, file = "model80")
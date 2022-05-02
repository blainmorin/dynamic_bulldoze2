### Packages
library(tidyverse)
library(ctsem)
library(rstan)


### Raw Data
fed = read.csv("https://www.dropbox.com/s/wlhuq8mdyidxdk3/fed_agency_capacity_autonomy.csv?dl=1")

agy_types = unique(fed$agy_typ)[unique(fed$agy_typ) != "TOTAL"]

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
  filter(!AGYSUB %in% drop.agy) %>%
  filter(!agy_full == "Dept. of Defense") %>%
  filter(AGYSUB != "TOTL") %>% ### Removes Total Rows 
  filter(!grepl("NET", agy_full))

### Set Seed
set.seed(3710)


# Choose Year Range
startyear = 1981 # Needs to be >=1974
endyear = 2010 # Needs to be <=2019

min_empl = 3000

# Choose Agency Type
agencytype = c("Natural Resources and Environment",
               "Health",
               "Social Welfare",
               "Crime, Law Enforcement, and Incarceration")

temp = fed %>%
  filter(n >= min_empl) %>%
  filter(agy_typ %in% agencytype)

inc = unique(temp$agy_full)

df = fed %>%
  filter(agy_full %in% inc)

regressors = c("log_appt_pct",  "log_advs_pct", "log_top_appt_pct", "log_top_advs_pct", "log_b18_dng_r",
               "logn", "logb18",
               "med_sal_", "LOSavg", "ma_pct")

dff = df %>%
  filter(yr %in% startyear:endyear) %>%
  mutate(logn = log(n + 1)) %>%
  mutate(logb18 = log(b18 + 1)) %>%
  mutate(top_appt_pct = ifelse(is.na(top_appt_pct), appt_pct, top_appt_pct)) %>%
  mutate(top_advs_pct = ifelse(is.na(top_advs_pct), advs_pct, top_advs_pct)) %>%
  mutate(log_appt_pct = log(appt_pct + 1)) %>%
  mutate(log_advs_pct = log(advs_pct + 1)) %>%
  mutate(log_top_appt_pct = log(top_appt_pct + 1)) %>%
  mutate(log_top_advs_pct = log(top_advs_pct + 1)) %>%
  mutate(b18_dng_r = ifelse(is.infinite(b18_dng_r), 750, b18_dng_r)) %>%
  mutate(log_b18_dng_r = log(b18_dng_r + 1)) %>%
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

lambdas = c(-1, "lam21", "lam31", "lam41", "lam51", 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 1, "lam72", 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 1, "lam93", "lam103")

manifestvar = data.frame(
  
  auto1 = c("var11", "cov21", "cov31", "cov41", "cov51", 0, 0, 0, 0, 0),
  auto2 = c(0, "var22", "cov32", "cov42", "cov52", 0, 0, 0, 0, 0),
  auto3 = c(0, 0, "var33", "cov43", "cov53", 0, 0, 0, 0, 0),
  auto4 = c(0, 0, 0, "var44", "cov54", 0, 0, 0, 0, 0),
  auto5 = c(0, 0, 0, 0, "var55", 0, 0, 0, 0, 0),
  reso1 = c(0, 0, 0, 0, 0, "var66", "cov76", 0, 0, 0),
  reso2 = c(0, 0, 0, 0, 0, 0, "var77", 0, 0, 0),
  expert1 = c(0, 0, 0, 0, 0, 0, 0, "var88", "cov98", "cov108"),
  expert2 = c(0, 0, 0, 0, 0, 0, 0, 0, "var99", "cov109"),
  expert3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, "var1010")
  
  
)

manifestvar = as.matrix(manifestvar)

# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 3), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                n.latent=3,
                
                T0MEANS = 0,
                
                latentNames=c('Autonomy', 'Resources', 'Expertise'),
                
                MANIFESTVAR = manifestvar,
                
                CINT = c('cint1', 'cint2', 'cint3'),
                
                id = "agy_full",
                
                time = "yr")


model = ctStanFit(datalong = dff,
                  ctstanmodel = model,
                  iter = 2000,
                  chains = 8,
                  cores = 8, 
                  optimize = FALSE,
                  nopriors = FALSE,
                  control = list(max_treedepth = 13, adapt_delta = .85))

save(model, file = "model105")

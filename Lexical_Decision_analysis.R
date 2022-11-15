
## setup
library(dplyr)
library(readxl)
library(foreign)
library(ggplot2)
library(rjags)
library(runjags)
source("DBDA2E-utilities.R")

# Write data file location here
setwd("######")
# Notice that if you want to see the Hebrew words in R you need to 
# change the R coding to UTF-8

## Uploading the data
data <- read_xlsx('LD_data.xlsx')

##### Visualizations of raw data #####

## Descriptive graph of the data 1- Raw data 
Words_data = data$RT[data$Word_type==0] ## all RT's for words
NonWords_data =  data$RT[data$Word_type==1] ## all RT's for Nonwords
h1 = hist(Words_data, breaks = seq(0, 2000, by = 50), plot = F)
h2 = hist(NonWords_data, breaks = seq(0, 2000, by = 50), plot = F)
plot(h1, col=rgb(0,0,1,1/4), xlim = c(0, 2000), xlab = "RT (ms)", main="RT to Words and Nonwords")
plot(h2, col=rgb(1,0,0,1/4), add = T)
legend('topright', c('Words', 'Nonwords'),
       fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


# calculating mean RT per condition per subject
mean_RTs = data %>% group_by(Subject, Word_type) %>% 
  summarise(mean_RT = mean(RT))

# calculating the differences between the means per subject
Words_means = filter(mean_RTs, Word_type == 0)$mean_RT
NonWords_means= filter(mean_RTs, Word_type == 1)$mean_RT
Differences=NonWords_means-Words_means # the vector of differences in RT

# Descriptive graph of the data 2- vector of differences in RT
hist(Differences, breaks = seq(-300, 300, by = 25),xlab = "Differences in RT (ms)",
     main="Differences in RT to Words & Nonwords Per Subject",col=rgb(1,0,0,1/2))

##### Statistical analysis#####

## Non-parametric test 

# preperations
resampleNum = 10000  # number of permuted samples
n_sample    = length(Differences) # in order to keep the same sample size
permutation_sample = rep(NA, resampleNum) # vector to store all the permuted average differences

# Permutation
for (i in 1:resampleNum){
  signs = sample(x = c(-1, 1), size = n_sample, replace = TRUE)
  permutation_sample[i] = mean(Differences * signs) # The measure we are permuting is the mean difference
}

HDIofMCMC(permutation_sample) # CI
perm_p = sum(permutation_sample >= mean(Differences))/resampleNum #calculating p-value

## Bayesian hierarchical model

# Specify the data in a list, for later shipment to JAGS:
dataList = list(y         = data$RT,
                Ntotal    = length(data$RT),
                Nsubjects = length(unique(data$Subject)),
                s         = data$Subject,
                c         = data$Word_type,
                meanY     = mean(data$RT),
                sdY       = sd(data$RT))

# Specify the model
HierModel = "model {
  # Word type
  for ( i in 1:Ntotal ) {
    non[i] = c[i] * mu_nonword_s[s[i]]            # Was this a nonword?
    mu[i]  = mu_word_s[s[i]] + non[i]             # Mean for this trial (word effect for subject + nonword effect for subject)
    y[i]   ~ dt(mu[i], 1/sigma_s[s[i]]^2, nu)     # RT at this trial
  }
  
  # Subjects
  for (j in 1:Nsubjects) {
    mu_nonword_s[j]     ~ dnorm(mu_nonword, 1/sigma_nonword^2)  # Additive effect of day per subject
    mu_word_s[j]   ~ dnorm(mu_word, 1/sigma_word^2)             # Baseline per subject
    sigma_s[j] ~ dunif(L, H)                                    # Sigma per subject
  }
  
  # Priors
  mu_nonword       ~ dnorm(0, 1/(100*sdY)^2) # Mean nonword effect for the population
  mu_word     ~ dnorm(meanY, 1/(100*sdY)^2)  # Mean word effect for the population
  sigma_nonword ~ dunif(L, H)                # SD nonword effect for the population
  sigma_word  ~ dunif(L, H)                  # SD word for the population
  nu      ~ dexp(1/30)                       # Normality parameter
  
  L = 1/(100*sdY)
  H = 100*sdY
  }
  "

writeLines(text = HierModel, con = "HierModel.txt") 

modelHier = jags.model(file = "HierModel.txt", data = dataList, n.chains = 3, n.adapt = 1500)
update(modelHier, n.iter = 1500)
postHier = coda.samples(modelHier, variable.names = c("mu_nonword", "mu_word", "sigma_nonword", "sigma_word", "mu_word_s", "mu_nonword_s", "sigma_s", "nu"), n.iter = 15000)

# Diagnostics- Population level
diagMCMC(codaObject = postHier, parName = "mu_word")
diagMCMC(codaObject = postHier, parName = "mu_nonword")
diagMCMC(codaObject = postHier, parName = "sigma_nonword")
diagMCMC(codaObject = postHier, parName = "sigma_word")

# Diagnostics- subject level parameters- subject 5 was chosen as example
diagMCMC(codaObject = postHier, parName = "mu_word_s[5]")
diagMCMC(codaObject = postHier, parName = "mu_nonword_s[5]")
diagMCMC(codaObject = postHier, parName = "sigma_s[5]")

# Diagnostics-nu
diagMCMC(codaObject = postHier, parName = "nu")

###### Results visualization #####

# permutation results
plotPost(permutation_sample, compVal = mean(Differences), main="Permutation Results", xlab="Differences in RT between conditions") 

# Bayesian model results
plotPost(postHier[,"mu_nonword"], xlab="mu_nonword", compVal = 0,main="Bayesian Model Results")

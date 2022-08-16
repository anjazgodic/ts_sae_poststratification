rm(list=ls())
library(mvtnorm)
library(fastDummies)
library(haven)
library(readxl)
library(MASS)
library(VCA)
library(Matrix)
library(dplyr)
library(tidyr)
library(stringr)
library(survey)

#################################
#####READ IN CENSUS COUNTS#######
#######DO MANIPULATIONS##########
#################################

# Read in a file connecting the fips number with the state name so that I can identify states
front <- "~/Desktop/"
fipsst <- read.csv(paste0(front, "FIPSST.csv"))
names(fipsst)[1] <- "fipsst"
fipsst <- fipsst[order(fipsst$fipsst),]

# Read in CENSUS N's and reshape data
ps_count_2016 <- read.csv(paste0(front, "2016_ts_county_n.csv"))[,-1]
ps_count_2017 <- read.csv(paste0(front, "2017_ts_county_n.csv"))[,-1]
ps_count_2018 <- read.csv(paste0(front, "2018_ts_county_n.csv"))[,-1]

# Exclude ages we do not need 
ps_count_2016 <- ps_count_2016[ps_count_2016$age!=5,]
ps_count_2017 <- ps_count_2017[ps_count_2017$age!=5,]
ps_count_2018 <- ps_count_2018[ps_count_2018$age!=5,]

# There is a FIPS number that changed recently, we fix that there
ps_count_2016$fips[ps_count_2016$fips==2158] <- 2270
ps_count_2017$fips[ps_count_2017$fips==2158] <- 2270
ps_count_2018$fips[ps_count_2018$fips==2158] <- 2270

# We create a group variable to make it easier to do the group_by command later
ps_count_2016$group <- paste0(ps_count_2016$RACESEX, "_" , ps_count_2016$age)
ps_count_2017$group <- paste0(ps_count_2017$RACESEX, "_" , ps_count_2017$age)
ps_count_2018$group <- paste0(ps_count_2018$RACESEX, "_" , ps_count_2018$age)

# We delete some variables we no longer need (since we have the new group variable)
ps_count_2016[, c("race4", "age", "RACESEX")] = ps_count_2017[, c("race4", "age", "RACESEX")] = ps_count_2018[, c("race4", "age", "RACESEX")] <- NULL

# For each county, sum all the children in a given group
ps_count_2016 <- ps_count_2016 %>% group_by(fips, group) %>% summarize(county_n = sum(county_n))
ps_count_2016 <- data.frame(ps_count_2016)
ps_count_2017 <- ps_count_2017 %>% group_by(fips, group) %>% summarize(county_n = sum(county_n))
ps_count_2017 <- data.frame(ps_count_2017)
ps_count_2018 <- ps_count_2018 %>% group_by(fips, group) %>% summarize(county_n = sum(county_n))
ps_count_2018 <- data.frame(ps_count_2018)

# Read in some fips numbers we will need
fips <- read.csv(paste0(front, "/fips_to_county.csv"))

# Make group variable factor to use the spread command
ps_count_2016$group <- factor(ps_count_2016$group)
ps_count_2017$group <- factor(ps_count_2017$group)
ps_count_2018$group <- factor(ps_count_2018$group)

# Make the dataset from long format into wide format
ps_count_2016 <- spread(ps_count_2016, group, county_n)
ps_count_2017 <- spread(ps_count_2017, group, county_n)
ps_count_2018 <- spread(ps_count_2018, group, county_n)

# Add a missing county with a count of 1 child per group, it will be averaged at the state level
ps_count_2016 <- rbind(ps_count_2016, c(51515, rep(1, ncol(ps_count_2016)-1)))
ps_count_2017 <- rbind(ps_count_2017, c(51515, rep(1, ncol(ps_count_2017)-1)))
ps_count_2018 <- rbind(ps_count_2018, c(51515, rep(1, ncol(ps_count_2018)-1)))

# Order matrix based on how variables appear in the model output csv files
column_order <- c("fips", "female_6", "male_6", "female_7", "male_7", 
                  "female_8", "male_8", "female_9", "male_9", 
                  "female_10",  "male_10", "female_11",  "male_11", 
                  "female_12", "male_12", "female_13", "male_13",
                  "female_14", "male_14", "female_15", "male_15", 
                  "female_16",  "male_16", "female_17", "male_17")

# Order matrix based on how variables appear in the model output csv files
ps_count_2016 <- ps_count_2016[,column_order]
ps_count_2017 <- ps_count_2017[,column_order]
ps_count_2018 <- ps_count_2018[,column_order]

# Order rows by FIPS number and create a dataframe of counts for years 2019 and 2020 based on 2018
ps_count_2016 <- ps_count_2016[order(ps_count_2016$fips), ]
ps_count_2017 <- ps_count_2017[order(ps_count_2017$fips), ]
ps_count_2018 <- ps_count_2018[order(ps_count_2018$fips), ]
ps_count_2019 <- ps_count_2018
ps_count_2020 <- ps_count_2018

# Put together matrix P_ts of poststratifiation counts based on previous data frames of countys
P_ts <- c()
for(i in c("ps_count_2016", "ps_count_2017", "ps_count_2018", "ps_count_2019", "ps_count_2020")){
  for(j in column_order[-1]){
    tmp <- get(i)
    P_ts <- cbind(P_ts, tmp[,j])
  }
}

P_ts <- data.frame(P_ts)
names(P_ts) <- c(t(outer(column_order[-1], c("_2016", "_2017", "_2018", "_2019", "_2020"), FUN=paste0)))

# Order and clean FIPS numbers in matrix P_ts of poststratifiation counts
P_ts <- data.frame(ps_count_2016[,"fips"], P_ts)
names(P_ts)[1] <- "fips"
P_ts$fips <- as.character(P_ts$fips)
P_ts$fips <- str_pad(P_ts$fips, 5, pad = "0")
P_ts$fips <- substr(P_ts$fips , start = 1, stop = 2)

# Use matrix P_ts of poststratifiation counts and divide by state counts to get the final matrix P_ts of poststratifiation weights
P_ts <- P_ts %>% group_by(fips) %>% summarise_all(sum)
P_ts$state_sums <- rowSums(P_ts[,-1])
for(i in 2:(ncol(P_ts)-1)){
  P_ts[,i] <- P_ts[,i]/P_ts[,"state_sums"]
}

state_sums <- P_ts[,c("fips", "state_sums")]
P_ts$state_sums <- NULL 

#################################
#########READ IN BETA'S##########
#########READ IN RE'S############
########READ IN COV'S############
#################################

# Read in beta's (fixed effect coefficients) and variance-covariance matrix Sigma of these coefficients
beta_hat_ts <- read.csv(paste0(front, "ts_model_est.csv"))[,"Estimate"]
beta_cov_hat_ts <- read.csv(paste0(front, "ts_model_cov.csv"))[,c(paste0("Col", 1:47))]
beta_cov_hat_ts <- as.matrix(beta_cov_hat_ts)

#################################
#####RESHAPE SIGMA FOR DRAWS#####
#################################

# Read in b's (random effect coefficients)
re_ts <- read.csv(paste0(front, "ts_model_RE.csv"))
re_ts <- re_ts[order(re_ts$Subject.1),]
b_o_hat_ts <-  re_ts[,"Estimate"]
b_o_cov_hat_ts <-  re_ts[,"Std.Err.Pred"]
re_ts_id <- re_ts[,c("Subject.1", "Estimate")]

# Remove two states from poststratification weight matrix P_ts since we have no estimates for them
P_ts <- P_ts[as.numeric(P_ts$fips) %in% re_ts_id$Subject.1,]
P_ts <- P_ts[order(as.numeric(P_ts$fips)),]
ps_fips = P_fips <- as.numeric(P_ts$fips)
P_ts$fips <- NULL

# Take off the columns/rows corresponding to reference coefficients for easier computation, will put them back in later on
beta_cov_hat_ts2 <- beta_cov_hat_ts[c(beta_hat_ts!=0),c(beta_hat_ts!=0)]
beta_hat_ts2 <- beta_hat_ts[beta_hat_ts!=0]
beta_cov_hat_ts2 <- as.matrix(nearPD(beta_cov_hat_ts2)$mat) 

# Read in data for state-level covariates
ts_d <- read.csv(paste0(front, "nsch_ts_data.csv"))
ts_d <- ts_d[,c("FIPSST", "somecoll_2019", "Pediatric_Emergency_Medicine")]
ts_d <- unique(ts_d)
ts_d$somecoll_2019_Pediatric_Emergency_Medicine <- ts_d$somecoll_2019 * ts_d$Pediatric_Emergency_Medicine
ts_d <- ts_d[order(ts_d$FIPSST), ]
ts_d$FIPSST <- NULL 

# Create design matrix for TS, put variables in order in which they appear in model
dum <- data.frame(expand.grid(c(2016, 2017, 2018, 2019, 2020), c(1, 2), c(6:17)))
names(dum) <- c("YEAR", "SEX", "AGE")
X_ts <- dummy_cols(dum, select_columns=c("YEAR", "SEX", "AGE"))
X_ts$AGE=X_ts$SEX=X_ts$YEAR <- NULL
names(X_ts)[6:7] <- c("SEX_FEMALE", "SEX_MALE")
X_ts$SEX_FEMALE_AGE_6 <- X_ts$SEX_FEMALE *  X_ts$AGE_6
X_ts$SEX_FEMALE_AGE_7 <- X_ts$SEX_FEMALE *  X_ts$AGE_7
X_ts$SEX_FEMALE_AGE_8 <- X_ts$SEX_FEMALE *  X_ts$AGE_8
X_ts$SEX_FEMALE_AGE_9 <- X_ts$SEX_FEMALE *  X_ts$AGE_9
X_ts$SEX_FEMALE_AGE_10 <- X_ts$SEX_FEMALE *  X_ts$AGE_10
X_ts$SEX_FEMALE_AGE_11 <- X_ts$SEX_FEMALE *  X_ts$AGE_11
X_ts$SEX_FEMALE_AGE_12 <- X_ts$SEX_FEMALE *  X_ts$AGE_12
X_ts$SEX_FEMALE_AGE_13 <- X_ts$SEX_FEMALE *  X_ts$AGE_13
X_ts$SEX_FEMALE_AGE_14 <- X_ts$SEX_FEMALE *  X_ts$AGE_14
X_ts$SEX_FEMALE_AGE_15 <- X_ts$SEX_FEMALE *  X_ts$AGE_15
X_ts$SEX_FEMALE_AGE_16 <- X_ts$SEX_FEMALE *  X_ts$AGE_16
X_ts$SEX_FEMALE_AGE_17 <- X_ts$SEX_FEMALE *  X_ts$AGE_17
X_ts$SEX_MALE_AGE_6 <- X_ts$SEX_MALE *  X_ts$AGE_6
X_ts$SEX_MALE_AGE_7 <- X_ts$SEX_MALE *  X_ts$AGE_7
X_ts$SEX_MALE_AGE_8 <- X_ts$SEX_MALE *  X_ts$AGE_8
X_ts$SEX_MALE_AGE_9 <- X_ts$SEX_MALE *  X_ts$AGE_9
X_ts$SEX_MALE_AGE_10 <- X_ts$SEX_MALE *  X_ts$AGE_10
X_ts$SEX_MALE_AGE_11 <- X_ts$SEX_MALE *  X_ts$AGE_11
X_ts$SEX_MALE_AGE_12 <- X_ts$SEX_MALE *  X_ts$AGE_12
X_ts$SEX_MALE_AGE_13 <- X_ts$SEX_MALE *  X_ts$AGE_13
X_ts$SEX_MALE_AGE_14 <- X_ts$SEX_MALE *  X_ts$AGE_14
X_ts$SEX_MALE_AGE_15 <- X_ts$SEX_MALE *  X_ts$AGE_15
X_ts$SEX_MALE_AGE_16 <- X_ts$SEX_MALE *  X_ts$AGE_16
X_ts$SEX_MALE_AGE_17 <- X_ts$SEX_MALE *  X_ts$AGE_17
Intercept <- rep(1, nrow(X_ts))
X_ts <- cbind(Intercept, X_ts)

#################################
########START BOOTSTRAP##########
#################################

# Designate how many bootstrap repetitions
boot_rep <- 10000

# Create empty matrix to store results. Rows will be repetitions and columns will be counties
p_tilde <- matrix(NA, nrow = boot_rep, ncol=49) #51 with two missing states

# Create empty vector to store beta draws
beta_tilde_clean_ts <- rep(NA, length(beta_hat_ts))
p_tilde_out = p_tilde_fixed_out  = national_rate <- c()

# Bootstrap Procedure for Poststratification
for(i in 1:boot_rep){ 
  set.seed(i)
  print(i)
  
  # Draw coefficients for beta and reinsert zeros to make it match original design matrix
  beta_tilde <- rmvnorm(n = 1, mean = beta_hat_ts2, sigma = beta_cov_hat_ts2)
  beta_tilde_clean_ts[which(beta_hat_ts!=0)] <- beta_tilde
  beta_tilde_clean_ts[which(beta_hat_ts==0)] <- 0
  
  # Subset beta_tilde with participant-level fixed effects
  beta_tilde_subset_group <- beta_tilde_clean_ts[1:44]
  
  # Subset beta_tilde with state-level fixed effects
  beta_tilde_subset_state <- beta_tilde_clean_ts[45:47]
  
  # Draw coefficients for random effects (both missing and observed) using variaance-covariance (sigma) elements we calculated earlier
  b_o_tilde <- rnorm(n = length(b_o_hat_ts), mean = b_o_hat_ts, sd = (b_o_cov_hat_ts)) 
  
  # Sum various beta*X
  sum_part_groups <- t(as.matrix(X_ts) %*% (beta_tilde_subset_group))
  sum_part_county <- as.matrix(ts_d) %*% matrix(beta_tilde_subset_state, nrow = length(beta_tilde_subset_state)) + matrix(b_o_tilde, ncol = 1)
  
  sum_coef <- outer(as.vector(sum_part_county), as.vector(sum_part_groups), FUN = "+")
  # note: each row is one state for all 120 groups (strata of year, gender, age)
  
  # Get on probability scale
  p <- exp(sum_coef)/(1+exp(sum_coef))
  
  # Do Post-Stratification
  p_tilde <- rowSums(p*P_ts)

  # Save Output
  p_tilde_out <- rbind(p_tilde_out,p_tilde)

}

#################################
#######ORIGINAL RESULTS##########
#################################

# Collapse results
p_tilde_out <- data.frame(p_tilde_out)

means <- apply(p_tilde_out, 2, mean)
sds <- apply(p_tilde_out, 2, sd)
lower_mean <- means-1.96*sds
upper_mean <- means+1.96*sds

table1 <- data.frame(ps_fips, means, sds, lower_mean, upper_mean)
table1$CI_length_mean <- table1$upper_mean-table1$lower_mean

state_sums$fips <- as.numeric(state_sums$fips)

table1 <- merge(table1, state_sums, by.x = "ps_fips", by.y = "fips", all.x = T)
table1 <- merge(table1, fipsst, by.x = "ps_fips", by.y = "fipsst", all.y = T)

names(table1) <- c("State_FIPS", 
                   "Prevalence_Estimate_Mean", 
                   "Prevalence_SD_Mean", 
                   "Prevalence_LowCI_Mean", 
                   "Prevalence_UpperCI_Mean",
                   "CI_Length_Mean",
                   "State_Population", "State_Name", 
                   "State_Abbreviation")

#################################
######## BENCHMARKING ###########
#################################

# Get direct estimates
options(survey.lonely.psu = "adjust")
ts_d <- read.csv(paste0(front, "nsch_ts_data.csv"))
ts_d <- ts_d[order(ts_d$FIPSST), ]

des <- svydesign(id      = ~HHID,
                 strata  = ~STRATUM,
                 weights = ~FWC,
                 nest    = TRUE,
                 data    = ts_d)
emp_yes_ts <- svymean(~ts, des)[1] 

# Get counts propotion of children per state: state/national
ps_count_2018$fips[ps_count_2018$fips==2158] <- 2270
ps_count_2018$fips <- str_pad(ps_count_2018$fips, 5, pad = "0")
ps_count_2018$fips <- substr(ps_count_2018$fips , start = 1, stop = 2)
ps_count_2018 <- data.frame(cbind(ps_count_2018$fips, rowSums(ps_count_2018[,-1])))
names(ps_count_2018) <- c("fips", "county_n")
ps_count_2018 <- ps_count_2018 %>% group_by(fips) %>% summarize(state_n = sum(as.numeric(county_n)))
total_pop <- sum(ps_count_2018$state_n)
ps_count_2018$state_n <- ps_count_2018$state_n/total_pop

ps_count_2018 <- ps_count_2018[match(as.numeric(ps_count_2018$fips), table1$State_FIPS),]
ps_count_2018 <- ps_count_2018[!as.numeric(ps_count_2018$fips) %in% c(11,15),]

# Reweight prevalences by state to calculate national rate
p_tilde_out_tmp = sweep(p_tilde_out, 2, ps_count_2018$state_n, FUN="*")
means <- apply(p_tilde_out_tmp, 1, sum)

# Create benchmarking ratio by dividing empirical national rate with our estimated national rate
ratio <- emp_yes_ts/mean(means)

# Reweight our results with benchmarking weight
p_tilde_out_benchmark <- p_tilde_out*ratio

# Save benchmarked estimates
means <- apply(p_tilde_out_benchmark, 2, mean)
sds <- apply(p_tilde_out_benchmark, 2, sd)
lower_mean <- means-1.96*sds
upper_mean <- means+1.96*sds

table2 <- data.frame(sort(as.numeric(ps_count_2018$fips)), means, sds, lower_mean, upper_mean)
table2$CI_length_median <- table2$upper_mean-table2$lower_mean

names(table2) <- c("State_FIPS", 
                   "Benchmarking_Prevalence_Estimate_Mean", 
                   "Benchmarking_Prevalence_SD_Mean", 
                   "Benchmarking_Prevalence_LowCI_Mean", 
                   "Benchmarking_Prevalence_UpperCI_Mean",
                   "Benchmarking_CI_Length_Mean")

# Final results saved here, includes benchmarked estimates and original estimates
results <- merge(table1, table2, by = "State_FIPS", all.x = T)

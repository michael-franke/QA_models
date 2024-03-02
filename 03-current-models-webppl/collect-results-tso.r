library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(mvtnorm)
library(aida)    # remotes::install_github("michael-franke/aida-package")


##################################################

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the aida-theme for plotting
theme_set(theme_aida())

# global color scheme / non-optimized
project_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
} 

##################################################

run_model_tso <- function (params, utils) {
  
  webPPL_data <- tibble('task' = "TSO") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "qa-models-current.webppl",
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

run_model_secondary_goal <- function (params, utils) {
  
  webPPL_data <- tibble('task' = "cost") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "qa-models-current-secondary-goal.webppl",
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}


run_model_prior <- function (params, utils) {
  
  webPPL_data <- tibble('task' = "prior") %>% 
    cbind(params)  %>% 
    cbind(utils)
  
  webppl(
    program_file = "qa-models-current-priors.webppl",
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

priorSampleParams <- function() {
  params <- tibble(
    'policyAlpha'      = runif(1,min = 2.25, max = 2.75), # soft-maximization rationality alpha for action selection in Eq (6)?
    'questionerAlpha'  = runif(1,min = 3.75, max = 4.25), # soft-maximization rationality alpha of pragmatic questioner in Eq (7)?
    'R1Alpha'          = runif(1,min = 2.75, max = 3.25), # soft-maximization rationality alpha of R1 in Eq (2)?
    'relevanceBetaR0'  = 0, # ?
    'relevanceBetaR1'  = runif(1,min = 0.95, max = 0.97), # beta in Eq (3)
    'costWeight'       = runif(1,min = 0.5, max = 3), # Cost(r)
    'questionCost'     = runif(1,min = 0.2, max = 0.3) # Cost(q) included at level of Q1?
  )
  return(params)
}

priorSampleUtils <- function() {
  # covariance matrix for MV-Gaussian
  sigma = matrix(c( 1.0,  0.9,  0.8, -0.5,
                    0.9,  1.0,  0.8, -0.5,
                    0.8,  0.8,  1.0, -0.5,
                   -0.5, -0.5, -0.5,  1.0), byrow = T, nrow = 4)
  # sample from MV-Guassian
  pSample <- rmvnorm(n = 1, mean = c(7,5,4,2), sigma = sigma)
  utils <- tibble(
    'utilTarget'       = pSample[1],
    'utilCompetitor'   = pSample[2],
    'utilSameCat'      = pSample[3],
    'utilOtherCat'     = pSample[4]
  )
  return(utils)
}

priorSampleParamsFixed <- function() {
  params <- tibble(
    'policyAlpha'      = 2.5,
    'questionerAlpha'  = 4,
    'R1Alpha'          = 3,
    'relevanceBetaR0'  = 0,
    'relevanceBetaR1'  = 0.96,
    'costWeight'       = 0.45,
    'questionCost'     = 0.25
  )
  return(params)
}

priorSampleUtilsFixedSUV <- function() {
  utils <- tibble(
    'utilParkingCoffee'     = 7,
    'utilNoParkingCoffee'   = 3,
    'utilNothing'           = 0,
    'R1Context'             = 'R1PriorSUVContext_secondaryGoals'
  )
  return(utils)
}
priorSampleUtilsFixedPedestrian <- function() {
  utils <- tibble(
    'utilParkingCoffee'     = 3,
    'utilNoParkingCoffee'   = 7,
    'utilNothing'           = 0,
    'R1Context'             = 'R1PriorPedestrianContext_secondaryGoals'
  )
  return(utils)
}

n_samples = 10#100

priorPred_sg <- map_df(1:n_samples, function(i) {
  message('run ', i)
  ##params <- priorSampleParams()
  params <- priorSampleParamsFixed()
  #utils  <- priorSampleUtils()
  utils  <- priorSampleUtilsFixedPedestrian()
  show(utils)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(utils) %>%
    cbind(run_model_secondary_goal(params, utils))
  return (out)
})

#########
# respondent has low prob card
priorUtilsFixedLowProb <- function() {
  utils <- tibble(
    'R1Context'     = 'priorContextLowProb',
    'R0PriorOverWorlds' = 'CB',
    'R1PriorOverWorlds' = 'CB',
    
  )
  return(utils)
}
# respondent has high prob card
priorUtilsFixedHighProb <- function() {
  utils <- tibble(
    'R1Context'     = 'priorContextHighProb',
    'R0PriorOverWorlds' = 'AE',
    'R1PriorOverWorlds' = 'AE'
  )
  return(utils)
}
priorPred_prior <- map_df(1:n_samples, function(i) {
  message('run ', i)
  params <- priorSampleParamsFixed()
  utils  <- priorUtilsFixedLowProb()
  show(params)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(utils) %>%
    cbind(run_model_prior(params, utils))
  return (out)
})

#write_csv(priorPred, 'priorPred.csv')
## priorPred <- read_csv('priorPred.csv')

## insert relevant df here as input ##
priorPredSummary <- priorPred_sg %>% 
  group_by(support) %>% 
  do(aida::summarize_sample_vector(.$prob)) %>% 
  select(-Parameter)


answerOrder <- c('taciturn', 'competitor', 'same category', 'other category', 'exhaustive', 'unclassified')

priorPredSummary %>% 
  mutate(
    answerType = case_when(
      support == "no.---" ~ 'taciturn',
      support == "no.competitor" ~ 'competitor',
      support == "no.competitor+sameCat" ~ 'same category',
      support == "no.competitor+sameCat+otherCat" ~ 'exhaustive',
      support == "no.otherCat" ~ 'other category'
    ) %>% factor(levels = answerOrder)
  ) %>% 
  ggplot(aes(x = answerType, fill = answerType, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = `|95%`, ymax = `95%|`), alpha = 0.3, width =0.2) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('mean prior predictive')

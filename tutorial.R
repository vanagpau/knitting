

# Code to reinstall Stan and Stan Headers
# remove.packages(c("StanHeaders", "rstan"))
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


library(brms)
library(readr)
stemcell <- read_csv("stemcell.csv")
fit_sc1 <- brm(formula = rating ~ 1 + belief, data = stemcell, family = (cumulative("probit")))


summary(fit_sc1)

conditional_effects(fit_sc1, "belief", categorical = TRUE)

# model for category specific beliefs
fit_sc2 <- brm(formula = rating ~ 1 + cs(belief), data = stemcell, family = acat("probit"))
summary(fit_sc2)
conditional_effects(fit_sc2, categorical = TRUE)

# Fit model with unequal variances
fit_sc4 <- brm(formula = bf(rating ~ 1 + belief) + lf(disc ~ 0 + belief, cmc = FALSE),
               data = stemcell, family = cumulative("probit"))
summary(fit_sc4)


# model comparison
loo(fit_sc1, fit_sc2, fit_sc4)


               
               
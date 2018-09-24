# Compute omega-squared and partial omega-squared
# By Arnoud Plantinga
# Based on http://stats.stackexchange.com/a/126520

# Functions ---------------------------------------------------------------

# Omega-squared
Omegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  msError    <- sumAov[residRow,3]
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  ssTotal    <- rep(sum(sumAov[1:residRow, 2]), 3)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- rownames(sumAov)[1:{residRow-1}]
  Omegas
}

# Partial omega-squared
partialOmegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  dfError    <- sumAov[residRow,1]
  msError    <- sumAov[residRow,3]
  nTotal     <- nrow(model.frame(aovMod))
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
  partOmegas
}

# Example -----------------------------------------------------------------

data(ToothGrowth)

lm1 <- lm(len ~ supp * dose, data=ToothGrowth)
summary(lm1)

# Eta-squared
require(lsr)
etaSquared(model3)

# Omega-squared
Omegas(model3)
partialOmegas(model3)

require(mstate)
require(mvtnorm)
require(etm)

rweib <- function(n, a, b)
{
  return((-log(runif(n))/a)^(1/b))
}

rcweib <- function(n, a, b, s)
{
  return((s^b - log(runif(n))/a)^(1/b))
}

# Alternative version with (exponential) censoring applied every time a new state is visited
sim1 <- function(current, msmodel)
{
  # Simulates one round from msmodel, starting from current
  #
  # Arguments:
  #   current: list containing
  #   - n: sample size
  #   - ids: vector of length n containing ids
  #   - time: vector of length n containing times of last transition (or 0)
  #   - state: vector of length n containing states of last transition
  #   - Z: vector of length n containing values of frailties
  #   msmodel: list containing
  #   - trans: transition matrix (S x S) (S = no of states)
  #   - shape: vector of length S containing shape parameters for each starting state
  #   - rate: vector of length K (no of transitions) containing (baseline)
  #           rate parameters for each transition
  #   - censrate: vector of length S containing (Weibull) rate of censoring per sate
  #   - censshape: vector of length S containing (Weibull) shape of censoring per sate
  #   - clock: character string, one of "reset" or "forward"
  #   - frailty: list (if missing no frailty included) containing
  #      - phi: vector of length K containing power of frailty (element=0 means
  #             no frailty effect)
  #      - variance: variance of gamma frailty (mean=1)
  # 
  # Output:
  #   list containing
  #   - data: new transition times and states in etm format (99=censoring)
  #   - cont: list with same items as current for input in next sim1
  #   - msmodel: copy of msmodel for input in next sim1

  # Shorter names for variables
  n <- current$n
  ids <- current$ids
  ctime <- current$time
  cstate <- current$state
  x <- current$x
  Z <- current$Z
  tmat <- msmodel$trans
  shape <- msmodel$shape
  rate <- msmodel$rate
  censrate <- msmodel$censrate
  censshape <- msmodel$censshape
  clock <- msmodel$clock
  frailty <- !is.null(msmodel$frailty)
  # Derived variables
  S <- nrow(tmat)
  K <- max(tmat, na.rm=TRUE)
  absorb <- apply(is.na(tmat), 1, all)

  # deb(n, method="cat")
  # deb(ctime)
  # deb(cstate)
  # deb(censrate)
  # deb(censshape)
  # deb(clock)
  # deb(frailty)
  # deb(rate)
  # deb(shape)
  
  # Generate transition times
  ttime <- rep(NA, n)
  nstate <- rep(NA, n)
  for (s in 1:S) {
    # deb(s, method="cat")
    if (!absorb[s]) {
      whs <- which(cstate == s)
      ns <- length(whs)
      # Create matrix with new time points
      tmats <- tmat[s, ]
      sstates <- as.numeric(which(!is.na(tmats))) # target states reachable from s
      strans <- as.numeric(tmats[sstates]) # corresponding transitions
      sK <- length(sstates)
      ttimes <- matrix(NA, ns, sK)
      for (k in 1:sK) {
        Zs <- Z[whs, strans[k]]
        rates <- rate[strans[k]]
        shapes <- shape[strans[k]]
        if (clock == "reset")
          ttimes[, k] <- rweib(ns, Zs * rates, shapes)
        else if (clock == "forward")
          ttimes[, k] <- rcweib(ns, Zs * rates, shapes, ctime[whs])
      }
      # deb(ttimes)
      ttime[whs] <- apply(ttimes, 1, min)
      nstate[whs] <- sstates[apply(ttimes, 1, which.min)]
    }
  }
  ntime <- ttime
  if (clock == "reset") ntime <- ntime + ctime
  censtime <- rcweib(n, censrate[cstate], censshape[cstate], ctime)
  
  # deb(ctime)
  # deb(ntime)
  # deb(nstate)
  # deb(censtime)

  # Apply censoring
  status <- as.numeric(ntime <= censtime)
  nstate99 <- nstate
  nstate99[status==0] <- 99
  # Prepare output
  dfr <- data.frame(id=ids, from=cstate, to=nstate99, entry=ctime,
                    exit=pmin(ntime, censtime))
  whcont <- which(status==1 & !absorb[nstate]) # those are the subjects that continue
  cont <- list(
    n = length(whcont),
    ids = ids[as.numeric(whcont)],
    time = ntime[whcont],
    state = nstate[whcont],
    Z = Z[whcont, , drop=FALSE]
  )
  
  # deb(cont)
  
  return(list(data=dfr, cont=cont, msmodel=msmodel))
}

simmstate <- function(n, msmodel, startprobs)
{
  # Simulate n trajectories from msmodel
  #
  # Arguments:
  #   n: sample size
  #   msmodel: list containing
  #   - trans: transition matrix (S x S) (S = no of states)
  #   - shape: vector of length S containing shape parameters for each starting state
  #   - rate: vector of length K (no of transitions) containing (baseline) rate
  #           parameters for each transition
  #   - censrate: vector of length S containing (exponential) rate of censoring per sate
  #   - clock: character string, either "reset" or "forward"
  #   - frailty: if missing, no frailty, otherwise list with
  #     - mean: K vector
  #     - sigma: covariance matrix of normal distribution
  #     frailty will exponent of multivariate normal with mean and sigma
  #   startprobs: vector of length S containing starting probabilities
  #   
  # Output:
  #   data containing transition times and states in etm format (99=censoring)

  ids <- 1:n
  # Shorter names for variables
  tmat <- msmodel$trans
  # Derived variables
  S <- nrow(tmat)
  K <- max(tmat, na.rm=TRUE)
  # Sample starting states
  cstate <- sample(1:S, size=n, replace=TRUE, prob=startprobs)
  # Sample frailty values
  if (is.null(msmodel$frailty)) 
    Z <- matrix(1, n, K)
  else 
    Z <- exp(rmvnorm(n, mean=msmodel$frailty$mean, sigma=msmodel$frailty$sigma))
    # Z <- rgamma(n, shape=0.5/fvar, rate=1/fvar) # !!!!!!!!!!!!!!!! temporarily, to comply with Gunnes !!!!
  current <- list(
    n = n,
    ids = ids,
    time = rep(0, n),
    state = cstate,
    Z = Z
  )

  dfr <- NULL
  s1 <- sim1(current=current, msmodel=msmodel)
  dfr <- s1$data
  while (s1$cont$n > 0) {
    s1 <- sim1(current=s1$cont, msmodel=s1$msmodel)
    dfr <- rbind(dfr, s1$data)
  }
  dfr <- dfr[order(dfr$id, dfr$entry), ]
  dfr
}
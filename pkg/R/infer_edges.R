##### This file is the main file of therese

# calculate the number of edges from a matrix (loops removed)
nb.edges <- function(x) {
  edges <- sum(abs(x)>0)    
  edges <- (edges - sum(abs(diag(x))>0))/2
  edges
}

# calculate penalized log-Likelihood and its differential
LL <- function(beta, S, s, Lambda) {
  L <- 0.5*t(beta) %*% S %*% beta + t(beta) %*% s + sum(Lambda*abs(beta))
  L
}

differentialLL <- function(beta, S, s, Lambda) {
  dL <- S %*% beta + s
  zero <- beta==0
  # terms for which beta is different from zero
  dL[!zero] <- dL[!zero] + Lambda[!zero] * sign(beta[!zero])
  # terms for which beta is zero
  can.be.null <- abs(dL[zero]) < Lambda[zero]
  # which ones can be null?
  dL[zero][can.be.null] <- 0
  dL[zero][!can.be.null] <- dL[zero][!can.be.null] -
    Lambda[zero][!can.be.null]*sign(dL[zero][!can.be.null])  
  dL
}

# Inferrence for one variable (all tasks together)
cLasso <- function(S, s, rho, T, beta, max.size=ncol(S), max.it) {
  
  # Initialization
  p <- ncol(S)
  sigma <- abs(beta) > 0
  theta <- sign(beta)
  it <- 0
  abs.dL.min <- abs(differentialLL(beta, S, s, rho))
  eps <- 1e-8

  if (all(abs.dL.min < eps) | sum(sigma) >= max.size) {
    return(list(beta=beta,converged=TRUE))
  } else {
    l <- which.max(abs.dL.min)
    nabla.f <- S %*% beta + s
    sigma[l] <- TRUE
    theta[l] <- -sign(nabla.f[l])
  }
  
  # Optimization
  repeat {
    it <- it+1
    sign.feasible <- FALSE
    
    while (!sign.feasible) {
      # Optimization over the active set
      h <- rep(0,p)
      x <- try(solve(S[sigma,sigma],cbind(rho*theta+s)[sigma]), silent=TRUE)
      if (is.vector(x)) {
        h[sigma] <- - beta[sigma] - x 
      } else {
        out <- optim(beta[sigma], method="BFGS", fn=LL, gr=differentialLL,
                     S=S[sigma,sigma], s=s[sigma], Lambda=rho[sigma])
        h[sigma] <- out$par-beta[sigma]
      }
      
      # Update active set
      sign.feasible <- all(sign(beta+h)[sigma]==theta[sigma])
      if (!sign.feasible) {
        ind <- which(sign(beta+h)[sigma]!=theta[sigma])
        gamma <- -beta[sigma][ind]/h[sigma][ind]
        gamma[is.nan(gamma)] <- Inf
        k <- which(gamma == min(gamma))
        if (gamma[k[1]] == Inf) {
          return(list(beta=beta,converged=FALSE))
        }
        beta <- beta + gamma[k[1]]*h
        sigma[sigma][ind][k] <- FALSE
      } else {
        beta <- beta+h
      }
      theta[sigma] <- sign(beta)[sigma]
    }
    
    abs.dL.min <- abs(differentialLL(beta,S,s,rho))
    if (all(abs.dL.min[!sigma] < eps) | it > max.it | sum(sigma) >= max.size) {
      if (it > max.it & any(abs.dL.min[!sigma] > eps)) {
        return(list(beta=beta,converged=FALSE))
      } else {
        return(list(beta=beta,converged=TRUE))
      }
    } else {
      l <- which.max(abs.dL.min)
      nabla.f <- S %*% beta + s
      sigma[l] <- TRUE
      theta[l] <- -sign(nabla.f[l])
    }
  }
}

# Inferrence for one penalty (loop over variables inside)
infer.edges <- function(X, tasks, penalty, options) {
	# Initialization
	n <- nrow(X)
	p <- ncol(X)
	Rho <- matrix(penalty, p, p)
	T  <- nlevels(tasks)
	nt <- table(tasks)
	U <- lapply(as.list(nt), function(x) -diag(rep(x/n,p-1)))
	Ulig <- Reduce(cbind,U)
	U <- NULL
	for (t in 1:T) U <- rbind(U,Ulig)
	# define matrix 'A' (averaged consensus)
	if (options$ctype=="average") {
	  A <- U
	  for (t in 1:T) {
	    current.block <- ((t-1)*(p-1)+1):(t*(p-1))
	    A[current.block,current.block] <- diag(1,p-1)+A[current.block,current.block]
	    A[current.block,] <- A[current.block,] * sqrt(nt[t])
	  }
	}  
	if (options$ctype=="fixed") {
	  clean.fixed <- matrix(options$fixed.cons[lower.tri(options$fixed.cons)|
                                               upper.tri(options$fixed.cons)],
	                        nrow=nrow(options$fixed.cons)-1)
	  priorB <- NULL
	  for (t in 1:T) {
	    priorB <- rbind(priorB,clean.fixed)
	  }
	}
  
	if (is.null(options$initial.guess)) {
	  Beta <- matrix(0,(p-1)*T, p)
	} else Beta <- options$initial.guess
  
	S.t <- by(X,tasks,var,na.rm=TRUE) 
	
	# Variable loop: build one LM for each variable
	for (k in 1:p) {

	  Lambda <- cbind(rep(Rho[-k,k],T)*rep(max(nt)/nt,rep(p-1,T)))
	  C.11 <- matrix(0,(p-1)*T,(p-1)*T)
	  C.12 <- NULL
	  for (t in 1:T) {
	    current.block <- ((t-1)*(p-1)+1):(t*(p-1))
	    C.11[current.block,current.block] <- S.t[[t]][-k,-k]
	    C.12 <- cbind(c(C.12,S.t[[t]][-k,k]))      
	  }
    
	  if (options$ctype=="average") {
	    C.11 <- C.11 + options$mu^2 * t(A)%*%A
	  } else if (options$ctype=="fixed") {
      C.11 <- C.11 + options$mu^2*diag(rep(1,(p-1)*T))
	  }
	  
	  if (options$ctype=="fixed")
	    C.12 <- C.12-options$mu^2*priorB[,k]
	  
	  ## Function that performs the inference for one variable
	  results <- cLasso(C.11, C.12, Lambda, T, beta=Beta[,k], max.size=min(n,p*T),
	                    max.it=options$max.it)
        
	  if (results$converged) {
	    Beta[,k] <- results$beta
	  } else  {
	    cat("out of convergence... stopping here")
	    return(list(Theta=NA, Beta=NA, n.edges=NA, loglik=NA, loglik.pen=NA,
                  BIC=NA, AIC=NA))
	  }
    
	  # Gathering all outputs together
	  Theta     <- list()
	  Theta.and <- list()
	  Theta.or  <- list()
	  loglik    <- 0
	  loglik.pen <- 0
	  total.df       <- 0
	  AIC       <- 0
	  
	  # Calculate consensus
	  cons.beta <- matrix(0,ncol=p,nrow=p-1)
	  for (t in 1:T) {
	    indices <- ((t-1)*(p-1)+1):(t*(p-1))
	    cons.beta <- cons.beta + nt[t]/n*Beta[indices,]
	  }
	  
	  # Build parameters list for every task
	  for (t in 1:T) {
	    Theta.c <- matrix(0,p,p)
	    dimnames(Theta.c) <- list(colnames(X), colnames(X))
	    
	    indices <- ((t-1)*(p-1)+1):(t*(p-1))
	    for (k in 1:p) {
	      Theta.c[k,k]  <- 1/(S.t[[t]][k,k])
	      Theta.c[-k,k] <- Beta[indices,k] * Theta.c[k,k] 
	    }
	    D  <- diag(Theta.c)
	    Theta.tilde <- Theta.c %*% diag(D^(-1/2))
	    loglik.c <- (nt[t]/2)*(log(prod(D)) - 
	                             sum(t(Theta.tilde) %*% var(X)%*%Theta.tilde))
	    loglik.pen.c <- loglik.c - sum(abs(Rho * Theta.c)) - options$mu/n*
        sum(sweep((Beta[indices,]-cons.beta)^2,2,diag(S.t[[t]]),"/"))
	    df.c <- (sum(abs(Theta.c)>0)- sum(abs(D)>0))/2
	    AIC.c <- -2*loglik.c + 2*df.c
	    loglik <- loglik + loglik.c
	    loglik.pen <- loglik.pen +  loglik.pen.c
	    total.df <- total.df + df.c
	    AIC <- AIC + AIC.c
	    Theta[[t]] <- Theta.c
	    
	    # Post-symetrization with the AND/OR rules
	    Theta.and[[t]] <- sign(Theta.c) * pmin(abs(Theta.c),t(abs(Theta.c)))
	    Theta.or[[t]] <- pmax(Theta.c,t(Theta.c)) - pmax(-Theta.c,-t(Theta.c))
	    diag(Theta.or[[t]]) <- diag(Theta.or[[t]])/2
	  }
	  BIC <- -2*loglik + total.df * log(n)
	  
    if (options$symmetrization=="AND") {
	    Theta <- Theta.and
    } else Theta <- Theta.or
	  
	  # Get the number of edges inferred
	  n.edges <- sapply(Theta,nb.edges)
    partial.cor <- list()
    for (indt in 1:length(Theta)) {
	    partial.cor[[indt]] <- - sweep(sweep(Theta[[indt]], 1,
                                           sqrt(diag(Theta[[indt]])), "/"), 2,
                                     sqrt(diag(Theta[[indt]])), "/")
      diag(partial.cor[[indt]]) <- 1
    }
	}
	return(list(Theta=Theta, partial.cor=partial.cor, Beta=Beta, n.edges=n.edges,
              loglik=loglik, loglik.pen=loglik.pen, BIC=BIC, AIC=AIC))
}

## Tests
# X <- matrix(rnorm(100),ncol=5)
# tasks <- factor(c(rep(1,10), rep(2,10)))
# options <- set.options()
# X <- Reduce(rbind,by(X,tasks,scale.default,TRUE,options$scale))
# penalty <- 0.1
# options$mu <- 0.5
# out <- infer.edges(X, tasks, penalty, options)
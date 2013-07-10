##### This file is the main file of therese
# it contains the function that allows you to infer networks
# it is widely inspired from the code of
# Julien Chiquet's (former?) R package 'simone'

build.therese <- function(expr, conditions, ...) {
  # Initialize options
  args <- list(...)
  options <- do.call("set.options",args)
  
	# Centering (always) and normalizing if required (TRUE by default)
  if (!is.factor(conditions)) conditions <- factor(conditions)
	X <- Reduce(rbind,by(expr,conditions,scale.default,TRUE,options$scale))
  
  # 
  if (is.null(options$max.it)) 
    options$max.it <- 10*min(c(nrow(expr),
                               ncol(expr)*nlevels(factor(conditions))))
 
	# Setting the mu penalty if not provided
	r.max <- max(sapply(sapply(by(X,conditions,var),abs),max))
	if (is.null(options$mu)) options$mu <- 0.5 * r.max
  
  # Stetting the lambda penalty if not provided
	if (is.null(options$penalty.max)) options$penalty.max <- r.max
	options$penalty.max <- min(options$penalty.max,r.max)
	
	if (is.null(options$penalties)) {
		options$penalties <- seq(options$penalty.max, options$penalty.min,
                             length=options$n.penalties)
		keep.all <- FALSE
	} else keep.all <- TRUE

	options$penalties[options$penalties < options$penalty.min] <-
    options$penalty.min
	options$penalties[options$penalties > options$penalty.max] <-
    options$penalty.max
  
	if (options$verbose) cat("\nNetwork Inference by therese... \n")
  print(options)
  cat("\n\n")
  
	networks <- list()
  betas    <- list()
  pcors    <- list()
	BIC      <- c()
	AIC      <- c()
	lambdas     <- c()
	n.edges  <- c()
	loglik <- c()
	loglik.pen <- c()
	last.edges <- rep(-1,nlevels(conditions))
	last.crit  <- -Inf
	if (options$verbose) cat(format(c("|  penalty","|    edges","| BIC"),
                                  width=10, justify="right"),"\n\n")

  # Loop on penalties...
	for (lambda in options$penalties) {
    ## The main inference function is called here...
		out <- infer.edges(X, conditions, lambda, options)
		if (sum(is.na(out$n.edges))) break 
		if (sum(out$n.edges  - last.edges) < 0) break
		if (!keep.all) {
			if (all(out$n.edges == last.edges)) {
				lambdas[length(lambdas)] <- lambda
				next
			}
		}

		# Gather the results together
		options$initial.guess <- out$Beta
		BIC  <- c(BIC,out$BIC)
		AIC  <- c(AIC,out$AIC)
		loglik  <- c(loglik,out$loglik)
		loglik.pen  <- c(loglik.pen,out$loglik.pen)
		lambdas <- c(lambdas,lambda)
		last.edges <- out$n.edges
		n.edges    <- rbind(n.edges,out$n.edges)
		last.crit  <- out$loglik.pen
		networks[[length(networks)+1]] <- out$Theta
    pcors[[length(networks)]] <- out$partial.cor
    betas[[length(networks)]] <- list()
    for (cur.conditions in 1:length(unique(conditions))) {
      cur.ind <- ((cur.conditions-1)*(ncol(out$Theta[[1]])-1)+1):
        (cur.conditions*(ncol(out$Theta[[1]])-1))
      betas[[length(networks)]][[cur.conditions]] <- out$Beta[cur.ind,]
    }
    
		if (options$verbose) {
			cat(format(list(lambda,paste(last.edges,collapse=","),out$BIC), width=10,
                 digits=4, justify="right"),"\n")
		}
		if (max(last.edges) > options$edges.max) break
	}

	# Return results
	res <- structure(list(data=expr, conditions=conditions, networks=networks,
                        betas=betas, pcors=pcors, 
                        used.lambdas=as.vector(lambdas),
                        n.edges=as.matrix(n.edges), BIC=as.vector(BIC),
	                      AIC=as.vector(AIC), loglik=as.vector(loglik),
	                      loglik.pen=as.vector(loglik.pen), options=options),
	                 class="therese")
  if (options$verbose) cat("\n\n"); summary(res)
  return(res)
}

##### S3 method for 'therese'
print.therese <- function(x,...) {
  cat("Therese object inferred for", nlevels(x$conditions), "conditions and",
      ncol(x$data),"variables observed for",nrow(x$data),"individuals.\n\n")
}

summary.therese <- function(object,...) {
  cat("Therese object inferred for", nlevels(object$conditions),
      "conditions and", ncol(object$data), "variables observed for",
      nrow(object$data), "individuals.\n\n")
  p <- ncol(object$data)
  
  print(object$options)

  cat("\n***** Results obtained by therese\n\n")
  cat("   ", length(object$networks), "networks inferred for",
      nlevels(object$conditions), " conditions.\n")
  cat("    Best BIC for networks number", which.min(object$BIC), "with",
      object$n.edges[which.min(object$BIC),],"edges (densities:",
      format(2*object$n.edges[which.min(object$BIC),]/p/(p-1),digits=2), ").\n")
  cat("    Best AIC for networks number", which.min(object$AIC), "with",
      object$n.edges[which.min(object$AIC),],"edges (densities:",
      format(2*object$n.edges[which.min(object$AIC),]/p/(p-1),digits=2), ").\n")
  cat("    Best penalized log-likelihood for networks number",
      which.max(object$loglik.pen), "with",
      object$n.edges[which.max(object$loglik.pen),], "edges. (densities:",
      format(2*object$n.edges[which.max(object$loglik.pen),]/p/(p-1), digits=2),
      ").\n")
}

## Tests
# X <- matrix(rnorm(100),ncol=5)
# conditions <- factor(c(rep(1,10), rep(2,10)))
# res <- build.therese(X, conditions, mu=0.1)
# print(res)
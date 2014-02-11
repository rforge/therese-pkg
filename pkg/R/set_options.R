##### Handling options

set.options <- function(ctype=c("average","fixed"), scale=TRUE, verbose=TRUE,
                        mu=NULL, fixed.cons=NULL, penalties=NULL,
                        penalty.min=1e-2, penalty.max=NULL, n.penalties=100,
                        edges.max=Inf, symmetrization=c("AND","OR"),
                        initial.guess=NULL, max.it=NULL) {
  
  ## TODO: add tests to check if the options are consistant
  ## TODO: add weight.task
  ctype <- match.arg(ctype)
  
  if (!is.null(penalties)) penalties <- sort(penalties, decreasing=TRUE)
  penalty.min <- max(penalty.min,.Machine$double.eps)
  
  if ((ctype=="average")&(!is.null(fixed.cons))) {
    ctype <- "fixed"
    warning("'ctype' switched to 'fixed' as 'fixed.cons' is not null!")
  }

  if ((ctype=="fixed")&(is.null(fixed.cons)))
    stop("When 'ctype==fixed', 'fixed.cons' must be supplied!")
      
	all.options <- list(ctype=ctype, scale=scale, verbose=verbose, mu=mu,
                      fixed.cons=fixed.cons, penalties=penalties,
                      penalty.min=penalty.min, penalty.max=penalty.max,
                      n.penalties=n.penalties,
                      symmetrization=match.arg(symmetrization),
                      edges.max=edges.max, initial.guess=initial.guess, 
                      max.it=max.it)
  
  class(all.options) <- "thereseOptions"
  return(all.options)
}

## Test
# set.options()


##### S3 method for thereseOptions

print.thereseOptions <- function(x,...) {
  cat("\n***** Parameters for therese\n\n")
  cat("    consensus type                 :", x$ctype, "\n")
  cat("    scaling                        : ", x$scale, "\n")
  if (!is.null(x$mu))
    cat("    mu                             : ", x$mu, "\n")
    cat("    number of penalties            : ", x$n.penalties, "\n")
    if (!is.null(x$penalty.min)&&!is.null(x$penalty.max))
      cat("    with min/max                   : ", x$penalty.min, "-",
          x$penalty.max, "\n")
  if (!(x$edges.max==Inf))
    cat("    maximum number of edges        : ", x$edges.max, "\n")
  if (x$symmetrization=="AND")
    cat("    symmetrization rule            :  AND\n")
  else cat("    symmetrization rule            :  OR\n")
}

## Test
# print.thereseOptions(set.options())
data(cancer)

# inference by therese
res.therese <- build.therese(expr, conditions, mu=1)
therese.nets <- therese2igraph(res.therese, "BIC")
summary(therese.nets)
plot(therese.nets)

# ... which can be compared to independent inference
res.indep <- build.therese(expr, conditions, mu=0)
therese.indep <- therese2igraph(res.indep, "BIC")
summary(therese.indep)
plot(therese.indep)

# ... and with a fixed consensus 
# in this case, it is the intersection of independent infe
prior.intersection <- graph.intersection(therese.indep[[1]],therese.indep[[2]])
prior.intersection <- get.adjacency(prior.intersection, sparse=FALSE)
res.fixed <- build.therese(expr, conditions, mu=1, ctype="fixed",
                           fixed.cons=prior.intersection)
therese.fixed <- therese2igraph(res.fixed, "BIC")
summary(therese.fixed)
plot(therese.fixed)
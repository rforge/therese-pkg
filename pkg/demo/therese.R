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
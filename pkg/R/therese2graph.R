##### This file contains script to convert therese object into igraphs

# build a network for one partial correlation matrix
pcor2graph <- function(pcors) {
  if (is.null(colnames(pcors))) 
    colnames(pcors) <- paste("n",1:ncol(pcors),sep="")
  
  pcors.bin <- (pcors-diag(diag(pcors)))!=0
  therese.graph <- graph.adjacency(pcors.bin, mode="undirected")
  pcors <- pcors[upper.tri(pcors)]
  pcors.bin <- pcors.bin[upper.tri(pcors.bin)]
  E(therese.graph)$pcor <- pcors[pcors.bin]
  
  therese.graph
}

# build a network from therese results
therese2igraph <- function(therese.res, choice=c("BIC","AIC","loglik","last"),
                           ind=NULL) {
  choice <- match.arg(choice)
  
	if (is.null(ind)) {
		ind <- switch(choice,
			"BIC"=which.min(therese.res$BIC),
			"AIC"=which.min(therese.res$AIC),
		  "loglik"=which.max(therese.res$"loglik.pen"),
			"last"=length(therese.res$BIC)
		)
	}

	pcors.list <- therese.res$pcors[[ind]]
	therese.net.list <- lapply(pcors.list, pcor2graph)
  names(therese.net.list) <- levels(therese.res$conditions)
	class(therese.net.list) <- "thereseNet"
  therese.net.list
}

##### S3 method for thereseOptions

print.thereseNet <- function(x,...) {
  cat("\n***** Networks inferred by therese\n")
  cat(length(x),"networks.\n")
  lapply(x, print)
}

summary.thereseNet <- function(object, ...) {
  print(object)
  cat("\n Densities:", format(unlist(lapply(object, graph.density)), digits=2))
  common.edges <- matrix(0,ncol=length(object),nrow=length(object))
  for (ind1 in 1:length(object)) {
    for (ind2 in 1:length(object)) {
      common.edges[ind1,ind2] <- ecount(graph.intersection(object[[ind1]],
                                                           object[[ind2]]))
      if (ind1!=ind2) common.edges[ind2,ind1] <- common.edges[ind1,ind2]
    }
  }
  rownames(common.edges) <- paste("cond",1:length(object),sep=".")
  colnames(common.edges) <- rownames(common.edges)
  cat("\n\n Number of common edges between conditions:\n")
  print(common.edges)
}

plot.thereseNet <- function(x, ..., best.layout="indiv") {
  args <- list(...)
  
  common.edges <- get.edgelist(x[[1]])
  common.edges <- paste(common.edges[,1],common.edges[,2])
  
  for (ind in 2:length(x)) {
    cur.el <- get.edgelist(x[[ind]])
    cur.el <- paste(cur.el[,1],cur.el[,2])
    common.edges <- intersect(common.edges,cur.el)
  }
  
  # layout
  ## TODO: improve to make it possible to choose the layout type
  if (best.layout=="indiv") {
    the.layout <- lapply(x, layout.auto) 
  } else if (!best.layout%in%(1:length(x))) {
    stop("'best.layout' must be 'indiv' or a number indicating the condition",
         .call=TRUE)
  } else
    the.layout <- layout.auto(x[[best.layout]])
  
  par(mar=rep(0,4))
  layout(matrix(1:length(x),ncol=length(x)))
  invisible(sapply(1:length(x), function(ind) {
    net <- x[[ind]]
    
    # common edges and color
    cur.el <- get.edgelist(net)
    cur.el <- paste(cur.el[,1],cur.el[,2])
    if (is.null(args$edge.color)) {
      ecol <- rep("pink",length(common.edges))
      ecol[match(common.edges,cur.el)] <- "lightblue"
    } else {
      if (length(args$edge.color)!=2)
        stop("You must provide 2 edge colors!", .call=TRUE)
      ecol <- rep(args$edge.color[1],E(common.edges))
      ecol[match(common.edges,cur.el)] <- args$edge.color[2]
    }
    args$edge.color <- ecol
    
    # default options
    if (is.null(args$vertex.size)) args$vertex.size=0
    if (is.null(args$edge.width)) args$edge.width <- sqrt(abs(E(net)$pcor))/
      max(sqrt(abs(E(net)$pcor)))*2
    if (is.null(args$edge.lty)) {
      args$edge.lty <- rep(1,ecount(net))
      args$edge.lty[E(net)$pcor<0] <- 2
    }
    if (is.null(args$vertex.label.color)) args$vertex.label.color <- "black"
    
    if (best.layout=="indiv") {
      args$layout <- the.layout[[ind]]
    } else args$layout <- the.layout
    
    args$x <- net
    do.call("plot",args)
  }))
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(5, 4, 4, 2)+0.1)
}
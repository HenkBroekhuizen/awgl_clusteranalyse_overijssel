ggd_removeCorrelatedColumns <- function(
    data,
    threshold = .8,
    always.keep = NA, # column names
    centrality.direction = 1, # 1=remove low centrality first, -1=high first
    variance.weight = .1
){
  # required packages
  pkg_req = c("igraph", "tidyverse", "corrplot")
  for (pkg in pkg_req) {
    if (system.file(package = pkg) == "") {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  # init
  simplify_igraph <- igraph::simplify

  iter = 1
  force_stop = F
  
  # exclude non-numeric columns with cat warnings
  non_numeric_columns <- !sapply(data, is.numeric)
  if(any(non_numeric_columns)){
    cat("   excluding these columns because they are not numeric: ", names(which(non_numeric_columns)), "\n")
    data <- data %>% select(-all_of(names(which(non_numeric_columns))))
  }

  # calculate variances and correlation matrix
  variances <- apply(data, 2, var, na.rm=T)
  correlation_matrix <- abs(cor(data, use = "na.or.complete"))
  
  if(!is.na(always.keep)){
      always_keep_rows_columns <- grepl(paste(always.keep, collapse = "|"), colnames(correlation_matrix))
  correlation_matrix[, always_keep_rows_columns] <- 0
  correlation_matrix[always_keep_rows_columns, ] <- 0
  }
  diag(correlation_matrix) <- 0
  plot(correlation_matrix %>% as.numeric %>% density)
  View(correlation_matrix)
  # init output
  to_remove <- rep(F, ncol(data))

  # build graph from correlation matrix
  # columns are nodes
  # edges are correlations>threshold
  g <- graph_from_data_frame(
    d = which(correlation_matrix > threshold, arr.ind = T),
    directed = F
  ) %>% simplify_igraph
  V(g)$name <- colnames(data)[V(g)$name %>% as.numeric]
  # run through the graph
  while(ecount(g) > 0 & !force_stop){
    # remove (without consequences for final column selection)
    # all nodes that have no connections left
    g <- delete_vertices(g, degree(g) < 1)
    # calculate metrics for each node
    metrics <- data.frame(
      node_degree = degree(g),
      centrality = percent_rank(centr_betw(g)$res),
      variance = percent_rank(variances[V(g)$name]),
      always_keep = V(g)$name %in% always.keep
    )
    metrics <- metrics %>%
      mutate(
        score = 
          (1 + centrality) * centrality.direction * 
          variance.weight * variance
      ) %>% 
      mutate(
        score = case_when(
          always_keep ~ NA,
          T ~ score
        )
      )
    print(metrics %>% arrange(., score))
    # remove node with least favorable score
    # this is a column that will be removed
    local_to_remove <- which.min(metrics$score)
    if(length(local_to_remove) > 0){
      message("removing ", rownames(metrics)[local_to_remove])
      to_remove[which(colnames(data) == rownames(metrics)[local_to_remove])] <- T
      plot(g, 
           vertex.label.cex = 0.75,# + percent_rank(metrics$variance),  # Small labels
           vertex.size = 10,        # Small vertex size
           vertex.shape = c("square", "circle")[1 + (metrics$score == min(metrics$score))],
           vertex.color = heat.colors(10)[round(percent_rank(metrics$score)*10)],
           edge.arrow.size = 0,     # No arrows
           edge.arrow.width = 0,
           vertex.label.dist = 1.5,  # Distance of the label from the vertex
           main = iter
      )
      g <- delete_vertices(g, rownames(metrics)[local_to_remove])
    } else {
      force_stop <- T
    }
    iter <- iter + 1
  }
  #corrplot(correlation_matrix[!to_remove, !to_remove], method="number",type="upper")
  
  return(data[, !to_remove]) 
}
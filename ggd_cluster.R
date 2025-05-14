ggd_cluster <- function(
    data,
    method.in, #kproto, kmeans, hierarch
    correlation.threshold = 1,
    correlation.keep = "",
    impute.threshold = 0,
    dont.cluster.on = "",
    method.pars = list(
      n.clusters = 2,
      hierarchical.distance.metric = "gower",
      hierarchical.agglomeration.method = "complete",
      kproto.lambda = .1
    ),
    cluster.quality.criterion = "Silhouette",
    strict.quality.check = F,
    calculate.importance = F,
    return.mode = 2, #1=data+clusters, 2=list(data+clusters, model), 3=clusters, 0=nothing
    do.report = T
){
  # startup message
  cat("Starting cluster analysis\n")
  
  #### Install and load needed packages if not installed yet ####
  pkg_req = c("haven", "tidyverse", "survey","clustMixType", "RColorBrewer", "labelled", "scales", "FeatureImpCluster", "cluster",
              "clusterCrit", "kernlab", "FactoMineR", "factoextra", "fpc")
  for (pkg in pkg_req) {
    if (system.file(package = pkg) == "") {
      message("Package '", pkg, "' not installed in ", .libPaths()[1],". Attempting install...")
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  # load required functions
  source("ggd_randomImpute.R")
  source("ggd_unHaven.R")
  source("ggd_removeCorrelatedColumns.R")
  
  #### Checks on inputs ####
  # clustering method
  # specify clustering method functions: in(data, hyperparameters), out(clusters)
  if(method.in == "hierarchical"){
    clustering.function <- function(data, hyperparameters){
      model <- data %>%
        daisy(metric = hyperparameters$hierarchical.distance.metric) %>% 
        hclust(method = hyperparameters$hierarchical.agglomeration.method)
      return(list(model = model, clusters = model %>% cutree(k = hyperparameters$n.clusters)))
    }
  } else if(method.in == "kmeans"){
    clustering.function <- function(data, hyperparameters){
      model <- kmeans(
        x = data,
        centers = method.pars$n.clusters
      )
      return(list(model = model, clusters = model$cluster %>% unname))
    }
  } else if(method.in == "kproto"){
    clustering.function <- function(data, hyperparameters){
      model <- kproto(
        x = data, 
        k = hyperparameters$n.clusters,
        lambda = hyperparameters$kproto.lambda,
        type = hyperparameters$kproto.type,
        verbose = F
      )
      return(list(model = model, clusters = model$cluster %>% unname))
    }
  } else if(method.in == "spectral"){
    clustering.function <- function(data, hyperparameters){
      model <- specc(
      ~.,
      kernel=hyperparameters$kernel,
      centers = hyperparameters$n.clusters,
      nystrom.red = hyperparameters$nystrom.red,
      mod.sample = hyperparameters$mod.sample,
      data = data
      )
      pca <- PCA(model.matrix(~. -1, data))
      return(list(model = model, clusters = model@.Data, pca=pca))
    }
  } else if(method.in == "dbscan"){
    clustering.function <- function(data, hyperparameters){
      model <- dbscan(
        data,
        eps = hyperparameters$eps,
        MinPts = hyperparameters$MinPts,
        method = hyperparameters$method,
        showplot = 2
      )
      return(list(model = model, clusters = model$cluster))
    }
  } else {
    stop("Unknown clustering method '",method.in, "'. Supported options are 'hierarchical', 'kmeans', 'kproto', and 'dbscan")
  }
  
  #### Data cleaning ####
  # impute missing values, then remove cases with remaining NA
  data <- data %>% 
    ggd_RandomImpute(names(which(colSums(apply(data, 2, is.na)) > impute.threshold)), onbekend_values = 99) %>%
    na.omit
  # remove too highly correlated columns
  if(correlation.threshold < 1){
    data <- data %>% ggd_removeCorrelatedColumns(threshold = correlation.threshold, always.keep = correlation.keep)
  }
  # unhaven so clustering functions can use conventional df's
  data_havened <- data
  data <- data %>% ggd_unHaven
  
  #### Clustering ####
  hyperparameters <- do.call(expand.grid, list(method.pars, stringsAsFactors = F))
  models <- vector(mode="list", length = nrow(hyperparameters))
  clusters <- vector(mode="list", length = nrow(hyperparameters))
  cat("Starting cluster analysis ...", nrow(hyperparameters), "analyses will be done.\n")
  for(i in 1:nrow(hyperparameters)){
    clustering_result <- clustering.function(data %>% select(-any_of(dont.cluster.on)), hyperparameters[i, ])
    models[[i]] <- clustering_result$model
    clusters[[i]] <- clustering_result$clusters
      hyperparameters$performance[i] <- intCriteria(
        traj = data_havened %>% select(-any_of(dont.cluster.on)) %>% as.matrix,
        part = clusters[[i]] %>% as.integer,
        crit = cluster.quality.criterion
        )[[1]]
    cat("[",i,"/",nrow(hyperparameters),"] Clustering using",toupper(method.in),
        "with parameters", paste0(colnames(hyperparameters)[-ncol(hyperparameters)],"=",
                                  unname(hyperparameters[i, -ncol(hyperparameters)])), ": performance = ", 
        hyperparameters$performance[i], "\n")
  }
  hyperparameters$performance[is.na(hyperparameters$performance) | is.nan(hyperparameters$performance)] <- -999
  best_hyperparameter_set <- bestCriterion(hyperparameters$performance, cluster.quality.criterion)
  best_model <- models[[best_hyperparameter_set]]
  calculated_clusters <- clusters[[best_hyperparameter_set]]
  
  # convert calculated clusters to labbeled for use in tabellenboeken etc
  unique_clusters <- unique(calculated_clusters)
  cluster_labels <- setNames(paste0("Cluster", unique_clusters), as.character(unique_clusters))
  calculated_clusters <- calculated_clusters %>% as.character %>% labelled(labels = cluster_labels)
  var_label(calculated_clusters) <- paste("Cluster berekend met", method.in)
  
  # plotting
  plot(
    hyperparameters$performance,
    xlab="Cluster solution", ylab=cluster.quality.criterion, type="b",ylim=c(0,1)
    )
  
  #### Return statements ####
  if(return.mode == 0){
    out <- NULL
  }
  else if(return.mode == 1)  {
    out <- data %>% add_column(cluster = calculated_clusters)
  } else if(return.mode == 2){
    hyperparameters$performance <- round(hyperparameters$performance, 2)
    out <- list(
      data = data %>% add_column(cluster = calculated_clusters),
      clusters = calculated_clusters,
      model = best_model,
      best_hyperparameter_set = hyperparameters[best_hyperparameter_set, ],
      cluster.quality.criterion = cluster.quality.criterion,
      model.type = method.in,
      all_models = models,
      all_clusters = clusters,
      hyperparameters = hyperparameters
    )
  } else if(return.mode == 3){
    out <- calculated_clusters
  } else {
    stop("Unknown return mode: ", return.mode)
  }
  if(calculate.importance & !(return.mode %in% c(1, 3))){
    cat(" calculating variable importance ...\n")
    out <- c(
      out,
      list(
        variable_importance = FeatureImpCluster(clusterObj = best_model, data = data %>% as.data.table)
      )
    )
  }
  return(out)  
}
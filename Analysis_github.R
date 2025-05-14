# title: AWGL cluster analyses script
# Author: Henk Broekhuizen
# Date: 08/04/2025
# Updated by author: Henk Broekhuizen and Floor Kerkhof
# Date updated: 08/04/2025

#### Install and load needed packages if not installed yet ####
pkg_req = c("tidyverse", "readxl", "sfdep")

for (pkg in pkg_req) {
  if (system.file(package = pkg) == "") {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

source("ggd_cluster.R")
source("ggd_removeCorrelatedColumns.R")

#### Initialize variables ####
data_path_output_analysis <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/data_with_clusters.gpkg"
data_path_output_cluster_characteristics <- "I:/Repositories/__R_CODE__/Eenmalige projecten/AWGL_project_Overijssel_2024/cluster_characteristics_new.xlsx"

#### Load data ####
# not used, assumed 'ETL script.r' was run in same session!

#### remove correlated columns ####
data_for_clustering <- data %>% 
  select(
    buurtcode,
    HuishoudensMetEenLaagInkomen_78,
    OpleidingsniveauLaag_64,
    PercentageGroen,
    voorzieningen,
    no2_median,
    pm25_median,
    sound_median,
    heat_median,
    MateVanStedelijkheid_116
  ) %>% 
  as.data.frame %>% 
  ggd_removeCorrelatedColumns

# calculate local moran's
morans <- NULL
neighbours <- st_contiguity(data, queen=F)
spatial_weights <- st_weights(neighbours)

for(i in 1:ncol(data_for_clustering)){
  if(is.numeric(data_for_clustering[[i]])){
    ldat <- data_for_clustering[[i]] %>% as.vector
    ldat[is.na(ldat)] <- 0
    local_moran_i <- sfdep::local_moran(ldat, neighbours, spatial_weights)
    local_moran_i <- local_moran_i$median %>% as.numeric * (local_moran_i$p_ii < 0.05) %>% as.data.frame
    colnames(local_moran_i) <- colnames(data_for_clustering)[i]
    morans <- morans %>% bind_cols(local_moran_i)
  } else {
    morans <- morans %>% bind_cols(rep(NA, nrow(data_for_clustering)))
  }
}
colnames(morans) <- paste0("LISA_", colnames(morans))
data <- data %>% bind_cols(morans)

#### dataset for clustering ####
data_for_clustering_LISA <- data %>% 
  select(buurtcode) %>% 
  bind_cols(morans) %>% 
  as.data.frame %>% 
  select(-geom)

#### make clusters (hierarchical) ####
cluster_hierarchical <- ggd_cluster(
  data = data_for_clustering_LISA,
  dont.cluster.on = "buurtcode",
  cluster.quality.criterion = "Silhouette",
  method.in = "hierarchical",
  method.pars = list(
    n.clusters = 2:25,
    hierarchical.distance.metric = "gower",
    hierarchical.agglomeration.method = "ward.D"
  )
)
cluster_hierarchical$all_clusters <- cluster_hierarchical$all_clusters %>% as.data.frame
colnames(cluster_hierarchical$all_clusters) <- paste0("cluster_hier_", cluster_hierarchical$hyperparameters$n.clusters)

cluster_hierarchical$data <- cluster_hierarchical$data %>% 
  rename(cluster_hier_best = cluster) %>% 
  bind_cols(cluster_hierarchical$all_clusters)

data_with_cluster <- data %>% 
  left_join(cluster_hierarchical$data %>% select(buurtcode, starts_with("cluster_hier")), join_by(buurtcode == buurtcode))

# calculate and plot principal components
pca <- PCA(data_for_clustering_LISA %>% select(-buurtcode))
pca$var$contrib %>% rowSums %>% sort(decreasing = T) %>% round

# map with main clustering solution
plot(data_with_cluster %>% select(cluster_hier_best))

# write data with clusters for QGIS mapping
st_write(data_with_cluster, data_path_output_analysis, append = F)

# summarize cluster characteristics
df_summary <- data_with_cluster %>%
  group_by(cluster_hier_best) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE)) %>%
  as.data.frame %>%
  select(-geom) %>%
  mutate(across(where(is.list), ~ sapply(., toString))) %>% 
  select(-cluster_hier_best) %>% 
  round

write.xlsx(df_summary, file = data_path_output_cluster_characteristics)
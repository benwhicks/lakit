# Explorers: Functions good for early stage analysis, often clunky combinations
# of other functions to aid in choosing settings for specifics

#sample_activty_summary <- read_csv(file.path(getwd(), "data", "sample_student_lms_summary_data.csv"))

#' heatmap_lakit
#'
#' Reworked heatmap with different defaults and easier choosing
#' of methods
#' @param x numeric matrix of the values to be plotted
#' @param cluster_method method of clustering
#' @param distance_metric method of measuring distance between two points
#' @param ... other parameters passed to heatmap
#' @keywords heatmap clustering
#' @export heatmap_lakit
heatmap_lakit <- function(x, cluster_method = "average", distance_metric = "euclidian", k = 1, ...) {
  x <- to_matrix_greedily(df)
  x <- scale(x) # hclust and heatmaps only make sense with normed data
  col_branches
  heatmap.2(x,
          distfun = function(x) dist(x, method = distance_metric),
          hclustfun = function(x) hclust(x, method = cluster_method),
          main = paste(cluster_method, "cluster method,", distance_metric, "distance metric"),
          trace = "none",
          labRow = FALSE,
          ...)
}

#' dendrogram_lakit
#'
#' Reworked dendrogram with different defaults and easier choosing
#' of methods
#' @param x numeric matrix of the values to be plotted
#' @param cluster_method method of clustering
#' @param distance_metric method of measuring distance between two points
#' @param ... other parameters passed to heatmap
#' @keywords dendrogram clustering
#' @export dendrogram_lakit
dendrogram_lakit <- function(x, cluster_method = "average", distance_metric = "euclidian", k = 1, ...) {
  x <- to_matrix_greedily(x)
  x <- scale(x) # hclust and heatmaps only make sense with normed data
  distance <- dist(x, method = distance_metric)
  hc <- hclust(distance, method = cluster_method)
  as.dendrogram(hc) %>%
    set("branches_k_color", k = k) %>%
    set("leaves_pch", 19) %>% set("leaves_cex", 0.3) %>% set("leaves_col", "black") %>%
  plot(xlab = paste("Cluster method:", cluster_method, ":: Distance metric:", distance_metric),
       sub = paste(k, "clusters highlighted"),
       leaflab = "none",
       ...)
}

# explore dendrogram replace with a vignette using:
# par(mfrow = c(2,3))
# then repeated application of dendrogram_lakit

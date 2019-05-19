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
  x <- as_matrix_greedily(df)
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
  x <- as_matrix_greedily(x)
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


#' add_edge_layer
#'
#' Adds additional edges to a graph, with a binary variable to identify the new edges.
#' Returns a graph object with edge attribute 'add' which is TRUE or FALSE.
#' The idea is that a first edge list generates the structure for
#' the nodes, and then a second edge list is plotted alongside these
#' original edges. All the nodes should be specified in a single node list
#' @param graph a tidygraph object
#' @param edges_add a df of the edges to be added to the network. Should not add additional nodes and have the same edge attributes.
#' @export add_edge_layer
#'
#' @examples
#'
#' set.seed(1)
#' nds <- data.frame(id = 1:6)
#' egs1 <- data.frame(from = 1:6, to = sample(1:6, 6, replace = TRUE))
#' egs2 <- data.frame(from = 1:4, to = sample(1:6, 4, replace = TRUE))
#'
#' # plotting base network
#' base_graph <- tbl_graph(nodes = nds, edges = egs1)
#' lyt <- create_layout(base_graph, layout = 'nicely')
#' ggraph(base_graph, layout = 'manual', node.positions = lyt) +
#'   geom_node_point() +
#'   geom_edge_link() +
#'   theme_graph()
#'
#' # plotting extra layer
#' new_graph <- add_edge_layer(base_graph, egs2)
#' ggraph(new_graph, layout = 'manual', node.positions = lyt) +
#'   geom_node_point(aes(x = lyt$x, y = lyt$y)) +
#'   geom_edge_link(aes(edge_linetype = add)) +
#'   theme_graph()
add_edge_layer <- function(graph, edges_add) {
  old_nodes <- graph %>% activate(nodes) %>% as.data.frame()
  old_edges <- graph %>% activate(edges) %>% as.data.frame()
  old_edges <- old_edges %>% mutate(add = FALSE)
  add_edges <- edges_add %>% mutate(add = TRUE)
  new_edges <- bind_rows(old_edges, add_edges)
  new_graph <- tbl_graph(nodes = old_nodes, edges = new_edges)
  return(new_graph)
}

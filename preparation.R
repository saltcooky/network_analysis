library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)

# 基本グラフ描写関数
plot_tbl_data <- function(tbl_data, title = ""){
  tbl_data %>%  
    ggraph(layout = "kk") +
    geom_edge_link(alpha=0.8, colour = "lightgray") + 
    scale_edge_width(range = c(0.1,1)) +
    geom_node_point(aes( size = 4)) +
    geom_node_label(aes(label = name),repel = TRUE) +
    ggtitle(title)
}

# 中心性の描画関数
plot_tbl_cent_data <- function(tbl_data, title = "centrality"){
  tbl_data %>%  
    ggraph(layout = "kk") +
    geom_edge_link(alpha=0.8, colour = "lightgray") + 
    scale_edge_width(range = c(0.1,1)) +
    geom_node_point(aes(size = centrality)) +
    geom_node_label(aes(label = name),repel = TRUE)+
    ggtitle(title)
}

# コミュニティー抽出用 描画関数
plot_tbl_com_data <- function(tbl_data, title){
  tbl_data %>%
    ggraph(layout = "kk") +
    geom_edge_link(alpha=0.8, colour = "lightgray") + 
    scale_edge_width(range = c(0.1,1)) +
    geom_node_point(aes(colour = nclass, size = 4)) +
    geom_node_label(aes(label = name), repel = TRUE) +
    ggtitle(title)
}


# ネットワークAのデータを作成
A <- matrix(c(
  0,1,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0, 
  1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0, 
  1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0, 
  1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0, 
  0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0, 
  1,0,0,0,1,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0, 
  0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0, 
  0,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,1,1, 
  0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1, 
  0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,
  0,1,1,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0, 
  1,0,1,0,0,0,0,1,0,0,1,0,1,1,0,0,0,1,0,0, 
  1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0, 
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0, 
  0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0, 
  0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0, 
  0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0, 
  0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,1, 
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,1, 
  0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0),
  nrow = 20, ncol = 20, byrow = TRUE)

colnames(A) <- c(1:20)
rownames(A) <- c(1:20)

# グラフデータに変換
g1 <- graph_from_adjacency_matrix(A)
g1_tbl <- as_tbl_graph(g1, direted = F)


B <- matrix(c(
  0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0, 
  1,0,0,0,0,1,0,1,0,0,1,0,1,0,0,0,0,1,0,0, 
  0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0, 
  1,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0, 
  0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0, 
  1,0,0,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,0,0, 
  0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0, 
  0,1,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,1, 
  0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1, 
  0,0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,
  0,1,1,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0, 
  1,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,1,0,0, 
  1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0, 
  0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0, 
  0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0, 
  0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0, 
  0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0, 
  0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1, 
  0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,1, 
  0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0),
  nrow = 20, ncol = 20, byrow = TRUE)

colnames(B) <- c(1:20)
rownames(B) <- c(1:20)

# グラフデータに変換
g2 <- graph_from_adjacency_matrix(B)
g2_tbl <- as_tbl_graph(g2, direted = F)


C <- matrix(c(
  0,1,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,0,1, 
  1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0, 
  1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0, 
  0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0, 
  0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0, 
  1,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0, 
  0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0, 
  0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,1,0, 
  0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,1, 
  0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,
  0,1,1,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0, 
  1,0,1,0,0,0,0,1,0,0,1,0,1,1,0,0,0,0,0,0, 
  1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0, 
  0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0, 
  0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1,0, 
  0,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0, 
  0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0, 
  0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1, 
  0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,1, 
  1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,0),
  nrow = 20, ncol = 20, byrow = TRUE)

colnames(C) <- c(1:20)
rownames(C) <- c(1:20)
# グラフデータに変換
g3 <- graph_from_adjacency_matrix(C)
g3_tbl <- as_tbl_graph(g3, direted = F)

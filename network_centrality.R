library(ggraph)
library(sna)
library(ggplot2)
library(igraph)
library(tidygraph)

source("preparation.R")

# グラフ描写
plot_tbl_data(g1_tbl)

# 密度
graph.density(g1_tbl)

# 推移性
transitivity(g1_tbl)

# 相互性
reciprocity(g1_tbl)


# 中心性 ---------------------------------------------------------------------

# 次数中心性
g1_tbl.deg <- g1_tbl %>% 
  mutate(centrality = centrality_degree())

plot_tbl_cent_data(g1_tbl.deg, "centrality:degree")

# 媒介中心性
g1_tbl.bet <- g1_tbl %>% 
  mutate(centrality = centrality_betweenness())

plot_tbl_cent_data(g1_tbl.bet, "centrality:betweenness")

# 隣接中心性 
g1_tbl.cls <- g1_tbl %>% 
  mutate(centrality = centrality_closeness())

plot_tbl_cent_data(g1_tbl.cls,"centrality:closeness")

# 固有ベクトル中心性
g1_tbl.eig <- g1_tbl %>% 
  mutate(centrality = centrality_eigen())

plot_tbl_cent_data(g1_tbl.eig, "centrality:eigen")

# page rankの算出
g1_tbl.pr <- g1_tbl %>% 
  mutate(centrality = centrality_pagerank())

plot_tbl_cent_data(g1_tbl.pr, "centrality:page rank")

# 情報中心性
g1_tbl.inf <- g1_tbl %>% 
  mutate(centrality = centrality_information())

plot_tbl_cent_data(g1_tbl.inf, "centrality:infomation")


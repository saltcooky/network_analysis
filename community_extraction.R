library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)
library(netrankr)

source("preparation.R")

# ハードクラスタリング コミュニティ検出 ----------------------------------------------------------------

#媒介中心性によるクラスタリング
g1_tbl.bet_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_edge_betweenness()))
plot_tbl_com_data(g1_tbl.bet_C,"betweenness")

#固有ベクトル中心性によるクラスタリング
g1_tbl.eig_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_leading_eigen()))
plot_tbl_com_data(g1_tbl.eig_C,"eigenvector")

#ランダムウォークによるクラスタリング
g1_tbl.walk_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_walktrap()))
plot_tbl_com_data(g1_tbl.walk_C,"random walk")

#情報中心性によるクラスタリング
g1_tbl.inf_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_infomap()))
plot_tbl_com_data(g1_tbl.inf_C,"infomap")

#スピングラス法によるクラスタリング
g1_tbl.spin_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_spinglass()))
plot_tbl_com_data(g1_tbl.spin_C,"spin glass")

#グリーディアルゴリズムによるクラスタリング
g1_tbl.gr_C <- g1_tbl %>% 
  mutate(nclass = as.factor(group_fast_greedy()))
plot_tbl_com_data(g1_tbl.gr_C)

# デンドログラムを作ってみる
g1_tbl %>% 
  leading.eigenvector.community(.) %>% 
  as.dendrogram() %>% 
  plot

# ソフトクラスタリング コミュニティ検出 ----------------------------------------------------------------
library(linkcomm)
library(reshape2)

g1 %>% 
  get.edgelist() %>% 
  getOCG.clusters %>% 
  plot(type="graph",margin=-0.0)

# リンクコミュニティ
g1.com <- g1 %>% 
  get.edgelist %>% 
  getLinkCommunities 

plot(g1.com, type="graph",margin=1.0)

# ノードの相関を求める
library(sna)

graph_cor <- sedist(A, method = "correlation")

graph_cor.melt <- melt(graph_cor)
p <- ggplot(graph_cor.melt,aes(as.factor(Var1),as.factor(Var2))) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient(low="white",high="red")
p




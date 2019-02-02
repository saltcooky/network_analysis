library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)
library(MASS)

source("preparation.R")

# Bのグラフ描写
plot_tbl_data(g2_tbl)

# Cのグラフ描写
plot_tbl_data(g3_tbl)

# ハミルトン距離
sum(abs(A-B))

# グラフ同士の相関(入れ替えなし)
gcor(A,B)

# グラフ同士の相関(入れ替えを考える)
gscor(A,B)

# 中心性の類似性 中心化共鳴分析
library(sna)
library(MASS)

degree_df <- data.frame(A_deg = degree(A),B_deg = degree(B),C_deg = degree(C))
cre <- corresp(degree_df, nf = min(dim(degree_df)) - 1)

# データの形成とグラフ化
cre$rscore <- cre$rscore %>% as.data.frame %>% mutate(type = "nord")
cre$cscore <- cre$cscore %>% as.data.frame %>% mutate(type = "graph")
cre_df <- rbind(cre$rscore, cre$cscore) %>% 
  mutate(name = c(1:20,"A","B","C"))

p <- ggplot(cre_df,aes(x = V1, y = V2, label = name)) +
  geom_hline(yintercept = 0, colour = "gray75") +
  geom_vline(xintercept = 0, colour = "gray75") +
  geom_text(aes(colour = type), alpha = 0.8, size = 5) +
  ggtitle("Centering Resonanse Analysis plot")
p

eig <- cre$cor^2
cumsum(100 * eig / sum(eig))

# コアの抽出
library(GA)

make.pat.matrix <- function(x) {
  pat.matrix <- matrix(NA, length(x), length(x))
  pat.matrix[which(x == 1), which(x == 1)] <- 1
  pat.matrix[which(x == 0), which(x == 0)] <- 0
  diag(pat.matrix) <- NA
  return(pat.matrix)
}

# Aのコア抽出
matrix.cor.A <- function(x) {return(gcor(A, make.pat.matrix(x)))}
ga.core.A <- ga(type = "binary", fitness = matrix.cor.A,nBits = nrow(A))
summary(ga.core.A)

A_core = t(ga.core.A@solution) %>% as.data.frame()
g1_tbl <- g1_tbl %>% 
  mutate(nclass = as.factor(A_core$V1))
plot_tbl_com_data(g1_tbl,"estimete core by GA")

# Cのコア抽出
matrix.cor.C <- function(x) {return(gcor(C, make.pat.matrix(x)))}
ga.core.C <- ga(type = "binary", fitness = matrix.cor.C,nBits = nrow(C))
summary(ga.core.C)

C_core = t(ga.core.C@solution) %>% as.data.frame()
g3_tbl <- g3_tbl %>% 
  mutate(nclass = as.factor(C_core$V1))
plot_tbl_com_data(g3_tbl,"estimete core by GA")

library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)
library(sna)

source("preparation.R")

A1 <- matrix(c(
  0,1,1,1,0,1,1,
  1,0,1,0,0,1,1,
  1,1,0,0,1,0,0,
  1,0,0,0,1,0,0,
  0,0,1,1,0,1,0,
  1,1,0,0,1,0,0,
  1,1,0,0,0,0,0),
  nrow = 7, ncol = 7)

A2 <- matrix(c(
  0,0,1,1,0,1,0,
  0,0,1,0,0,0,1,
  1,1,0,1,1,0,0,
  1,0,1,0,1,0,0,
  0,0,1,1,0,1,1,
  1,0,0,0,1,0,1,
  0,1,0,0,1,1,0),
  nrow = 7, ncol = 7)

graph_from_adjacency_matrix(A1) %>%
  as_tbl_graph(direted = F) %>% 
  mutate(name = 1:7) %>% 
  plot_tbl_data("A1")

graph_from_adjacency_matrix(A2) %>%
  as_tbl_graph(direted = F) %>% 
  mutate(name = 1:7) %>% 
  plot_tbl_data("A2")

gstack <- array(dim = c(2,7,7))
gstack[1,,] <- A1
gstack[2,,] <- A2

gscor(gstack)[1,2]

# QAP検定
qap <- qaptest(gstack, gcor, g1 = 1, g2 = 2, reps = 1000)
summary(qap)
# plot(qap)

qap_df <- data.frame(dist = qap$dist)
gp <- ggplot(qap_df, aes(x = dist)) +
  geom_histogram(binwidth = 0.1) + 
  geom_vline(xintercept = gscor(gstack)[1,2], colour = "red")
gp

# CUG検定 
gcor(ADVICE, FRIEND)

gstack3 <- array(dim = c(2,21,21))
gstack3[1,,] <- ADVICE
gstack3[2,,] <- FRIEND

cug_test <- cugtest(gstack3, gcor, reps = 1000)
summary(cug_test)

dist_data <- data.frame(dist = cug_test$dist) 
gp <- ggplot(dist_data, aes(x = dist)) +
  geom_histogram()+ 
  geom_vline(xintercept = gcor(gstack3)[1,2], colour = "red")
gp

# 推移性の検定(サイズと密度を固定)
gtrans(FRIEND)  

cug_gtrans <- cug.test(FRIEND, gtrans, cmode = "edges")
print(cug_gtrans)

dist_data <- data.frame(dist = cug_gtrans$rep.stat) 
gp <- ggplot(dist_data, aes(x = dist)) +
  geom_histogram() + 
  geom_vline(xintercept = gtrans(FRIEND), colour = "red") 
gp

# 密度の検定(辺が張る確率が0.5に固定)
gden(FRIEND)
cug.gden <- cug.test(FRIEND, gden, cmode="size")
print(cug.gden)

dist_data <- data.frame(dist = cug.gden$rep.stat) 
gp <- ggplot(dist_data, aes(x = dist))
gp <- gp + geom_histogram() + geom_vline(xintercept = gden(FRIEND), colour = "red") 
gp

# 密度の検定(サイズを固定)
cug.gtrans2 <- cug.test(FRIEND, gtrans, cmode="size")
print(cug.gtrans2)

dist_data <- data.frame(dist = cug.gtrans2$rep.stat) 
gp <- ggplot(dist_data, aes(x = dist))
gp <- gp + geom_histogram() + geom_vline(xintercept = gtrans(FRIEND), colour = "red") 
gp

library(tidyverse)
library(statnet)

data(florentine)

flomarriage_tbl <- as_tbl_graph(flomarriage)

# 富を重みにしたグラフ化
flomarriage_tbl %>%  
  ggraph(layout = "kk") +
  geom_edge_link(alpha=0.8, colour = "lightgray") + 
  scale_edge_width(range = c(0.1,1)) +
  geom_node_point(aes(size = wealth)) +
  geom_node_label(aes(label = name),repel = TRUE)

# エッジのみを加味したモデル
flomarriage.model.0 <- ergm(flomarriage ~ edges)
summary(flomarriage.model.0)

# エッジと三角形と加味したモデル
flomarriage.model.1 <- ergm(flomarriage ~ edges + triangle)
summary(flomarriage.model.1)

# エッジと富の度合いと加味したモデル
flomarriage.model.2 <- ergm(flomarriage ~ edges + nodecov("wealth"))
summary(flomarriage.model.2)

# エッジと三角形と加味したモデル
flomarriage.model.3 <- ergm(flomarriage ~ edges + triangle + nodecov("wealth") + nodecov("priorates") )
summary(flomarriage.model.3)

# 作成したモデルの当てはまり評価
gof_model2 <- gof(flomarriage.model.3)
par(mfrow = c(2,2))
plot(gof_model2)

# 属性の追加
ADVICE <- as.network(ADVICE)

age <- c(33,42,40,33,32,59,55,34,62,37,46,
         34,48,43,40,27,30,33,32,38,36)
tenure <- c(9.333,19.583,12.75,7.5,3.333,
            28,30,11.333,5.417,9.25,27,8.917,0.25,10.417,
            8.417,4.667,12.417,9.083,4.833,11.667,12.5)
dpt <- c(4,4,2,4,2,1,0,1,2,3,3,1,2,2,2,4,1,3,2,2,1)
level <- c(3,2,3,3,3,3,1,3,3,3,3,3,3,2,3,3,3,2,3,3,2)
ADVICE %v% "age" <- age
ADVICE %v% "tenure" <- tenure
ADVICE %v% "dpt" <- dpt
ADVICE %v% "level" <- level

# 要因のデータを作成
diff.age <- age %>% 
  matrix(nrow = 21, ncol = 21) %>% 
  sweep(2, age) %>% 
  abs()

diff.tenure <- tenure %>% 
  matrix(nrow = 21, ncol = 21, byrow = TRUE) %>% 
  sweep(1, tenure)

diff.level <- level %>% 
  matrix(nrow = 21, ncol = 21) %>% 
  sweep(2, level)

ADVICE_model <- ergm(ADVICE ~ edges + edgecov(diff.age) +
                         edgecov(diff.tenure) + edgecov(diff.level) +  nodefactor("dpt") +
                         nodematch("dpt") + receiver(base = 13) + mutual)
summary(advice.model.2)

gof_ADVICE_model <- gof(ADVICE_model)
par(mfrow = c(2,3), mar = c(4,4,3,1))
plot(gof_ADVICE_model)

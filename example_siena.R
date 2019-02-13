# Rsienaのインストール
# install.packages("RSiena",repos = "http://R-Forge.R-project.org")

library(tidyverse)
library(RSiena)
library(tidygraph)
library(ggraph)
library(sna)
data(s501)

# データの確認
plot_tbl_directed_data <- function(data, title){
  data %>% 
    as_tbl_graph(directed = TRUE,mode = "out") %>%  
    ggraph(layout = "kk") +
    geom_edge_link(alpha=0.8, colour = "#424242",arrow = arrow(length = unit(2, 'mm')), end_cap = circle(2, 'mm')) + 
    scale_edge_width(range = c(0.1,1)) +
    geom_node_point() +
    ggtitle(title) +
    coord_fixed()
}

plot_tbl_directed_data(s501, title = "s501")
plot_tbl_directed_data(s502, title = "s502")
plot_tbl_directed_data(s503, title = "s503")

# データの整形
# 説明変数オブジェクトを作成
friendship <- array(c(s501,s502,s503), dim = c(50,50,3))
friendship <- sienaDependent(friendship)
alcohol <- varCovar(s50a) #時間変化する説明変数：飲酒
smoke <- coCovar(s50s[,1]) #時間変化しない説明変数：喫煙

mydata <- sienaDataCreate(friendship, alcohol, smoke)
mydata

myeffect <- getEffects(mydata)
effectsDocumentation(myeffect)

myeffect <- includeEffects(myeffect, egoX, interaction1 = "alcohol")
myeffect <- includeEffects(myeffect, altX, egoXaltX, interaction1 = "alcohol")
myeffect <- includeEffects(myeffect, simX, interaction1 = "smoke1" )
myeffect

myproject <- sienaAlgorithmCreate(projname = "s50")
resalt <- siena07(myproject, data=mydata, effects=myeffect)
summary(resalt)
siena.table(resalt, type="html", sig = T)

# モデルの当てはまりの診断
resalt <- siena07(myproject, data=mydata, effects=myeffect, returnDeps = TRUE)
gofi <- sienaGOF(resalt, IndegreeDistribution, varName = "friendship", cumulative = FALSE, join=TRUE)
plot(gofi)




library(FactoMineR)
library(factoextra)

clusterTest <- read_csv("Data/Processed/clusteringTest.csv")
res.pca <- PCA(clusterTest, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

head(res.hcpc$data.clust, 10)
res.hcpc$desc.var$quanti
res.hcpc$desc.axes$quanti
res.hcpc$desc.ind$para

head(res.hcpc$desc.axes, 10)

clusters <- res.hcpc$data.clust
write.csv(clusters, file = "clusters.csv")



clusterTest2 <- read_csv("Data/Processed/clusteringTest2.csv")

test.mca <- MCA(clusterTest2,
               ncp = 20,
               graph = T)
test.hcpc <- HCPC(test.mca, nb.clust = -1, graph = FALSE)
fviz_dend(res.hcpc, show_labels = FALSE)
fviz_cluster(test.hcpc, geom = "point", main = "Factor map")
# Description by variables
res.hcpc$desc.var$test.chi2
# Description by variable categories
res.hcpc$desc.var$category
# show principal dimensions that are the most associated with clusters
res.hcpc$desc.axes$quanti
# Description by individuals
res.hcpc$desc.ind$para

dimdesc(res.mca, axes=1:7, proba=0.05)
clusterTest2 <- res.hcpc$data.clust


#count instances of each cluster
clusterTest21 <- clusterTest2 %>%
  group_by(clust) %>%
  summarize(count = n()) 


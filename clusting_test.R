library(FactoMineR)
library(factoextra)
# Compute PCA with ncp = 3
res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)


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

ex = 1
th = 0
other = 2
senior = 1
junior = 0
collab =0
community = 1
other = 2
funder = 3
individual = 4

clusterSummary <- read_csv("clusterSummary.csv")


clusterTest2 <- read_csv("Data/Processed/clusteringTest2.csv")

data(clusterTest2)
res.mca <- MCA(clusterTest2,
               ncp = 20,
               graph = FALSE)
res.hcpc <- HCPC(res.mca, graph = FALSE, max = 4)
fviz_dend(res.hcpc, show_labels = FALSE)
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")
# Description by variables
res.hcpc$desc.var$test.chi2
# Description by variable categories
res.hcpc$desc.var$category
# Description by principle components
res.hcpc$desc.axes
# Description by individuals
res.hcpc$desc.ind$para


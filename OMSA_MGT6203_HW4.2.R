mydata <- read.csv("ShoppingVisits.csv")

plot(mydata, pch=20)
### Task 1: K-means and scaling ###

# 1. K-means
km.res.ns <- kmeans(mydata,3,nstart=20)
km.res.ns
plot(mydata, col=km.res.ns$cluster, pch=20, main="K-means W/out Scaling")

# 2. K-means scaled
mydata.sc <- scale(mydata)
km.res.ns.2 <- kmeans(mydata.sc,3,nstart=20)
plot(mydata, col=km.res.ns.2$cluster, pch=20, main="K-means With Scaling")

# 3. compare scaled vs unscaled
par(mfrow=c(1,2))
plot(mydata, col=km.res.ns$cluster, pch=20, main="K-means W/out Scaling")
plot(mydata, col=km.res.ns.2$cluster, pch=20, main="K-means With Scaling")

# 4. without scaling we can see the clusters are drawn only on the y-axis as the values are much larger than the x-values

### Task 2: K-means with varied number of clusters ###

# 5. k-means with 2-5 clusters
km.res.2 <- kmeans(mydata.sc,2,nstart=20)
km.res.4 <- kmeans(mydata.sc,4,nstart=20)
km.res.5 <- kmeans(mydata.sc,5,nstart=20)

# 6. display 2-5 cluster graphs side by side
par(mfrow=c(2,2))
plot(mydata, col=km.res.2$cluster, pch=20, main="K-means / Scaled / 2-clusters")
plot(mydata, col=km.res.ns.2$cluster, pch=20, main="K-means / Scaled / 3-clusters")
plot(mydata, col=km.res.4$cluster, pch=20, main="K-means / Scaled / 4-clusters")
plot(mydata, col=km.res.5$cluster, pch=20, main="K-means / Scaled / 5-clusters")

# 7. looks like 4 clusters could be most appropriate

### Task 3: Choosing K ###

# 8. create the elbow chart
ss.vec <- c(
  km.res.2$totss,
  km.res.2$tot.withinss,
  km.res.ns.2$tot.withinss,
  km.res.4$tot.withinss,
  km.res.5$tot.withinss
)
par(mfrow=c(1,1))
plot(ss.vec, type='b', xlab="Number of Clusters", ylab = "Total Within-Cluster SS")

# 9. Based on this 3 clusters looks to be pretty good
 
### Task 4: Hierarchical Clustering ###

# 10. Perform hierarchical clustering, average linkage
hc.res.average <- hclust(dist(mydata.sc), method = "average")
plot(hc.res.average, cex=0.5, main="Dendrogram (Average Linkage)",xlab="",ylab="",sub="")

# 11. Cut into 3 clusters
abline(h=1.75, col='red', lty=2)

### Task 5: K-means Vs heirarchical ###

# 12. compare 3 clusters of both meathods
par(mfrow=c(1,2))
plot(mydata, col=km.res.ns.2$cluster, pch=20, main=paste("K-means with 3 Clusters"))
plot(mydata, col=cutree(hc.res.average, k=3), pch=20, main="Heirarchical Clustering (Average Linkage)")

# 13. it looks like the two outcomes are slightly different.

### Task 6: Heirarchical with different linkages ###

# 14. perform the remaing 3 heirarchical clustering and compare.
hc.res.centroid <- hclust(dist(mydata.sc), method = "centroid")
hc.res.complete <- hclust(dist(mydata.sc), method = "complete")
hc.res.single <- hclust(dist(mydata.sc), method = "single")

par(mfrow=c(2,2))
plot(mydata, col=cutree(hc.res.average, k=3), pch=20, main="Heirarchical Clustering (Average Linkage)")
plot(mydata, col=cutree(hc.res.complete, k=3), pch=20, main="Heirarchical Clustering (Complete Linkage)")
plot(mydata, col=cutree(hc.res.single, k=3), pch=20, main="Heirarchical Clustering (Single Linkage)")
plot(mydata, col=cutree(hc.res.centroid, k=3), pch=20, main="Heirarchical Clustering (Centroid Linkage)")

# 15. Single looks to be the most unreasonable, it has one very large cluster with another only having 2 points

### Task 7: Interpret clusers ###

# 16. The three clusters seem to be low visits with low spending, low visits with high spending, and high visits with high spending

# 17. I think marketing resources should be directed to the low visits high spending group. If you could get them to visit more often they might spend even more.
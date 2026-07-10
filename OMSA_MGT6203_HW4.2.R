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


### Task 5: K-means Vs heirarchical ###

### Task 6: Heirarchical with different linkages ###

### Task 7: Interpret clusers ###
# K-Nearset Neighbours
library(class)
?knn
attach(Smarket)
Xlag = cbind(Lag1,Lag2)
Xlag[1:5,]
class(Xlag)
train=Year<2005
knn_pred=knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn_pred, Direction[!train])
mean(knn_pred==Direction[!train])

# Try with 3 neighbors
knn_pred=knn(Xlag[train,], Xlag[!train,], Direction[train], k=3)
table(knn_pred, Direction[!train])
mean(knn_pred==Direction[!train])

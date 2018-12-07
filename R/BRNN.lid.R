#' BRNN function for lidar point cloud
#'
#' BRNN.lid(data,max.k,max.dist,min.pts)
#'
#'
#' @data lidar point cloud in data.frame e.g. data[,c('x','y','z')]
#' @max.k max number of neighborhood data
#' @max.dist  max distance within neighborhood
#' @min.pts  min number of points within one cluster
#'
#'
BRNN.lid<-function(data,max.k,max.dist,min.pts) {
  ###data:input data with xyz coordinates; k: number of neighbors
  ###max.dist: max distance in neighborhood; min.pts: min points per cluster
  nearest<-nn2(data,data,k=max.k,searchtype="radius",radius = 1)

  nearest.c<-nearest

  for ( i in 1:dim(nearest$nn.idx)[1] ) {
    temp<-nearest$nn.dists[i,]
    nearest.c$nn.idx[i,which(temp>max.dist)]<-0
    #nearest$nums[i]<-sum(temp)
  }

  #nearest<-nn2(pts.cors,pts.cors,k=5)
  P.cluster<-rep(NA,dim(nearest$nn.idx)[1])
  class.idx=1
  for (p.i in 1: length(P.cluster)){

    if(length(which(nearest.c$nn.idx[p.i,]>0))==0) {next}
    if(length(which(nearest.c$nn.dists[p.i,]>0))==0) {next}

    i.neighbors<-nearest.c$nn.idx[p.i,which(nearest.c$nn.idx[p.i,]>0)]
    temp.cluster<-P.cluster[i.neighbors]


    if (length(unique(temp.cluster[which(!is.na(temp.cluster))]))==0) {
      P.cluster[i.neighbors]<-class.idx
      class.idx<-class.idx+1
    }
    else if (length(unique(temp.cluster[which(!is.na(temp.cluster))]))==1){
      P.cluster[i.neighbors]=unique(temp.cluster[which(!is.na(temp.cluster))])      ###only one class in the cluster
    }
    else if (length(unique(temp.cluster[which(!is.na(temp.cluster))]))>1){
      i.class<-sort(unique(temp.cluster[which(!is.na(temp.cluster))]))
      for (i.rep in 2:length(i.class)){
        P.cluster[which(P.cluster==i.class[i.rep])]<-i.class[1]
      }
      P.cluster[i.neighbors]=i.class[1]
    }

  }
  #length(unique(P.cluster))
  ###add cluter to pts
  #data.signed<-data
  # pts.cors.signed$cluster.org<-P.cluster

  #length(unique(P.cluster))
  #;table(P.cluster)
  #color3<-terrain.colors(max(unique(P.cluster),na.rm=T))
  #color3<-rainbow(max(unique(P.cluster),na.rm=T))
  #color3<-colors(max(unique(P.cluster),na.rm=T))
  #open3d();plot3d(pts.cors$x,pts.cors$y,pts.cors$z,pch='*', col=color3[P.cluster])
  #windows();plot(pts.cors$y,pts.cors$z,pch=19, col=color3[P.cluster])
  #pts.cors[P.cluster==cl.idx,]


  ###filtering small clusters
  cluster.pts.counts<-table(P.cluster)
  small.clusters<-as.numeric(names(cluster.pts.counts[cluster.pts.counts<min.pts]))
  for (small.idx in 1:length(small.clusters)){
    P.cluster[P.cluster==small.clusters[small.idx]]<-0
  }
  pts.cors.thinned<-data[P.cluster!=0,]
  P.cluster.thinned<-P.cluster[P.cluster!=0]

  #color3<-rainbow(max(P.cluster.thinned,na.rm=T))
  #length(unique(P.cluster.thinned))
  #color3<-rainbow(max(unique(P.cluster),na.rm=T))
  #color3<-colors(max(unique(P.cluster),na.rm=T))
  #open3d();plot3d(pts.cors.thinned$x,pts.cors.thinned$y,pts.cors.thinned$z,pch='*', col=color3[P.cluster])

  #windows();plot(pts.cors.thinned$x,pts.cors.thinned$y,ylim = rev(range(pts.cors.thinned$y)),pch=19,cex=0.7,col=color3[P.cluster])

  ###update the cluster number and cluster centroid ($components) after merge
  cluster.idx.merge<-sort(unique(P.cluster[which(P.cluster!=0)]))
  cluster.thinned<-P.cluster

  for(c.m in 1:length(cluster.idx.merge)){
    cluster.thinned[which(cluster.thinned==cluster.idx.merge[c.m])]<-c.m
  }
  #sort(unique(pts.cors.signed$cluster.thinned))

  #rm(list=c("P.cluster","nearest","nearest.c",""))
  OUT<-list()
  OUT<-data;
  OUT$cluster<-cluster.thinned;

  return(OUT)

}

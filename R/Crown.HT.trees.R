###crown height extraction
###plot crown and ht
Crown.HT.trees<-function(data.pts,cluster.info){
  #data.pts<-pts.signed[,c('x','y','z')];
  #cluster.info<-pts.signed$cluster
  rep(0,length(unique(cluster.info[which(cluster.info!=0)])))->crown.a1->cht1->pts.num->EW->NS
  
  for(cls.picker in sort(unique(cluster.info[which(cluster.info!=0)]))) {
    single.cluster<-data.pts[which(cluster.info==cls.picker),]
    pts.num[cls.picker]<-dim(single.cluster)[1];
    EW[cls.picker]<-range(single.cluster[,"x"])[2]-range(single.cluster[,"x"])[1]
    NS[cls.picker]<-range(single.cluster[,"y"])[2]-range(single.cluster[,"y"])[1]
    #open3d();plot3d(single.cluster$x,single.cluster$y,single.cluster$z,pch='*', xlim=XRNG,ylim=YRNG)
    convex.hull<-chull(x=single.cluster[,1],y=single.cluster[,2])
    
    convex.hull<-c(convex.hull,convex.hull[1])
    convex.hull.coords<-single.cluster[convex.hull,1:2]
    
    #windows();plot(convex.hull.coords)
    #polygon(convex.hull.coords)
    
    #calculate convex area
    chull.poly <- Polygon(convex.hull.coords, hole=F)
    
    crown.a1[cls.picker] <- chull.poly@area
    
    cht1[cls.picker]<-max(single.cluster$z)
    
    
  }
  
  OUTPUTS<-list()
  OUTPUTS$crown<-crown.a1
  OUTPUTS$height<-cht1
  OUTPUTS$pts.num<-pts.num
  OUTPUTS$EW.cdia<-EW  ##crown diameter along East-West direction
  OUTPUTS$NS.cdia<-NS  ##crown diameter along North-South direction
  return(OUTPUTS)
}

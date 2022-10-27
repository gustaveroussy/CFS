CopyKat_all=function(data=NULL, species="S", threads=4,kcut=2,annotate=TRUE){
  
  data_B <- spatialPreprocess(data_B, platform="ST", n.PCs=7, n.HVGs=2000, log.normalize=TRUE)
  data_B <- qTune(data_B, qs=seq(2, 20), platform="ST", d=7)
  qPlot(data_B)
  
  set.seed(149)
  data_B <- spatialCluster(data_B, q=9, platform="ST", d=7,
                             init.method="mclust", model="t", gamma=2,
                             nrep=10000, burn.in=100,
                             save.chain=TRUE)
  
  clusterPlot(data_B)
  
  data_B.enhanced <- spatialEnhance(data_B, q=9, platform="ST", d=7,
                                      model="t", gamma=2,
                                      jitter_prior=0.3, jitter_scale=3.5,
                                      nrep=1000, burn.in=100,
                                      save.chain=TRUE)
  
  mcmcChain(data_B, "Ychange")
  
  return(BAYESSPACE_COMPLETE)
}
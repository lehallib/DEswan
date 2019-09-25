#' Calculate number of significant variables for a set of pvalues thresholds
#'
#' @param 
#' res.DEswan.wide: a data Frame - pvalues or qvalues from DEswan in a wide format
#' @param 
#' thresholds: a Vector - p or q-values thresholds. Default is c(0.05,1e-2,1e-3,1e-4,1e-5)
#' @export
#' @examples
#' res.DEswan=DEswan(data.df = agingplasmaproteome[,-c(1:3)],
#' qt = agingplasmaproteome[,1],
#' window.center = seq(40,100,10),
#' buckets.size = 10,
#' covariates = agingplasmaproteome[,c(2:3)])
#' res.DEswan.wide.p=reshape.DEswan(res.DEswan,parameter = 1,factor = "qt")
#' res.DEswan.wide.p.signif=nsignif.DEswan(res.DEswan.wide.p)
#' head(res.DEswan.wide.p.signif)


nsignif.DEswan=function(res.DEswan.wide, thresholds){
  
  if(missing(res.DEswan.wide)==T){
    print("No input DEswan results")
    stop
  }
  if(missing(thresholds)==T){
    thresholds=c(0.05,1e-2,1e-3,1e-4,1e-5)
  }
  
  
  res.DEswan.wide.signif=NULL
  i=1
  for(i in 1:length(thresholds)){
    res.DEswan.wide.signif=rbind(res.DEswan.wide.signif,
                                 apply(res.DEswan.wide[,-1],2,function(x) sum(x<thresholds[i],na.rm = T)))
  }
  rownames(res.DEswan.wide.signif)=thresholds
  return(res.DEswan.wide.signif)
}




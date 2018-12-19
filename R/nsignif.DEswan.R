#' Calculate number of significant variables for a set of pvalues thresholds
#'
#' @param 
#' res.DEswan.wide: a data Frame - pvalues or qvalues from DEswan in a wide format
#' @param 
#' thresholds: a Vector - p or q-values thresholds. Default is c(0.05,1e-2,1e-3,1e-4,1e-5)
#' @export
#' @examples
#' res.DEswan.wide.p.signif=nsignif.DEswan(res.DEswan.wide.p)
#' 
#' 
#' 

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
                                 apply(res.DEswan.wide[,-1],2,function(x) sum(x<thresholds[i])))
  }
  rownames(res.DEswan.wide.signif)=thresholds
  return(res.DEswan.wide.signif)
}




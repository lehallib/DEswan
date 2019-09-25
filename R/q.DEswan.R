#' caculate qvalues for DE-SWAN output
#'
#' @param 
#' res.DEswan.wide.p: a data Frame - pvalues from DEswan in a wide format
#' @param 
#' method: method for pvalues adjustment. Default is BH
#' @export
#' @examples
#' res.DEswan=DEswan(data.df = agingplasmaproteome[,-c(1:3)],
#' qt = agingplasmaproteome[,1],
#' window.center = seq(40,100,10),
#' buckets.size = 10,
#' covariates = agingplasmaproteome[,c(2:3)])
#' res.DEswan.wide.p=reshape.DEswan(res.DEswan,parameter = 1,factor = "qt")
#' res.DEswan.wide.q=q.DEswan(res.DEswan.wide.p,method="BH")
#' head(res.DEswan.wide.q)

q.DEswan=function(res.DEswan.wide, method){
  
  if(missing(res.DEswan.wide)==T){
    print("No input DEswan results")
    stop
  }
  if(missing(method)==T){
    method="BH"
  }
  

  res.DEswan.wide.q=data.frame(variable=res.DEswan.wide$variable,apply(res.DEswan.wide[,-1],2,function(x) p.adjust(x,method=method)),stringsAsFactors = F)
  
  return(res.DEswan.wide.q)
}



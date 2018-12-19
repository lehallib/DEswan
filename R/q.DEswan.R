#' caculate qvalues for DE-SWAN output
#'
#' @param 
#' res.DEswan.wide.p: a data Frame - pvalues from DEswan in a wide format
#' @param 
#' method: method for pvalues adjustment. Default is BH
#' @export
#' @examples
#' res.DEswan.wide.q=q.DEswan(res.DEswan.wide.p,method="BH")
#' res.DEswan.wide.q=q.DEswan(res.DEswan.wide.p,method="Bonferonni")
#' 
#' 
#' 

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



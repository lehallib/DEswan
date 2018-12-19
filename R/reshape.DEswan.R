#' Reshape DE-SWAN output
#'
#' @param 
#' res.DEswan: a data Frame - output from DEswan
#' @param 
#' parameter: an optional numeric - 1 for pvalue or 2 for coefficients. Default is 1
#' @param 
#' factor: an optional character string - qt or covariates in DEswan. Default is qt.
#' @export
#' @examples
#' # list of factor
#' unique(res.DEswan$p$factor)
#' unique(res.DEswan$coeff$factor)
#' res.DEswan.wide.p=reshape.DEswan(res.DEswan)
#' res.DEswan.wide.p=reshape.DEswan(res.DEswan,parameter = 1,factor = "qt")
#' res.DEswan.wide.coeff=reshape.DEswan(res.DEswan,parameter = 2,factor = "qt")
#' 
#' 
#' 

reshape.DEswan=function(res.DEswan, parameter, factor){
  
  if(missing(res.DEswan)==T){
    print("No input DEswan results")
    stop
  }
  if(missing(parameter)==T){
    parameter=1
  }
  if(missing(factor)==T){
    factor = "qt"
  }


  
  toReshape=res.DEswan[[parameter]]
  # head(toReshape)
  toReshape=toReshape[which(toReshape$factor==factor),-3]
  res.DEswan.wide=data.frame(reshape::cast(toReshape,variable~window.center,value = colnames(toReshape)[3]),stringsAsFactors = F)
  return(res.DEswan.wide)
}




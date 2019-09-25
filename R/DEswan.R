#' Differential Expression - Sliding Window ANalysis (DE-SWAN)
#'
#' @param 
#' data.df:	a data Frame - columns are variables and rows are samples (same order as qt and covariates)
#' @param 
#' qt:	a Vector - quantitative trait for DE-SWAN (same order as data.df and covariates)
#' @param 
#' window.center: an optional Vector - window centers. Default is quantile(qt,  probs = seq(.1,.9,.1))
#' @param 
#' buckets.size:	an optional Numeric - size of each bucket. Default is set to (max(range(qt))-min(range(qt)))/2
#' @param 
#' covariates:	a data Frame. Columns are variables and rows are samples (same order as qt and covariates)
#' @return
#' list of 2 data Frames: $p  =  pvalues of qT and covariates for each variable and each window.center and $coefficients  =  coefficients of qT and covariates for each variable and each window.center
#' @export
#' @examples
#' res.DEswan=DEswan(data.df = agingplasmaproteome[,-c(1:3)],
#'                qt = agingplasmaproteome[,1],
#'                window.center = seq(40,100,10),
#'                buckets.size = 10,
#'                covariates = agingplasmaproteome[,c(2:3)])
#' head(res.DEswan$p)
#' head(res.DEswan$coeff)
#' 
DEswan=function(data.df, qt, window.center, buckets.size, covariates){
  
  if(missing(data.df)==T){
    print("No input data frame")
    stop
  }
  if(missing(qt)==T){
    print("No quantitative trait")
    stop
  }
  if(missing(window.center)==T){
    window.center = quantile(qt,  probs = seq(.1,.9,.1))
  }
  if(missing(buckets.size)==T){
    buckets.size  <-  (max(range(qt))-min(range(qt)))/2
  }
  if(missing(covariates)==T){
    covariates  <-  NULL
  }else{
    covariates <- data.frame(covariates)  
  }
  
  pvalues.tot  <-  NULL
  coefficients.tot  <-  NULL
  
  
  # k  <-  5
  # reorder window.center and remove duplicates
  window.center=sort(unique(window.center))
  
  for(k in 1:length(window.center)){
    pvalues  <-  NULL
    coefficients  <-  NULL
    for(i in 1:ncol(data.df)){
      qt.tmp  <-  rep(NA, length(qt))
      qt.tmp[which(qt < window.center[k] & qt >= (window.center[k] - buckets.size))] <- 0
      qt.tmp[which(qt > window.center[k] & qt <= (window.center[k] + buckets.size))] <- 1
      qt.tmp  <-  factor(qt.tmp)
      if(is.null(covariates) == T){
        deswan.formula = "data.df[, i] ~ qt.tmp"  
      }else{
        deswan.formula  <-  paste(c("data.df[, i] ~ qt.tmp", paste("covariates$",colnames(covariates), collapse  =  " + ", sep = "")), collapse = " + ", sep = "")
      }
      test.glm  <-  NULL
      test.glm  <- try(glm.fit  <-  glm(as.formula(deswan.formula), family  =  gaussian), silent=TRUE)
      if(class(test.glm)[1] !=  "try-error"){
        glm.res  <-  car::Anova(glm.fit,  type  =  "2")
        pvalues  <-  rbind(pvalues, data.frame(variable  =  colnames(data.df)[i], window.center  =  window.center[k], factor  =  rownames(glm.res), pvalue=glm.res$`Pr(>Chisq)`, stringsAsFactors  =  F))
        coefficients  <- rbind(coefficients, data.frame(variable  =  colnames(data.df)[i], window.center  =  window.center[k], factor  = names(coefficients(glm.fit)), 
                                                        coefficient=coefficients(glm.fit), stringsAsFactors  =  F))
      }
    }
    pvalues.tot  =  rbind(pvalues.tot, pvalues)
    coefficients.tot  =  rbind(coefficients.tot, coefficients)
    
    print(paste("window.center  ", k, "/", length(window.center), sep = ""))
  }
  pvalues.tot$factor[which(pvalues.tot$factor=="qt.tmp")]<-"qt"
  pvalues.tot$factor=gsub("^covariates\\$","",pvalues.tot$factor)
  coefficients.tot$factor[which(coefficients.tot$factor=="qt.tmp1")]<-"qt"
  coefficients.tot$factor=gsub("^covariates\\$","",coefficients.tot$factor)

  
  results  =  list(p  =  pvalues.tot, coeff  =  coefficients.tot)
  return(results)
}


#' 
#' @title Computes correlation between two or more vectors
#' @description This is similar to the R base function 'cor'.
#' @details In addition to computing correlations this function, unlike
#' the R base function 'cor', produces a table outlining the number of complete cases 
#' to allow for the user to make a decision about the 'relevance' of the correlation
#' based on the number of complete cases included in the correlation calculations.
#' @param x a character, the name of a numerical vector, matrix or dataframe
#' @param y NULL (default) or the name of a vector, matrix or data frame with compatible 
#' dimensions to x.
#' @param naAction a character string giving a method for computing covariances in the 
#' presence of missing values. This must be one of the strings: "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' The default value is set to "pairwise.complete.obs"
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a list containing the results of the test
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   myvar <- list('LAB_HDL', 'LAB_TSC', 'GENDER')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example 1: generate the correlation matrix for the assigned dataset 'D' 
#'   # which contains 4 vectors (2 continuous and 1 categorical)
#'   ds.cor(x='D')
#' 
#'   # Example 2: calculate the correlation between two vectors (first assign some vectors from the dataframe 'D')
#'   ds.assign(newobj='labhdl', toAssign='D$LAB_HDL')
#'   ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
#'   ds.assign(newobj='gender', toAssign='D$GENDER')
#'   ds.cor(x='labhdl', y='labtsc')
#'   ds.cor(x='labhdl', y='gender')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.cor = function(x=NULL, y=NULL, naAction='pairwise.complete.obs', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
    
  if(is.null(x)){
    stop("x must be the name of a numeric vector or a table structure (matrix or data frame)", call.=FALSE)
  }else{
    defined <- isDefined(datasources, x)
  }
  
  # check the type of the input objects
  typ <- checkClass(datasources, x)
  
  if(typ=='numeric' | typ=='integer' | typ=='factor'){
    if(is.null(y)){
      stop("If x is a numeric vector, y must be a numeric vector!", call.=FALSE)
    }else{
      defined2 <- isDefined(datasources, y)
      typ2 <- checkClass(datasources, y)
    }
  }
  
  if(typ=='matrix' | typ=='data.frame' & !(is.null(y))){
    y <- NULL
    warning("x is a matrix or a data frame; y will be ignored and a correlation matrix computed for x!")
  }
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # call the server side function to compute the correlation matrix
  if(typ=='matrix' | typ=='data.frame'){
    cally <- paste0("corDS(x=", x, ", y=NULL", ", use='", naAction, "')")
  }else{
    if(!(is.null(y))){
      cally <- paste0("corDS(x=", x, ", y=", y, ", use='", naAction, "')")
    }else{
      cally <- paste0("corDS(x=", x, ", y=NULL", ", use='", naAction, "')")
    }
  }
  results <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  for(l in 1:length(stdnames)){
    for(i in 1:2){
      n1 <- " --correlation"
      n2 <- " --number of complete cases used"
      names(results[[l]]) <- c(n1, n2)
    }
  }
  
  return(results)
  
}

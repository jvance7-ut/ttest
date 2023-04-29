#' @title t-test final
#'
#'
#' @param x X Vector (numeric)
#' @param y Y vector (numeric)
#' @param alpha alpha/error value (numeric)
#' @param paired Is sample paired? (default FALSE)
#'
#' @return t-test and input results
#' @import devtools
#'
#' @export myttest
#'
#' @examples
#' \dontrun{myttest(x=rnorm(30,5,2), y=rnorm(30,3,2), alpha=0.05)}
myttest = function(x, y, alpha, paired = FALSE){
  #for the check function
  devtools = NULL
  t.test = NULL
  var.test = NULL

  library(devtools)

  #choose the type of test to run
  if(paired==TRUE){
    ttest = t.test(x, y, paired = TRUE)
    testType = "Paired"
  }
  else{
    #run an f-test to test variance equivalence
    ftest = var.test(x,y)

    if(ftest$p.value>=alpha){
      #run the t-test with equal var
      ttest = t.test(x, y, var.equal = TRUE)
      equalVar = TRUE
      testType = "T-test"
    }
    else{
      #run the t-test with unequal var
      ttest = t.test(x, y, var.equal = FALSE)
      equalVar = FALSE
      testType = "Welch"
    }
  }

  #accept or reject H0:
  if(ttest$p.value>=alpha){
    accept = "Y"
  }
  else{
    accept = "N"
  }

  #create the data frame of x and y
  #x and y won't always be the same length and may return an error
  #check the lengths of x and y and make corrections to length if
  #needed with NA values
  if(length(x)==length(y)){
    df = data.frame(x = x, y = y)
  }

  else if(length(x)<length(y)){
    xtemp = rep(NA, length(y))
    for (i in 1:length(x)) {
      xtemp[i] = x[i]
    }
    df = data.frame(x = xtemp, y = y)
  }

  else{
    ytemp = rep(NA, length(x))
    for (i in 1:length(y)) {
      ytemp[i] = y[i]
    }
    df = data.frame(x = x, y = ytemp)
  }

  #return the function information in a list
  tlist = list(Data = df, CI = ttest$conf.int, type = testType,
       nullAccept = accept, x = x, y = y, p = paired, ttest$statistic)

  #silently change the class to Rttest
  attr(tlist, "class") ="Rttest"
  tlist

}

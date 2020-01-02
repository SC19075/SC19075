#' @title A function to test parameter in function surlogrank
#' @description Whether the parameter lengths are equal,time variable input is of numeric type,status variable with only 0,1,group only two elements
#' @param x time variable
#' @param y status variable
#' @param z group variable
#' @return 1 or 0
#' @export
paraerr <- function(x,y,z){
  if(length(x)==length(y) & length(x)==length(z)){  
    if(is.numeric(x)){         
      if(identical(y^2,y)){    
        if(length(table(z))==2){  
          return(0)
        }else{
          return(1)
        }
      }else{
        return(1)
      }
    }else{
      return(1)
    }
  }else{
    return(1)
  }
}


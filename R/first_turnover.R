#' first_turnover() computes the last time an element is observed in a list
#'
#' @param a numeric vector such as year, or ordered number
#'
#' @return a numeric vector
#'
#' @details sequence function to identify a year an ordering index
#'
#' @examples
#' library(dplyr)
#' person_sequence <- stacked_data %>% 
#'                    dplyr::select(year, id_j, id_i, id_ij) %>%
#'                    unique() %>%
#'                    group_by(id_ij) %>%
#'                    summarise(start = min(year), final = first_turnover(year)) %>% 
#'                    mutate(time = final - start + 1)
#'
#' @export
first_turnover <- function(a){
  b<-c()
  for (i in 1:length(a)){
    if(i==length(a)){
      b[i]<-0
    } else {
      if((a[i+1]-a[i])==1){
        b[i]<-0
      } else {
        b[i]<-1
      }
    }
    if(length(a)==1) {b[i]=1}
  }

  if(all(b==0)){
    return(max(a))
  } else{
  return(a[min(which(b==1))])
  }
# Note: function design by merivera@uc.cl
}
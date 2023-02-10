# format response variable
# TODO: add ml.type == survival
format_y = function(y, ml.type){
  if (ml.type == 'classification'){
    y = as.factor(y)
  } else if (ml.type == 'regression') {
    y = as.numeric(y)
  } else if (ml.type == 'survival') {
    stop("implementation of survival models pending - 2023-01-24")
  } else {
    stop("ml.type must be of 'classification', 'regression'")
  }
  return(y)
}

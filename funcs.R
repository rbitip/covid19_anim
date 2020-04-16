# code to test if the safe transform works
test_safety <- function(data){
  b <- usmap_transform(select(data, long, lat, everything()))
  print(length(data$date.day) - length(b$date.day))
}

# safe version of the usmap transform - for some reason output of the usmap transform
# doesn't return the same amount of rows as the input if there data with different dates
# are mixed within the data frame. so, split the data up by date 
safe_transform <- function(data){
  days <- unique(data$date.day)
  output <- NULL
  for(day in days){
    transf <- usmap_transform(data[data$date.day == day,])
    
    if(is.null(output)) output <- transf
    else output <- rbind(output, transf)
  }
  return(output)
}
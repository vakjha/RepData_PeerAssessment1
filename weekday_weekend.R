weekend_weekday <- function(date) { 
  day <- weekdays(date) 
  if (day %in% c("Mon", "Tue", "Wed", "Thu", "Fri")) 
    return("weekday") 
  else if (day %in% c("Saturday", "Sunday")) 
    return("weekend") 
  else 
    stop("invalid date") 
   } 

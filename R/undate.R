# this function takes a date and turns it into three separate columns - year, month, day
# the input is a dataframe, x
# must have a column named 'date'


undate <- function(x){
  split = str_split(x$date, pattern="-")
  x$year = as.numeric(lapply(split, "[[",1))
  x$month = as.numeric(lapply(split, "[[",2))
  x$day = as.numeric(lapply(split, "[[",3))
  return(x)
}
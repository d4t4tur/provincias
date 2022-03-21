#aux_function

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}



#Change text of "Download JPEG" export menu item.
hcoptslang <- getOption("highcharter.lang")
hcoptslang$months <- as.character(lubridate::month(1:12, label = T, abbr = F))
hcoptslang$shortMonths <- as.character(lubridate::month(1:12, label = T, abbr = T))
hcoptslang$decimalPoint <- ","
hcoptslang$thousandsSep <- "."
hcoptslang$numericSymbols <- c("mil", "millones", "G", "T", "P", "E")
options(highcharter.lang = hcoptslang)
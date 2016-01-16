##### Function: Merge multiple files by column
####    Input:    path to multiple datasets
####    Output:   one dataset combining other datasets
# Source: http://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T,na.strings=c("", "NA"))})
  Reduce(function(x,y) {merge(x,y)}, datalist)}


##### Function: Merge multiple files by column
####    Input:    path to multiple datasets
####    Output:   one dataset combining other datasets
# Source: https://psychwire.wordpress.com/2011/06/03/merge-all-files-in-a-directory-using-r-into-a-single-dataframe/
combine = function(mypath) {
  old.wd <- getwd()
  setwd(mypath)
  file_list <- list.files()
  
  for (file in file_list){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.csv(file, header=TRUE)
    }
    
    # if the merged dataset does exist, append to it
    else if (exists("dataset")){
      temp_dataset <- read.csv(file, header=TRUE)
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    
  }
  setwd(old.wd)
  rm(old.wd)
  return(dataset)
}


##### Function: Order data by city, then by address, then by date
####    Input:    dataset
####    Output:   dataset sorted as above
sort.data <- function(data) {
  data <- data[order(
    data[,"Postal.City"], 
    data[,"Street.Address..Full."], 
    as.numeric(as.Date(data[, "Sale.Date"], "%m/%d/%Y")) 
    # as.Date(data[, "Sale.Date"], "%Y-%m-%d")
  )
  , ## This tells R to sort the rows
  ]
  return(data)
}

##### Function:  Generate the data for a purchase/sell pair
####    Inputs:   dataset, purchase row number, sell row number
####    Output:   one vector with agent name, p. price, p. date, s. price, s. date, address, city, years difference
generate.pair = function(data, p, s) {
  vector = c(
    as.character(data[p,"Selling.Agent.Full.Name"]), 
    as.numeric(data[p, "Sale.Price"]),
    as.numeric(as.Date(data[p, "Sale.Date"], "%m/%d/%Y")),
    as.numeric(data[s, "Sale.Price"]),
    as.numeric(as.Date(data[s, "Sale.Date"], "%m/%d/%Y")),
    as.character(data[p, "Street.Address..Full."]),
    as.character(data[p, "Postal.City"]),
    as.numeric(round(as.numeric((1/52) * difftime(as.Date(data[s, "Sale.Date"], "%m/%d/%Y"),
                                                  as.Date(data[p, "Sale.Date"], "%m/%d/%Y") , units="weeks")), digits = 2))
  )
}

##### Function: Generate the quality score (Q) for a given row of agent.data
####    Input: dataset, row of dataset
####    Output: quality score
quality.raw = function(agentdata, row) {
  total.appreciation = (as.numeric(agentdata[row, "Sale.Price.s"]) ) / as.numeric(agentdata[row, "Sale.Price.p"] )
  yearly.appreciation = total.appreciation  ^ (1 / as.numeric(agentdata[row, "Years"] ) ) - 1
  return(yearly.appreciation)
}

##### Function: Generate the market appreciation in median house prices given beginning and end dates
####    Input:  beginning date, end date in MLS format
####    Output: market appreciation in median house prices for that range of time
market = function(date.p, date.s) {
  
  # How many years between the dates?
  datediff = (as.numeric(date.s) - as.numeric(date.p))  # number of days
  datediff = datediff / 365     # number of years
  # Convert dates back
  date.p <- as.Date(as.numeric(date.p), origin="1970-01-01")
  date.s <- as.Date(as.numeric(date.s), origin="1970-01-01")
  # Get the month and year
  month.date.p <- format(date.p,"%m")
  year.date.p <- paste("X", format(date.p,"%Y"), sep = "")
  month.date.s <- format(date.s,"%m")
  year.date.s <- paste("X", format(date.s,"%Y"), sep = "")
  # Retreive the data
  market.p <- market.data[as.numeric(month.date.p), year.date.p]
  market.s <- market.data[as.numeric(month.date.s), year.date.s]
  # Calculate the market appreciation
  total.appreciation.market = (market.s / market.p)
  yearly.appreciation.market = total.appreciation.market  ^ (1 / as.numeric(datediff) )  - 1
  return(yearly.appreciation.market)
}



# as.Date(13764, origin="1970-01-01")
# as.numeric(as.Date("09/08/2007", "%m/%d/%Y"))

# 1998-08-16



##### Initialize environnment ------------------------------------------
setwd("C:/r/gargantuan-koala")

# Prepare data -----
  ##### Where is the data? -----------------------------------------------
  data <- read.csv("C:/r/svdata/svdata.full.csv", header = TRUE, stringsAsFactors = FALSE)
  
  ##### I got "Filtered.Address" instead of "Street.Address..Full." ------
  colnames(data)[3] <- "Street.Address..Full."
  data[, "Street.Address..Full."] <- toupper(data[, "Street.Address..Full."])

  ##### Read the data -----
  # Keep important columns only
  data <- data[, c("Postal.City", "Street.Address..Full.", "Sale.Date",  "Sale.Price", "Selling.Agent.Full.Name", "List.Agent.Full.Name", "List.Office.Name", "Selling.Office.Name")]
  
  # Read in sale dates
  data[, "Sale.Date.orig"] <- data[, "Sale.Date"]
  data[, "Sale.Date"] <- as.Date(data[, "Sale.Date"], "%Y-%m-%d")
  
  ##### Change "DeLeon Team" to "Kenneth Deleon"
  data[,"Selling.Agent.Full.Name"] [data[,"Selling.Agent.Full.Name"] == "DeLeon Team" ] <- "Kenneth Deleon"
  data[,"List.Agent.Full.Name"] [data[,"List.Agent.Full.Name"] == "DeLeon Team" ] <- "Kenneth Deleon"
  
  data[,"Selling.Agent.Full.Name"] [data[,"Selling.Office.Name"] == "Deleon Realty" ] <- "Kenneth Deleon"
  data[,"Selling.Agent.Full.Name"] [data[,"Selling.Office.Name"] == "Deleon Realty : 1 - DLRPA" ] <- "Kenneth Deleon"
  data[,"List.Agent.Full.Name"] [data[,"List.Office.Name"] == "Deleon Realty" ] <- "Kenneth Deleon"
  data[,"List.Agent.Full.Name"] [data[,"List.Office.Name"] == "Deleon Realty : 1 - DLRPA" ] <- "Kenneth Deleon"

##### Sort the data from MLS -------------------------------------------
data <- sort.data(data)

##### Define new agent data dataframe       ----------------------------
agent.data <- data.frame(matrix(ncol = 8, nrow = 0))


#####   Routine to run through data ------------------------------------
# At row i, check row i + 1 to see if "Street.Address..Full" of i is same as i+1 and if "Selling.Agent.Full.Name" of i is the same as "List.Agent.Full.Name" of i + 1

for (i in 1:(nrow(data) - 1)) {
  if (
    data[i, "Postal.City"] == data[i+1, "Postal.City"] &
    substr(data[i, "Street.Address..Full."], 1, 7) == substr(data[i+1, "Street.Address..Full."], 1, 7) &
    # Comment line below if we do NOT wish to restrict to same buying/selling agent
    as.character(data[i, "Selling.Agent.Full.Name"]) == as.character(data[i+1, "List.Agent.Full.Name"]) #&
#     # Comment lines below for complete cases only
#     any(is.na(data[i,])) == 0 &
#     any(is.na(data[i + 1,])) == 0
  )     {
    agent.data <- rbind(agent.data, as.character(generate.pair(data, i, i+1)))
    agent.data <- data.frame(lapply(agent.data, as.character), stringsAsFactors=FALSE)
  }
}

  ##### Name columns of the agent data dataframe  ------------------------
  colnames(agent.data) <- c(
    "Agent",
    "Sale.Price.p",
    "Sale.Date.p",
    "Sale.Price.s",
    "Sale.Date.s",
    "Street.Address..Full.",
    "Postal.City",
    "Years"
  )
  

##### Sort the agentdata dataframe -------------------------------------
agent.data <- agent.data[order(agent.data[,"Agent"]),]

##### Generate raw quality scores --------------------------------------
agent.data[, "Raw.Quality"] <- rep(NA, nrow(agent.data))
for (i in 1:(nrow(agent.data))) {
  agent.data[i, "Raw.Quality"] <- round(quality.raw(agent.data, i), digits = 4)
}

##### Only keep transactions where the property was held for at least half a year --
# agent.data <- agent.data[ agent.data[, "Years"] > 0.5 , ]
# 
# ####  Delete anything prior to a specified year
# year = 1000
# agent.data[, "Year"] <- as.numeric(format(agent.data[, "Sale.Date.p"], "%Y"))
# agent.data <- agent.data[ agent.data[, "Year"] >= year,]

##### Write dates in human-readable form again
agent.data[, "Sale.Date.p"] <- as.Date(as.numeric(agent.data[, "Sale.Date.p"]), origin = "1970-01-01")
agent.data[, "Sale.Date.s"] <- as.Date(as.numeric(agent.data[, "Sale.Date.s"]), origin = "1970-01-01")



##### Read market data
market.data <- read.csv("C:/r/tq/market.data.8cities.csv", header = TRUE, row.names = 1, nrows = 12)

##### Generate correction factors -------------------------------------
agent.data[, "Market"] <- rep(NA, nrow(agent.data))
for (i in 1:(nrow(agent.data))) {
  agent.data[i, "Market"] <- round(market(as.numeric(agent.data[i, "Sale.Date.p"]), 
                                          as.numeric(agent.data[i, "Sale.Date.s"])), digits = 4)
}

##### Uncomment this if we do NOT want a market correction factor ----
# agent.data[, "Market"] <- rep(0, nrow(agent.data))

##### Include correction factor --------------------------------------
agent.data[, "TQ"] <- agent.data[, "Raw.Quality"] - agent.data[, "Market"]
# 
# ##### Aggregate scores -----------------------------------------------
# results <- aggregate(agent.data[,"TQ"], by = list(agent.data[,"Agent"]), FUN = mean)
# colnames(results) <- c("Agent", "ATQ")
# results[, "ATQ"] <- round(results[, "ATQ"], digits = 2)
# results <- results[order(results[, "ATQ"], decreasing = TRUE), ]
# 
# ##### Generate total sales volume for each agent ---------------------
# for (i in 1:nrow(results)) {
#   results[i, "Volume"] <- sum(as.numeric(agent.data[, "Sale.Price.p"][agent.data[, "Agent"] == results[i, "Agent"]] ))
# }

# 
# ##### Include number of buy/sell transaction pairs for each agent
# for (i in 1:nrow(results)) {
#   results[i, "Pairs"] <- sum(agent.data[, "Agent"] == results[i, "Agent"]) }
# 
# ##### Only keep agents with certain criteria through eligible transactions
# # results <- results[ results[, "Volume"] >= 10000000 ,]
# results <- results[ results[, "Pairs"] > 5 ,]
# 


##### Results!
# View(results)
# View(agent.data)

##### Sort agent.data by TQ
################################ something in this section of code breaks SOMETHING in the agent.data df
# agent.data[, "Agent"] <- factor(agent.data[, "Agent"], levels=results[, "Agent"])
# agent.data <- agent.data[order(agent.data[, "Agent"], -agent.data[, "TQ"]),] 
# 
# write.csv(agent.data, "agent.data.csv")
# write.csv(results, "results.csv")

ken.data <- agent.data [ agent.data[, "Agent"] == "Kenneth Deleon", ]
ken.raw.average <- mean(ken.data[, "Raw.Quality"], na.rm = T)
ken.tq.average <- mean(ken.data[, "TQ"], na.rm = T)
write.csv(ken.data[complete.cases(ken.data),], "ken.data.csv")
write.csv(ken.data[order(ken.data[,"Sale.Date.p"], decreasing = TRUE), ], "ken.data.csv")
write.csv(ken.data[complete.cases(ken.data),], "ken.data.dlrpa.csv")


# View(results)
ken.raw.average
ken.tq.average

data[data[,"Street.Address..Full."] == "47 WALNUT AV",]
data[data[,"Street.Address..Full."] == "901 COLLEGE AVE",]

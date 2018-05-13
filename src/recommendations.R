library("quantmod")
library("forecast")
library("TTR")
source("src/esApi.R")

recommendationsIndexName <- "recommendation"
quotationsIndexName <- "quotation-december"
esUrl <- "http://localhost:9200"

period = 30 #days

filterQuery <- '{"range":{
          "date":{
"gte": "2017-12-31"
}
}}'
sortQuery <- '{"date": "asc"}'

data <- queryEs(esUrl, recommendationsIndexName, filterQuery, sortQuery)
target.xts <- xts(x = data$target, order.by = as.Date(data$date))
symbol.xts <- xts(x = data$symbol, order.by = as.Date(data$date))
publisher.xts <- xts(x = data$publisher, order.by = as.Date(data$date))

queryTemplate <- '{"bool": {
            "must": [
{
  "term": {
  "symbol": "%s"
  }
},
  {
  "range": {
  "date": {
  "gte": "%s",
  "lte": "%s||+%sd"
  }
  }
  }
  ]
  }}'
sortQuery <- '{"date": "asc"}'



#transactions <- lapply(data, function(s){
#  print(s)
#})


simulateStrategy <- function (publisher){
  df <- subset(data, publisher == publisher)
  for(row in 1:nrow(df)) {
    symbol <- df[row, 'symbol']
    date <- df[row, 'date']
    filterQuery <- sprintf(queryTemplate, symbol, date, date, period)
    quotations <- queryEs(esUrl, quotationsIndexName, filterQuery, sortQuery)
    if(is.na(quotations)){
      return(NA)
    }
    max <- max(quotations$high, na.rm = TRUE)
    target <- date <- df[row, 'target']
    diff <- max - target
    percentageDiff <- diff/target*100
  }
  
  
  
  
}

for(publisher in unique(data[1:nrow(data),'publisher'])) 
{
  simulateStrategy(publisher)
}    

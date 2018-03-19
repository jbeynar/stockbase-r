library("elasticsearchr")

esUrl <- 'http://localhost:9200'
indexName <- 'stockbase-finance'
symbol <- 'CD-PROJEKT'

filterQuery <- sprintf('{
    "bool": {
      "must": [
        {
          "term": {
            "symbol": "%s"
          }
        },
                	{
                    "range":{
                      "date":{
                        "gte": "2014-01-01",
                        "lte": "2016-12-31"
                      }
                    }
                  }
        ]
    }
}', symbol)
  
match <- query(filterQuery)
sort <- sort_on('{"date": "asc"}')

data <- tryCatch({
  elastic(esUrl, indexName, indexName) %search% (match + sort)
}, error = function(e){ print(e) })


plot(data$ebit ~ as.Date(data$date), 
     xlab="Czas", 
     ylab = "EBIT", 
     type='b', 
     main=sprintf("EBIT kwartalny %s", symbol))
grid(10, 10, lwd = 2)
abline(lm(data$ebit ~ as.Date(data$date)), lty=3, col="blue")




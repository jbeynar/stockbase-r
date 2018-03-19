library("elasticsearchr")

queryEs <- function(esUrl, indexName, filterQuery, sortQuery){
    match <- query(filterQuery)
    sortQuery <- sort_on(sortQuery)

    data <- tryCatch({
        elastic(esUrl, indexName, indexName) %search% (match + sortQuery)
    }, error = function(e){

        # TODO adjust it error handling
        print(e)
        return(NA)

    })
    data
}

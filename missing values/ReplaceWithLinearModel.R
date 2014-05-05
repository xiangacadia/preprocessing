## initialization
#  from which column to start indexing
startColumn <- 3
#  the NA string
NA_str <- 'x'
#  which column to stop preprocessing
endColumn <- ncol(data)
#  which row to start processing
startRow <- 1
#  which row to stop processing
endRow <- nrow(data)

## initialize result
result <- data.frame()

for(rowNum in c(startRow : endRow)){
    startRow <- 1
    endRow <- 1
    rowNum <- 1
    
    ## get data for each series
    series <- data[rowNum,]
    
    ## replace un-numeric values with NA
    series <- replace(series, c(1:endColumn)[series==NA_str], NA)
    
    ## to-do: there should be more check for numeric values in case there is other NA strings
    
    ## build linear model and get prediction
    prediction <- predict(lm((series[startColumn:endColumn], use.names = FALSE) ~ c(startColumn:endColumn), data=series), data.frame(c(startColumn:endColumn)), se.fit = TRUE)
    lm(split(series[startColumn:endColumn], rownames(series[startColumn:endColumn])) ~ as.matrix(c(startColumn:endColumn)), data=series)
    lm((series[startColumn:endColumn]) ~ as.matrix(c(startColumn:endColumn)), data=series)
    
    a <- split(series[startColumn:endColumn], rownames(series[startColumn:endColumn]))
    lm(as.matrix(a) ~ data.frame(c(startColumn:endColumn)),  data=series)
    
    ## put data together
    complete_series <- series
    #names(complete_series) <- names(series)
    complete_series[is.na(series)] <- prediction$fit[c(1:endColumn)[is.na(series)] - startColumn]
    
    ## rewrite the original dataset
    result <- rbind(result, complete_series)
}
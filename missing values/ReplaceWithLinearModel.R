classes <- rep("character",19)
data <- read.csv("/Users/xiangjiang/Documents/temp/GDP/1.[original]GDP_new.csv", colClasses=classes)

## initialization
#  from which column to start indexing
startColumn <- 3
#  which column to stop preprocessing
endColumn <- ncol(data)
#  which row to start processing
startRow <- 1
#  which row to stop processing
endRow <- nrow(data)

## possible min and max value
maxValue <- .Machine$integer.max
minValue <- 0

## initialize result
result <- data.frame()

for(rowNum in c(startRow : endRow)){
    
    #rowNum <- 4
    
    ## get data for each series
    series <- as.integer(data[rowNum,])
    
    ## test if all values in series are NA
    if(length(series[!is.na(series)[startColumn:endColumn]]) == 0){
        complete_series <- append(rep(NA,startColumn-1), rep(0, endRow - startRow))
    }else{
        ## test if all values in the series is not NA
        if(length(series[is.na(series)[startColumn:endColumn]]) == 0){
            complete_series <- series
        }else{
            
            ## replace un-numeric values with NA
            # series <- replace(series, c(1:endColumn)[series==NA_str], NA)
            
            ## to-do: there should be more check for numeric values in case there is other NA strings
            
            ## build linear model and get prediction
            prediction <- predict(lm(series[startColumn:endColumn] ~ c(startColumn:endColumn)), data.frame(c(startColumn:endColumn)), se.fit = TRUE)
            
            ## put data together
            complete_series <- series
            complete_series[is.na(series)][startColumn:endColumn] <- prediction$fit[is.na(series)[startColumn:endColumn]]
            
            complete_series[startColumn:endColumn][complete_series[startColumn:endColumn] < minValue] <- minValue
            complete_series[startColumn:endColumn][complete_series[startColumn:endColumn] > maxValue] <- maxValue
        }
    }
    
    ## rewrite the original dataset
    result <- rbind(result, complete_series)
    
}

## rewrite the names of the columns
names(result) <- names(data)

## rewrite values beyond replacing
result <- data.frame(result)
result <-cbind(data[,1:startColumn-1],result[startColumn:endColumn])

write.csv(result, "/Users/xiangjiang/Documents/temp/GDP/2.[no missing value]GDP_new.csv", row.names = FALSE)
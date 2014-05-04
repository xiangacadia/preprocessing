## initialization
#  from which column to start indexing
startColumn <- 3
#  the NA string
NA_str <- 'x'

## get data for each series
series <- data[1,startColumn:ncol(data)]

## replace un-numeric values with NA
series <- replace(series, c(1:ncol(series))[series==NA_str], NA)

## to-do: there should be more check for numeric values in case there is other NA strings

## build linear model and get prediction
prediction <- predict(lm(unlist(series, use.names = FALSE) ~ c(1:ncol(series)), data=series), data.frame(c(1:ncol(series))), se.fit = TRUE)

## put data together
series[is.na(series)] <- prediction$fit[is.na(series)]
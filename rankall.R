# definately mistakes in this code. best and worst cases not done correctly. Got too complicated.

sort_by_columns_1 <- function (data, col1 = 17, col2 = 2){
        orderdata <- data[order(data[,col1],data[,col2]),]
        return(orderdata)
}

sort_by_columns_2 <- function (data, col1 = 17, col2 = 2, decreasing = TRUE){
        orderdata <- data[order(data[,col1],data[,col2]),]
        return(orderdata)
}




returner <- function(l,num){
        l[2][num,]
}

rankall <- function(o,num=1){
        
        if (o == "heart attack"){i = 11}
        else if (o == "heart failure") {i = 17} 
        else {i = 23}
        
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        m <- c(11,17,23)
        suppressWarnings(outcome[,m] <- lapply(outcome[,m], as.numeric))
        
        ans <- split(outcome, f = outcome$State)
        
        if (num == "worst") {l <- lapply(ans, sort_by_columns_2, col1 = i)}
        else {l <- lapply(ans, sort_by_columns_1, col1 = i)}
        
        p <- lapply(l, returner, num)
        df <- as.data.frame(p)
        t(df)
        #test <- sort_by_columns(ans$TX,17,2)
        
}

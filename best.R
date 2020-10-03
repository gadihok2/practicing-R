
best <- function(state, o){
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #try(if(state %in% outcome$State == FALSE) stop("invalid state"))
        os = c("heart attack", "heart failure", "pneumonia")
        if (state %in% outcome$State){
                if (o %in% os){
                        state_sample <- state
                        df_hn <- split(outcome[,2],outcome$State)
                        l <- c(11,17,23)
                        suppressWarnings(outcome[,l] <- lapply(outcome[,l], as.numeric))
                        if (o == "heart attack"){
                                #outcome[, 11] <- as.numeric(outcome[, 11])
                                df_mr <- split(outcome[,11],outcome$State)
                        } else if (o == "heart failure") {
                                #outcome[, 17] <- as.numeric(outcome[, 17])
                                df_mr <- split(outcome[,17],outcome$State)
                        } else {
                                #outcome[, 23] <- as.numeric(outcome[, 23])
                                df_mr <- split(outcome[,23],outcome$State)
                        }
                        df1 <- df_mr[[state_sample]]
                        df2 <- df_hn[[state_sample]]
                        index <- which(df1 == min(df1, na.rm=TRUE))
                        df2[index]
                } else {stop("outcome")}
        } else {stop("invalid state")}
}



#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#head(outcome)
#names(outcome)
#ncol(outcome)
#nrow(outcome)

#outcome[, 11] <- as.numeric(outcome[, 11])

#Because we originally read the data in as character (by specifying colClasses = "character" 
#we need tocoerce the column to be numeric

#hist(outcome[, 11])






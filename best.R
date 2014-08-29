best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state is valid
        if (is.na(match(state, data$State)) == TRUE) stop("invalid state")
	
	## Check that outcome is valid and store column number if so
	if (outcome == "heart attack") {
		outcome.col <- 11
	}
	else if (outcome == "heart failure") {
		outcome.col <- 17
	}
	else if (outcome == "pneumonia") {
		outcome.col <- 23
	}
	else stop("invalid outcome")
	
	## Subset by state and eliminate hospitals with no data
        complete <- data$State == state & (data[, outcome.col]) != "Not Available"
	state.outcome <- subset(data, complete, c(2, outcome.col))
	
	## Return hospital name in that state with lowest 30-day death
        ## rate
        state.outcome[, 2] <- as.numeric(state.outcome[, 2])
        minimum <- which(min(state.outcome[, 2]) == state.outcome[, 2])
	alphabetical <- sort(state.outcome[minimum, 1])
	alphabetical[1]
}
best <- function(state = "TX", outcome = "heart attack") {
        
        # Import csv file and sort by selected state
        Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Create list of valid state & outcome
        Valid <- c("heart attack", "heart failure", "pneumonia")
        
        # Verify function input
        if (!state %in% Outcome$State) {
                stop("invalid state")
        } else if (!outcome %in% Valid) {
                stop("invalid outcome")
        } else {
                
                Outcome <- subset(Outcome, State == state)
                
                # Determine outcome
                if (outcome == "heart attack") {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,11])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        # Select hospital holding the lowest mortality rate
                        Result <- Data.df[1,1]
                        
                } else if (outcome == "heart failure") {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,17])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        # Select hospital holding the lowest mortality rate
                        Result <- Data.df[1,1]
                        
                } else {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,23])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        # Select hospital holding the lowest mortality rate
                        Result <- Data.df[1,1]
                        
                }}
        
        Result
}

rankhospital <- function(state = "TX", outcome = "heart attack", num = "best") {
        
        # Import csv file and sort by selected state
        Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Create list of valid state & outcome
        Valid <- c("heart attack", "heart failure", "pneumonia")
        
        # Verify function input
        if (!state %in% Outcome$State) {
                stop("invalid state")
        } else if (!outcome %in% Valid) {
                stop("invalid outcome")
        } else {
                
                Outcome <- subset(Outcome, State == state)
                
                # Determine outcome
                if (outcome == "heart attack") {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,11])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        # Create rank
                        if (num == "best") {
                                Rank <- 1
                        } else if (num == "worst") {
                                Rank <- nrow(Data.df)
                        } else {
                                Rank <- num
                        }
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        
                        # Select hospital holding according to predetermined rank
                        Result <- Data.df[Rank,1]
                        
                } else if (outcome == "heart failure") {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,17])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        # Create rank
                        if (num == "best") {
                                Rank <- 1
                        } else if (num == "worst") {
                                Rank <- nrow(Data.df)
                        } else {
                                Rank <- num
                        }
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        
                        # Select hospital holding according to predetermined rank
                        Result <- Data.df[Rank,1]
                        
                } else {
                        
                        Data.df <- data.frame(Outcome[,2], Outcome[,23])
                        colnames(Data.df) <- c("Hospital_Name", "Mortality")
                        
                        Data.df <- subset(Data.df, Mortality != "Not Available")
                        
                        # Create rank
                        if (num == "best") {
                                Rank <- 1
                        } else if (num == "worst") {
                                Rank <- nrow(Data.df)
                        } else {
                                Rank <- num
                        }
                        
                        Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                        Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                        
                        Data.df <- Data.df[order(Data.df[,2], Data.df[,1]),]
                        
                        # Select hospital holding according to predetermined rank
                        Result <- Data.df[Rank,1]
                        
                }}
        
        Result
}

rankall <- function(outcome = "heart attack", num = "best") {
        
        # Import csv file and sort by selected state
        Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Create list of valid state & outcome
        Valid <- c("heart attack", "heart failure", "pneumonia")
        
        # Verify function input
        if (!outcome %in% Valid) {
                stop("invalid outcome")
        } else {
                
                Output <- data.frame()
                State_list <- unique(Outcome$State)
                
                for (i in 1:length(State_list)) {
                        
                        # Determine outcome
                        if (outcome == "heart attack") {
                                
                                Data.df <- subset(Outcome, State == State_list[i])
                                Data.df <- data.frame(Data.df[,2], Data.df[,7], Data.df[,11])
                                colnames(Data.df) <- c("Hospital_Name", "State", "Mortality")
                                
                                Data.df <- subset(Data.df, Mortality != "Not Available")
                                
                                # Create rank
                                if (num == "best") {
                                        Rank <- 1
                                } else if (num == "worst") {
                                        Rank <- nrow(Data.df)
                                } else {
                                        Rank <- num
                                }
                                
                                Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                                Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                                
                                Data.df <- Data.df[order(Data.df[,3], Data.df[,1]),]
                                
                                # Select hospital holding the lowest mortality rate
                                Result <- Data.df[Rank,1:2]
                                Output <- rbind(Output, Result)
                                
                        } else if (outcome == "heart failure") {
                                
                                Data.df <- subset(Outcome, State == State_list[i])
                                Data.df <- data.frame(Data.df[,2], Data.df[,7], Data.df[,17])
                                colnames(Data.df) <- c("Hospital_Name", "State", "Mortality")
                                
                                Data.df <- subset(Data.df, Mortality != "Not Available")
                                
                                # Create rank
                                if (num == "best") {
                                        Rank <- 1
                                } else if (num == "worst") {
                                        Rank <- nrow(Data.df)
                                } else {
                                        Rank <- num
                                }
                                
                                Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                                Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                                
                                Data.df <- Data.df[order(Data.df[,3], Data.df[,1]),]
                                
                                # Select hospital holding the lowest mortality rate
                                Result <- Data.df[Rank,1:2]
                                Output <- rbind(Output, Result)
                                
                        } else {
                                
                                Data.df <- subset(Outcome, State == State_list[i])
                                Data.df <- data.frame(Data.df[,2], Data.df[,7], Data.df[,23])
                                colnames(Data.df) <- c("Hospital_Name", "State", "Mortality")
                                
                                Data.df <- subset(Data.df, Mortality != "Not Available")
                                
                                # Create rank
                                if (num == "best") {
                                        Rank <- 1
                                } else if (num == "worst") {
                                        Rank <- nrow(Data.df)
                                } else {
                                        Rank <- num
                                }
                                
                                Data.df$Hospital_Name <- as.character(Data.df$Hospital_Name)
                                Data.df$Mortality <- as.numeric(as.character(Data.df$Mortality))
                                
                                Data.df <- Data.df[order(Data.df[,3], Data.df[,1]),]
                                
                                # Select hospital holding the lowest mortality rate
                                Result <- Data.df[Rank,1:2]
                                Output <- rbind(Output, Result)
                                
                        }}}
        colnames(Output) <- c("Hospital_Name", "State")
        Output$State <- as.character(Output$State)
        Output <- Output[order(Output[,2]),]        
        Output
}
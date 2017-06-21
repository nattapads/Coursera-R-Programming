corr <- function(directory,threshold = 0) {
        files_list <- list.files(directory, full.names=TRUE)
        no_of_files <- length(files_list)
        DF <- data.frame()
        for(i in 1:no_of_files) {
                DF <- rbind(DF, read.csv(files_list[i]))
        }
        Nona <- DF[complete.cases(DF),]
        AggNona <- aggregate(sulfate ~ ID, Nona,length)
        MerNa <- merge(Nona,AggNona,by.x = "ID", by.y = "ID")
        Correl <- MerNa[MerNa$sulfate.y>threshold,]
        Output <- by(Correl[,3:4], Correl$ID, function(x) {
                cor(x$sulfate.x, x$nitrate)
        }) 
        Output.df <- as.data.frame(as.matrix(Output))
        Output.df[,1]
}

complete <- function(directory,id = 1:332) {
        files_list <- list.files(directory, full.names=TRUE)
        DF <- data.frame()
        for(i in id) {
                DF <- rbind(DF, read.csv(files_list[i]))
        }
        DF <- DF[complete.cases(DF),]
        Agg <- aggregate(sulfate ~ ID, DF,length)
        colnames(Agg) <- c("id", "nobs")
        Agg
}

pollutantmean <- function(directory,pollutant,id = 1:332) {
        files_list <- list.files(directory, full.names=TRUE)
        DF <- data.frame()
        for(i in id) {
                DF <- rbind(DF, read.csv(files_list[i]))
        }
        means <- mean(DF[,pollutant], na.rm = TRUE)
        means
}
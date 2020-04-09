pollutantmean <- function(directory, pollutant, id = 1:332){
  filelist <- list.files(directory, pattern = ".csv", full.names = TRUE)
  
  means = c()
  
  for(i in id){
    file <- read.csv(filelist[i])
    
    x <- mean(file[[pollutant]], na.rm = TRUE)
    means <- c(means, x)
    
  }
  mean(means)
}

complete <- function(directory, id = 1:332){
  filelist <- list.files(directory, pattern = ".csv", full.names = TRUE)
  list <- matrix(nrow = length(id), ncol = 2)
  
  for(i in 1:length(id)){
    file <- read.csv(filelist[id[i]])
    
    list[i, 1] <- id[i]
    list[i, 2] <- nrow(na.exclude(file))
  }
  
  df <- data.frame(list)
  df
}

corr <- function(directory, threshold = 0){
  filelist <- list.files(directory, pattern = ".csv", full.names = TRUE)
  corrlist <- c()
  
  for(i in 1:length(filelist)){
    file <- read.csv(filelist[i])
    noNA <- na.exclude(file)
    
    if(threshold <= nrow(noNA)){
      corrlist <- c(corrlist, cor(noNA$nitrate, noNA$sulfate))
    }
    
  }
  corrlist
}
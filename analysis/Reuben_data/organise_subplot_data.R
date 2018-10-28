################################################################################
#' @tile Organise subplot data 
#' @title James Margrove
#' @discription Organising the subplots and elevation data from the plot 

rm(list=ls())



abc <- list(v1 = (c("A", "B", "C", "D", "E", 
                    "J", "I", "H", "G", "F", 
                    "K", "L", "M", "N", "O", 
                    "T", "S", "R", "Q", "P", 
                    "U", "V", "W", "X", "Y")),
            v2 = (c("A", "J", "K", "T", "U", 
                    "B", "I", "L", "S", "V", 
                    "C", "H", "M", "R", "W", 
                    "D", "G", "N", "Q", "X", 
                    "E", "F", "O", "P", "Y")),
            v3 = (c("E", "D", "C", "B", "A", 
                    "F", "G", "H", "I", "J", 
                    "O", "N", "M", "L", "K", 
                    "P", "Q", "R", "S", "T", 
                    "Y", "X", "W", "V", "U")),
            v4 = (c("Y", "X", "W", "V", "U", 
                    "P", "Q", "R", "S", "T", 
                    "O", "N", "M", "L", "K", 
                    "F", "G", "H", "I", "J", 
                    "E", "D", "C", "B", "A")),
            v5 = (c("U", "T", "K", "J", "A", 
                    "V", "S", "L", "I", "B", 
                    "W", "R", "M", "H", "C", 
                    "X", "Q", "N", "G", "D", 
                    "Y", "P", "O", "F", "E")),
            v6 = (c("U", "V", "W", "X", "Y", 
                    "T", "S", "R", "Q", "P", 
                    "K", "L", "M", "N", "O", 
                    "J", "I", "H", "G", "F", 
                    "A", "B", "C", "D", "E"))
            )


abc$v7 <- matrix(LETTERS[1:25], nrow = 5)
abc$v8 <- rev(abc$v7)
abc$v9 <- t(abc$v7)
abc$v10 <- rev(abc$v8)
abc$v11 <- matrix(LETTERS[1:25], nrow = 5, byrow = T)
abc$v12 <- t(abc$v11)
abc$v13 <- rev(abc$v12)

abc$v1


require(ggplot2)
plot_corners <- read.table("./data/Reuben_data/Copy of Sepilok 4ha plots final.txt", header = TRUE)
source("./functions/quadSplit.R")
source("./functions/quaterPoly.R")
data <- data.frame(x = as.numeric(), 
                   y = as.numeric(),
                   Q = as.character(), 
                   `10x10` = as.numeric(),
                   `20x20` = as.character(),
                   ha4plot = as.character(), 
                   Forest = as.character(),
                   unqeID = as.character())


#for(k in c("A1","A2", "A3", "S1", "S2", "S3")){
  k = "S3"
  A1plots = quadSplit(plot_corners[plot_corners$Plot == k, c(5,6)], n = 2, graph = TRUE)

  flip <- function(data){
    #data <- A1plot1A
    mx <- with(data, tapply(x, Q, mean))
    my <- with(data, tapply(y, Q, mean))
    coords <- data.frame(mx, my)
    alpha = c(0,0,0,0)
    alpha[which(coords[,1] %in% (sort(coords[,1])[1:2]) & coords[,2] %in% (sort(coords[,2])[3:4]))] <- "4"
    alpha[which(coords[,1] %in% (sort(coords[,1])[3:4]) & coords[,2] %in% (sort(coords[,2])[3:4]))] <- "1"
    alpha[which(coords[,1] %in% (sort(coords[,1])[3:4]) & coords[,2] %in% (sort(coords[,2])[1:2]))] <- "2"
    alpha[which(coords[,1] %in% (sort(coords[,1])[1:2]) & coords[,2] %in% (sort(coords[,2])[1:2]))] <- "3"
    return(alpha)
  }

  
  flipD <- function(data){
    #data <- A1plot1A
    mx <- with(data, tapply(x, Q, mean))
    my <- with(data, tapply(y, Q, mean))
    coords <- data.frame(mx, my)
    alpha = c(0,0,0,0)
    alpha[which(coords[,2] == (max(coords[,2])))] <- "1"
    alpha[which(coords[,1] == max(coords[,1]))] <- "2"
    alpha[which(coords[,2] == min(coords[,2]))] <- "3"
    alpha[which(coords[,1] == min(coords[,1]))] <- "4"
    return(alpha)
  }
  
  
  A1plots$Q <- rep(factor(flipD(A1plots)), each = 4)
  ggplot(A1plots, aes(x = x, y = y, fill = Q)) + geom_polygon()
  
  
  A1plots <- A1plots[order(A1plots$Q),]
  #for(j in c("1", "2", "3", "4")){
    j = "1"
    A1plot1 <- quadSplit(A1plots[A1plots$Q == j, 1:2], n = 5, graph = TRUE)
    

    A1plot1[,"20x20"] <- as.factor(rep(abc$v13, each = 4))
    ggplot(A1plots, aes(x = x, y = y, group = Q)) + geom_polygon() + 
      geom_polygon(data = A1plot1, aes(x = x, y = y, fill = `20x20`))
    
    # order them so it is easer to keep track of all this 
    A1plot1 <- A1plot1[order(A1plot1[,"20x20"]),]
    source("./functions/quaterPoly.R")
    for(i in 1:25){
      #i = 12
      subplotLetter <- LETTERS[i]
      
      A1plot1A <- quadSplit(A1plot1[A1plot1$`20x20` == subplotLetter,1:2], n = 2, graph = TRUE)
      
      ggplot(A1plots, aes(x = x, y = y, group = Q)) + geom_polygon() + 
        geom_polygon(data = A1plot1, aes(x = x, y = y, group = `20x20`)) + 
        geom_polygon(data = A1plot1A, aes(x = x, y = y, fill = Q))
      
        A1plot1A$`10x10` <-  factor(rep(flipD(A1plot1A), each = 4))
        
      ggplot(A1plots, aes(x = x, y = y, group = Q)) + geom_polygon() + 
        geom_polygon(data = A1plot1, aes(x = x, y = y, group = `20x20`)) + 
        geom_polygon(data = A1plot1A, aes(x = x, y = y, fill = `10x10`))
      
      
      A1plot1A <- A1plot1A[order(A1plot1A$`10x10`),]
      A1plot1A$`20x20` <- subplotLetter
      A1plot1A$`ha4plot` <- j
      A1plot1A$`Forest` <- k
      A1plot1A$unqeID <- paste(A1plot1A$`10x10`, A1plot1A$`20x20`, A1plot1A$`ha4plot`, A1plot1A$`Forest`, sep = "")
      data <- rbind(data, A1plot1A)
  }
#  }
#}


ggplot(A1plots, aes(x = x, y = y, group = Q)) + geom_polygon() + 
  geom_polygon(data = A1plot1, aes(x = x, y = y, group = `20x20`)) + 
  geom_polygon(data = data, aes(x = x, y = y, group = unqeID, fill = `10x10`)) + 
  theme(legend.position = "null")




## A1 should be correct now 
#write.table(data, "./data/Reuben_data/plotA110x10.txt")
#write.table(data, "./data/Reuben_data/plotA210x10.txt")
#write.table(data, "./data/Reuben_data/plotA310x10.txt")
#write.table(data, "./data/Reuben_data/plotS110x10.txt")
#write.table(data, "./data/Reuben_data/plotS210x10.txt")
#write.table(data, "./data/Reuben_data/plotS310x10.txt")


plotS210x10 <- read.table("./data/Reuben_data/plotS110x10.txt", header = TRUE)
require(ggplot2)
ggplot(plotS210x10[plotS210x10$ha4plot != "4",], aes(x = x, y = y, fill = unqeID, group = unqeID)) + geom_polygon()+ 
  theme(legend.position = "null")
rm(list=ls())
plotA110x10 <- read.table("./data/Reuben_data/plotA110x10.txt", header = TRUE)
plotA210x10 <- read.table("./data/Reuben_data/plotA210x10.txt", header = TRUE)
#plotA310x10 <- read.table("./data/Reuben_data/plotA310x10.txt", header = TRUE)
plotS110x10 <- read.table("./data/Reuben_data/plotS110x10.txt", header = TRUE)
plotS210x10 <- read.table("./data/Reuben_data/plotS210x10.txt", header = TRUE)
plotS310x10 <- read.table("./data/Reuben_data/plotS310x10.txt", header = TRUE)




data <- rbind(plotA110x10,
  plotA210x10,
  #plotA310x10,
  plotS110x10,
  plotS210x10,
  plotS310x10)

6*10*10*4


write.table(data, file="./data/Reuben_data/plots_elev_10x10.txt")

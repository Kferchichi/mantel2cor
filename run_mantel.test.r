#run the required libraries
library(geodist)
library(ape)
library(vegan)
#read the genetic distance matrix
fst <- read.table(header = T, text = "here you can cpoy your fst matrix", sep = "")
#read the gps matrix
gps <- read.csv("C:/give/the/path/to/gpsdata.csv", row.names = 1)
#convert the genetic distance matrix to  the required class
fst.dist.matrix <- as.dist(fst)
#convert the geographic distance matrix to  the required class
gps.dist.matrix <- as.dist(geodist(gps, measure = "haversine"))
#set the name of the two matrices
names(fst.dist.matrix) <- names(gps.dist.matrix)
#run the matel test
mantel(fst.dist.matrix, gps.dist.matrix, permutations = 100000)
  
#convert the fst matrix object into a non-distributed vector object
 fstvec = as.vector(fst.dist.matrix)
#convert the gps matrix object into a non-distributed vector object
gpsvec = as.vector(gps.dist.matrix)
#create a dataframe with the two vectors objects
 df <- data.frame(x=gpsvec, y=fstvec, Population=c('pop1', 'pop2', 'pop3', 'pop4', 'pop5', 'pop6', 'pop7', 'pop8', 'pop9', 'pop10', 'pop11'))
#plot the correlation scatter plot 
library("ggplot2")
ggplot(df, aes(y = fstvec, x = gpsvec/1000)) + 
          geom_point(size = 2, alpha = 0.75, colour = "black",shape = 16) + 
          geom_smooth(method = "lm", colour = "black", alpha = 0.2) + 
          labs(x = "Geographic Distance (km)", y = "Fst") + 
          theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12), 
                              axis.text.y = element_text(face = "bold", size = 11, colour = "black"), 
                              axis.title= element_text(face = "bold", size = 14, colour = "black"), 
                              panel.background = element_blank(), 
                              panel.border = element_rect(fill = NA, colour = "black"),
                              legend.position = "right", 
                              legend.text = element_text(size = 10, face = "bold"),
                              legend.title = element_text(size = 11, face = "bold")) +
          scale_fill_continuous(high = "black", low = "black")

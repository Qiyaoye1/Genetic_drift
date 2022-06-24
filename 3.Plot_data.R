library(readxl)
library(foreach)
library(ggplot2)
library(reshape2)
library(rstudioapi)

#set your wd to the folder that contains all R files
setwd(dirname(getSourceEditorContext()$path))
source("2.Process_data.R") #import the import_data function


Pop_ploter <- function(Dataset){
   #this function plots the evolution of an entire population
   N_Col <- which(names(Dataset[[1]]) == "X") #column that are useful for plot
   last_gen <- nrow(Dataset[[1]]) 
   
   #creates a df of plot data
   Plot_table <- data.frame(matrix(rep(0, (length(Dataset) + 1) * last_gen), 
      nrow = last_gen))
   names <- c("Gen", paste0("sP", 1:length(Dataset)))
   names(Plot_table) <- names
   Plot_table$Gen <- seq(0, last_gen - 1)
   
   #Pass the X col of all subpop to the df for ploting
   foreach(Plot_col = seq(2, ncol(Plot_table)), sPOP = 1:length(Dataset)) %do%{
      Plot_table[,Plot_col] <- Dataset[[sPOP]][,N_Col]
   }
   
   #reshape for easy plot
   graphic_data <- melt(Plot_table, variable.name = "Graphic", id.vars = "Gen")
   
   #ggplot
   POP_graphic <- ggplot(data = graphic_data, aes(x = Gen, y = value)) + 
      geom_line(aes(colour = Graphic), size = 1)
   

   return(POP_graphic)
}



# Dataset <- import_data("Processed.xlsx")
# Pop_ploter(Dataset = Dataset)
# ggsave("Antonio.png", width = 14, height = 8)#save

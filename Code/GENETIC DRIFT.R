rm(list = ls())
library(rstudioapi)

# THIS FILE CONTAINS ALL THE FUNCTIONS OF OTHER FILES

#set your wd to the folder that contains all R files
setwd(dirname(getSourceEditorContext()$path))

#importing other parts.
source("1.Simulate_Data.R", echo = TRUE) # simulator
source("2.Process_data.R", echo = TRUE) # process
source("3.Plot_data.R", echo = TRUE) # ploter

#simulate a population
Population_simul(N_pop = 30, N_indiv = 30, alelle_N_prob = 5, 
      export_file = TRUE, only_stats = FALSE)

# analyze the simulated population. not argument needed unless you changed
# export file name in the previous function or u dont want to export data
initial_analysis()

#import data again
Data <- import_data("Processed.xlsx")

#create the plot with analyzed population
Plot <- Pop_ploter(Data)
Plot

#save if you want
ggsave("Plot1.png", width = 14, height = 6, limitsize = FALSE)


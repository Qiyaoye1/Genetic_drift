library(writexl)
library(readxl)

# THIS FILE IS FOR PROCESS ALL THE DATA GENERATED WITH THE SIMULATOR FILE
#The excel file should be in the work directory of rdsutio, otherwise indicate it pls


import_data <- function(filename = "Results.xlsx"){
   #this function import a excel file as a list with dfs
   
   #detect number of sheet to import
   number_subpop <- length(excel_sheets(filename))
   Dataset <- vector(mode = "list", length = number_subpop)
   
   #import
   for(i in 1:number_subpop) {
      Dataset[[i]] <- read_excel(filename,
         sheet = paste0("Sheet", i), col_types = "numeric")
   }
   return(Dataset)
}


count_phenos <- function(Data){
    #count how many of each phenotype in each generation for all subpopulations
    df <- Data[[1]]
    cols = ncol(df)
    Gametes <- cols - 1 #columns that involve gametes
    number_subpop <- length(Data) 
    NAMES <- c(names(df), c("NN", "NB", "BB"))
    NEW_DATA = list()
    #loop over every subpopulation
    for (subpop in seq(1, number_subpop)) {
        # loops over every population

        raw <- as.vector(t(Data[[subpop]]))
        formated_POP <- c()
        
        for (gen in seq(1,length(raw),cols)){
            # every gen
            current <- c(raw[gen: (gen + cols - 1)])
            NN <- 0
            BB <- 0
            NB <- 0
            
            gametes <- current[2: (cols - 1)]
            for(gamete in seq(1, length(gametes), 2)){
                # every 2 gametes and append
                current_gamete <- gametes[gamete: (gamete + 1)]
                fenotipe = sum(current_gamete)
            
                if (fenotipe == 2){NN <- NN + 1}
                else if(fenotipe == 1 ){NB <- NB + 1}
                else{BB <- BB + 1}
                }
            current <- append(current, c(NN,NB,BB))
            formated_POP <- append(formated_POP, current)
            }
        POP <- data.frame(matrix(formated_POP, byrow = T, ncol = cols + 3))
        colnames(POP) <- NAMES
        NEW_DATA <- append(NEW_DATA, list(POP))
    }
    return(NEW_DATA)
}


Generation_equalizer <- function(Data){
   #equalizes all the population to the same lenght
   
   number_subpop <- length(Data)
   #check longest population
   rows1 <-c()
   
   #create a vector with lenghts of all population
   for (i in 1:number_subpop){
      rows1 <- as.vector(append(rows1, nrow(Data[[i]])))
   }
   
   longestP <- max(rows1)
   
   #equalize the number of generations in all subpopulations
   for (i in 1:number_subpop){
      #appending last row until length equal to longest population
        POP <- Data[[i]]
        Rows <- nrow(POP)
        Difference <- longestP - Rows
        if (Difference == 0){next}
        
        Rest_data <- POP[rep(Rows, Difference),]
        Data[[i]][(Rows + 1):longestP,] <- Rest_data
        Data[[i]]$gen <- 0:(longestP -1)
        rownames(Data[[i]]) <- rep(1:longestP)
        }
   
   return(Data)
}


initial_analysis <- function(filename = "Results.xlsx", 
   export_data = TRUE){
   
   Plist <- import_data(filename) #import
   Plist1 <- count_phenos(Plist) #count
   Plist2 <- Generation_equalizer(Plist1) #equalize
   
   if(export_data == TRUE){write_xlsx(Plist2 ,"Processed.xlsx")} #export
   return(Plist2)
}


#examples
# (Data <- import_data("Results.xlsx"))
# (Data1 <- count_phenos(Data))
# (Data2 <- Generation_equalizer(Data1))
# 
# (hola <- initial_analysis("Results.xlsx")) #do the 3 before in 1 line






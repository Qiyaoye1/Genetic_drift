library(writexl)

#THIS FILE CONTAINS THE CODE FOR SIMULATE SOME POPULATIONS BY THE EFFECT OF GENETIC DRIFT

pop_Generator <- function(N_indiv = 5,
    alelle_N_prob = 5) {
    total_gametes <- N_indiv * 2
    alele_N <- round((alelle_N_prob * 10 * N_indiv * 2) / 100)
    alele_B <- round((N_indiv * 2) - alele_N)
    
    gametes <- rep(c(1, 0), c(alele_N, alele_B))
    POP <-c(0, gametes)
    POP <- append(POP, sum(POP[1:length(POP)]))
    NAMES <- c("gen", paste0("E", 1:total_gametes), "X")
    names(POP) <- NAMES
    
    return(POP)
}


simul1gen <- function(POP, 
    Number_black = NULL, 
    N_gen = 1, 
    N_indiv = 5){
    #this function simulates next generation for a given subpoplation

    gamete_pool_size <- N_indiv * 2
    last_gen <- tail(POP, n = (gamete_pool_size) + 2)
    
    #calculate probability
    if (missing(Number_black)){
        Number_black <- sum(last_gen[2:(1+gamete_pool_size)])
    }
    probN <- Number_black / gamete_pool_size * 100
    probB <- (100 - probN)
    
    #samples. Simulating the individual for next generation.
    next_gen <- sample(c(0, 1), size = gamete_pool_size, replace = TRUE,
        prob = c(probB, probN))
    
    #Append the new generation.
    total <- sum(next_gen)
    
    next_gen <- c(N_gen, next_gen, total)
    NAMES <- c("gen", paste0("E", 1:gamete_pool_size), "X")
    names(next_gen) <- NAMES
    
    POP <- append(POP, next_gen)
    
    if (total == 0 | total == gamete_pool_size){
        POP <- data.frame(matrix(POP, ncol = length(last_gen), byrow = T))
        names(POP) <- NAMES
        return(POP)}
    else{POP <- simul1gen(POP, total, N_gen = N_gen + 1, N_indiv = N_indiv)}
}


Population_simul <- function(N_pop = 6, #number of subpopulations
   export_file = FALSE, # generate a excel file. 
    # Don't export more than 500 subpopulations
   only_stats = FALSE, # Return only stats.
   filename = "Results", # customize filename if you want
   N_indiv = 5, # Population size
   alelle_N_prob = 5 # Frequency of alelle N, in a scale of 1-10.
   ){
   
   #creates a vector with the number of subpopulations and names
   POPlist <- vector(mode = "list", length = N_pop)
   NAMES <- c("gen", paste0("E", 1:(N_indiv * 2)), "X")
   
   #set initial population. 
   POP <- pop_Generator(N_indiv, alelle_N_prob)
   
   #Save initial table, later use
   init_POP <- POP
   
   #only for statistics. 
   B <- 0 #white
   N <- 0 #black
   
   #fixing time of every subpopulation
   Fixed_gen <- c()

   #Loop for every subpopulation
   for (i in seq(1, N_pop)){
         
         #simulates new generation until a phenotype is fixed
         POP <- simul1gen(POP, N_indiv = N_indiv)
         
         #stats
         if (POP[nrow(POP),]$X == N_indiv * 2) {N <- N+ 1}
         else {B <- B + 1}
         Fixed_gen <- append(Fixed_gen, nrow(POP))
         
         #append the new subpopulation to the list
         POPlist[[i]] <- POP
         
         #reset subpopulation 
         POP <- init_POP}

   #stats
   Average_fixed <- mean(Fixed_gen)
   Max_fixed <- max(Fixed_gen)
   Min_fixed <- min(Fixed_gen)
   
   #creates a df with the stats
   Statistics <- data.frame(N, B, Average_fixed, Min_fixed, Max_fixed)
   names(Statistics) <- c("N", "B", "Mean gen", "Fastest fix", "slowest fix")
   
   #export result
   if(export_file == TRUE) {
      write_xlsx(POPlist , paste0(filename,".xlsx"))}
   
   #exit
   if(only_stats == FALSE) {POPlist[[N_pop + 1]] <- Statistics
      return(POPlist)}
   else {return(Statistics)}
}



##examples
# Export <- Population_simul(N_pop = 25, export_file = T, N_indiv = 8, only_stats = T) #check file
# 
# POP1 <- Population_simul(N_pop = 1000, export_file = FALSE, N_indiv = 10,
#    only_stats = T)
# 
# POP2 <- Population_simul(N_pop = 100, export_file = FALSE, N_indiv = 10,
#     only_stats = F)



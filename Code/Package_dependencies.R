packages <- c("writexl", "readxl", "foreach", "ggplot2", "reshape2", "rstudioapi")

for (package in packages){
    if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
}

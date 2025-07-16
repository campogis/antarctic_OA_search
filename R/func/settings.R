################################################
# Settings for indicator extraction and analysis
# working directory
# packages
################################################

## Settings----
settings <- function(){

  # 1. Type the working directory of your choose 
  workdir <- your_dir # already set in previous file
  # workdir <- 'set_your_own_working_directory'
  
  cat('Your working dir: ', workdir, '\n')
  
  # 1.a. Set working directory
  setwd(workdir)
  
  # 2. List of packages needed
  listOfPackages <- c("readr", "dplyr", "stringr", "tidyr", 
                      "data.table", "openxlsx","gtools", 
                      "openalexR", "ggplot2"
                      #,"pbmcapply", "targets", "diffviewer"
                      )
  

  # 3.a. Install Packages  (if needed)
  for (i in listOfPackages){
    if(! i %in% installed.packages()){
      print('Installing packages')
      install.packages(i, dependencies = TRUE)

    }
  }
  
  # Instal internal IPBES package
  #library(devtools)
  #install_github("IPBES-Data/IPBES.R")
  

  cat('All packages installed')

  }  

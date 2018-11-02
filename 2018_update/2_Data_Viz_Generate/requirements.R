# Requires: R package
# Modifies: Nothing
# Effects: Installs package 
install_pkgs <- function(pkg) { 
  
  # Install package if it isn't already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cran.us.r-project.org')
  }
  
  library(pkg, character.only = TRUE)
  
  
} # end install_pkgs()



# load library packages
pkg_list <- c("trend", "plyr", "scales", "MASS", "reshape", "tools", "plotly", "rjson",
              "data.table", "gridExtra", "psych", "dplyr", "dbplyr", "extrafont")
lapply(pkg_list, install_pkgs)
# add ggplot2 after setting device
library(ggplot2)

# Note: These font import steps from the extrafont database only need to be run once
if(nrow(fonttable()) == 0){
  print("Importing fonts for the first time - this may take a few minutes")
  font_import()
  loadfonts(device = "pdf")
}

# set color scheme
cyan <- rgb (0, 123, 188,	maxColorValue = 255)
darkRed <- rgb (208, 32, 47, maxColorValue = 255)
darkBlue <- rgb (0, 66, 118	, maxColorValue=255)
darkGreen <- rgb (91, 140, 41, maxColorValue=255)
darkPurple <- rgb (135, 30, 110, maxColorValue=255)
darkGrey <- rgb (51, 54, 58, maxColorValue=255)	
lightGrey <- rgb (152, 152, 152, maxColorValue=255)




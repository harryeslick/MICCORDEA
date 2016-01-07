################################################################################
# title         : Get_Ricepest_Output.R;
# purpose       : download EPIRICE disease severity files from Figshare;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, Jan 2016;
# inputs        : ;
# outputs       : 2050, 2030 and base folders with EPIRICE data files;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

download_RICEPEST_output <- function(){
  tf <- tempfile()

  if(length(list.files(path = "../Data/RICEPEST Modified GPS3 Output")) != 73){
    download.file("https://ndownloader.figshare.com/files/3662088",
                  tf, method = "wb")
    unzip(tf, exdir = "../Data", overwrite = TRUE)
  }
}

# eos
################################################################################
# title         : Get_Data.R;
# purpose       : download MICCORDEA data files from Figshare;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, Jan 2016;
# inputs        : ;
# outputs       : Folder of data files to use with these scripts;
# remarks 1     : ;
# Licence:      : GPL2;
################################################################################

# Libraries --------------------------------------------------------------------
library(R.utils)

# Download data ----------------------------------------------------------------

download_data <- function(){
  tf <- tempfile()
  if (!file.exists("../Data.tar")) {
    download.file("https://ndownloader.figshare.com/files/5457443",
                  tf, mode = "wb")
    bunzip2(tf, overwrite = TRUE, destname = "../Data.tar")
    untar("../Data.tar", exdir = "../")
  }
}

# eos

library(installr)
library(aside)
library(data.table)

init_date <- as.Date("130911", "%y%m%d")
final_date <- as.Date("170815", "%y%m%d")

if (!dir.exists("CRANlogs")){
  dir.create("CRANlogs")
}

aside('
      download_RStudio_CRAN_data(START = init_date,
                           END = final_date,
      log_folder = "CRANlogs")
      ')

pkgs <- c("dosresmeta", "hetmeta")
my_RStudio_CRAN_data <- read_RStudio_CRAN_data("CRANlogs")
data_pkgs_tot <- read.table("pkgs_dwnld.txt", stringsAsFactors = FALSE)
data_pkgs <- subset(my_RStudio_CRAN_data, package %in% pkgs)
write.table(data_pkgs, file = "pkgs_dwnld.txt")

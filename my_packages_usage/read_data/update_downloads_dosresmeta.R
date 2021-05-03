library(installr)
library(data.table)

data_pkgs_tot <- read.table("pkgs_dwnld.txt", stringsAsFactors = FALSE)
dates_cran <- as.Date(substr(list.files("CRANlogs"), 1, 10))

if (max(dates_cran) != max(data_pkgs_tot$date)){
  message("The final date does not match with the maximum date in the cran downloads. Please check")
}

init_date2 <- max(dates_cran) + 1
final_date2 <- as.Date("171031", "%y%m%d")

if (!dir.exists("CRANlogs_update")){
  dir.create("CRANlogs_update")
}


download_RStudio_CRAN_data(START = init_date2,
                           END = final_date2,
                           log_folder = "CRANlogs_update")
pkgs <- c("dosresmeta", "hetmeta")
my_RStudio_CRAN_data <- read_RStudio_CRAN_data("CRANlogs_update")
data_pkgs <- rbind(data_pkgs_tot,
                   subset(my_RStudio_CRAN_data, package %in% pkgs))
write.table(data_pkgs, file = "pkgs_dwnld.txt")
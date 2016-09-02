# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# load data using dplyr
loadData <- function() {
   files <- list.files(file.path(responsesDir), full.names = TRUE)
   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
   data <- dplyr::bind_rows(data)
   data
}

# label for mandatory fields
labelMandatory <- function(label) {
   tagList(
      label,
      span("*", class = "mandatory_star")
   )
}

epochTime <- function() {
   as.integer(Sys.time())
}

appCSS <-
   ".mandatory_star { color: red; }
#error { color: red; }"

# create the directory
formName <- "year-semester-basic-info"
responsesDir <- file.path("data", formName)
dir.create(responsesDir, recursive = TRUE, showWarnings = FALSE)

# names of the fields on the form we want to save
fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")
# mandatory fields
fieldsMandatory <- c("name", "favourite_pkg")

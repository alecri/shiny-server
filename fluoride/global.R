# global.R for fluoride shiny app

# packages required
library(readxl)
library(dosresmeta)


# reading data from excel
fluo <- read_excel("Fluoride (2015)-animal-bioassay.xlsx", sheet = 1)

# cleaning data:
# keeping only relevant variables and changing 'seconds' in 'sec'
fluo <- fluo[, -c(3:20, 23:25, 29:32, 34:46, 50)]
fluo$`response units`[fluo$`response units` == "seconds"] <- "sec"
# id variable for plot labels
fluo$id_plot <- with(fluo, paste(`study name`, `endpoint id`, `dose index`,
                                 sep = ", " ))

# 'fluo_wide' for initial forest plot
fluo_wide <- reshape(fluo, direction = "wide",
                     v.names = c("dose", "N", "response", "stdev"),
                     timevar = "dose", idvar = "endpoint id",
                     drop = c("dose index","lower_ci", "upper_ci", "pairwise significant",
                              "percent control mean", "percent control low", 
                              "percent control high", "id_plot"))

# removing those studies with missing standard deviation or tags not in
# 'learning' or 'memory'
fluo <- subset(fluo, (!is.na(stdev)) & (tags %in% c("|learning|", "|memory|")))

# calculating smd and var(smd) (+ reconstructing covariances among smds)
covi <- by(fluo, fluo$`endpoint id`, function(x) 
  covar.smd(y = response, sd = stdev, n = N, measure = "smd", data = x))
fluo$smd <- unlist(lapply(covi, function(v) v$y))
fluo$smd_v <- unlist(lapply(covi, function(v) v$v))
fluo$smd_low <- fluo$smd - 1.96*sqrt(fluo$smd_v)
fluo$smd_upp <- fluo$smd + 1.96*sqrt(fluo$smd_v)

# data for predictions (tabular and graphical)
newdata_tab <- data.frame(dose = seq(0, 60, 10))


# additional function not yet in dosresmeta package
vpc <- function(object){
  v <- object$v
  id <- object$id
  Psi <- object$Psi
  Slist <- object$Slist
  vlist <- lapply(unique(id), function(j) cbind(v[id == j]))
  Z <- model.matrix(object$formula, data = object$model)[, 2:(object$dim$q+1), drop = FALSE]
  Zlist <- lapply(unique(id), function(j)
    Z[id == j, , drop = FALSE])
  if (object$center){
    Zlist <- mapply(function(Z, v){
      scale(Z, Z[v==0, ], scale = FALSE)
    }, Zlist, vlist, SIMPLIFY = FALSE)
    Z <- do.call("rbind", Zlist)
  }
  Zlist <- lapply(Zlist, function(z) z[-1, , drop = FALSE])
  vpclist <- mapply(function(Z, S){
    diag(Z %*% Psi %*% t(Z))/diag(S + Z %*% Psi %*% t(Z))
  }, Zlist, Slist, SIMPLIFY = FALSE)
  vpc <- do.call("c", vpclist)
  vpc
}
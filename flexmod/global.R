themes <- c("theme_base()", "theme_bw()", "theme_classic()", "theme_calc()", "theme_dark()",
           "theme_economist()", "theme_excel()", "theme_few()", "theme_fivethirtyeight()",
           "theme_gdocs()", "theme_gray()", "theme_grey()", "theme_get()", "theme_light()", "theme_hc()", 
           "theme_par()", "theme_pander()", "theme_solarized()", "theme_stata()",
           "theme_tufte()", "theme_wsj()", "theme_void()")
lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F")

## Fitting the chosen model: issue confounding + cox/poisson
fit_model <- function(formula, data, type, PTcond, confounding){
   if (type != "surv"){
      mod <- glm(update(formula, y ~ .), data = data, family = type)
      if (PTcond)
         mod <- update(mod, . ~ . + offset(log(eval(parse(text = input$pt)))))
   } else {
      mod <- coxph(update(formula, Surv(y, eval(parse(text = input$censor))) ~ .), data = data)
   }
   if (!is.null(confounding))
      mod <- update(mod, as.formula(paste(". ~ . +", paste(confounding, collapse = " + "))))
   mod
}

# Number of dose-response coeff (intercept excluded)
numbCoef <- function(model, type, k){
   nmax <- 2 + (k+1-2)*(model == "categorical") + (k+2-2)*(model == "linspl") +
      (k+3-2)*(model == "quadrspl") + (k+4-2)*(model == "cubspl") +
      (k-2)*(model == "rcubspl") + (k+1-2)*(model == "poly") 
   index <- 2:nmax
   if (type == "surv") index <- index - 1
   index
}

# Test for overall dose-response and for non-linearity
test_parm <- function(mod, label, type, k){
   index <- numbCoef(label, type, k)
   overall <- Wald(mod, subset = index)
   nonlinear <- if (label %in% c("categorical", "linear"))
      c(Chisq = NA,  d.f. = NA, P = NA) else
         Wald(mod, subset = index[-1])
   data.frame(model = label, overall = t(overall), nonlinear = t(nonlinear))
}

# Calculate 'partial' prediction
model_pred <- function(mod, formula, label, type, k, xref, int = F){
   X <- model.matrix(formula, data = pred_value)
   if (int == FALSE)
      X <-  t(apply(X, 1, "-", X[which(pred_value$x == xref)[1], , drop = F]))[, -1, drop = F]
   index <- numbCoef(label, type, k)
   if (int == TRUE)
      index <- c(1, index)
   pred <- X %*% coef(mod)[index]
   sepred <- sqrt(diag((X %*% vcov(mod)[index, index] %*% t(X))))
   if (type != "gaussian") pred <- exp(pred)
   ci <- if (type == "gaussian")
      data.frame(lowpred = pred - qnorm(.975)*sepred,
                 upppred = pred + qnorm(.975)*sepred)
   else data.frame(lowpred = exp(log(pred) - qnorm(.975)*sepred),
                   upppred = exp(log(pred) + qnorm(.975)*sepred))
   data.frame(pred, ci)
}

# Add 'partial' prediction to the plot
addPred <- function(name, p, data, ltype, ci, ltypeci, alpha, ciline){
   p1 <- p + geom_line(data = data, aes_string(x = "x", y = paste0(name, ".pred")), 
                      linetype = ltype)
   if (any(ci == name)){
     p1 <- p1 + geom_ribbon(aes_string(ymin = paste0(name, ".lowpred"),
                                       ymax = paste0(name, ".upppred")), alpha = alpha)
     if (any(ciline == TRUE))
       p1 <- p1 + 
         geom_line(aes_string(y = paste0(name, ".lowpred")), linetype = ltypeci) +
         geom_line(aes_string(y = paste0(name, ".upppred")), linetype = ltypeci)
   }
   p1
}

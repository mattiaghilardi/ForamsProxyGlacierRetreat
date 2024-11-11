#' Fit and compare GAM and GLM models
#'
#' Fit GLM and GAM and compares AIC.
#' 
#' @param glm_formula An object of class 'formula' (or one that can be coerced to that class): 
#'                    a symbolic description of the GLM to be fitted
#' @param gam_formula An object of class 'formula' (or one that can be coerced to that class):
#'                    a symbolic description of the GAM to be fitted
#' @param data An object of class 'data.frame' (or one that can be coerced to that class) 
#'             containing all variables used in the model
#' @param family Family distribution. Possible alternatives are: 
#'               'gaussian', 'poisson', 'gamma', 'beta'
#' 
fit_glm_gam <- function(glm_formula, gam_formula, data, family) {
  
  # gaussian
  if (family == "gaussian") {
    GLM <- lm(formula = glm_formula, data = data)
    GAM <- mgcv::gam(formula = gam_formula, data = data, family = "gaussian")
  }
  
  # poisson
  if (family == "poisson") {
    GLM <- glm(formula = glm_formula, data = data, family = "poisson")
    GAM <- mgcv::gam(formula = gam_formula, data = data, family = "poisson")
  }
  
  # beta
  if (family == "beta") {
    GLM <- betareg::betareg(formula = glm_formula, data = data)
    GAM <- mgcv::gam(formula = gam_formula, data = data, family = "betar")
  }
  
  # gamma
  if (family == "gamma") {
    GLM <- glm(formula = glm_formula, data = data, family = Gamma(link = "log"))
    GAM <- mgcv::gam(formula = gam_formula, data = data, family = Gamma(link = "log"))
  }
  
  # model selection
  aicc <- MuMIn::AICc(GLM, GAM)
  aicc$delta_AICc <- if (aicc[1, 2] < aicc[2, 2]) {
    c(0, round(aicc[2, 2] - aicc[1, 2], digits = 2))
  } else {
    c(round(aicc[1, 2] - aicc[2, 2], digits = 2), 0)
  }
  
  if (aicc[1, 3] == 0) {
    print(paste("GLM supported: delta_AICc =", aicc[2, 3]))
  } else {
    print(paste("GAM supported: delta_AICc =", aicc[1, 3]))
  }

  list("GLM" = GLM, "GAM" = GAM, "AICc" = aicc)
}

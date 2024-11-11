#' Plot diversity metrics and nmds1 vs glacier distance
#' 
#' @param model An object of class "lm", "glm", "gam", or "betareg"
#' @param data A data frame
#' @param newdata A data frame for predictions
#' @param y A string containing the column name of the response variable in "data"
#' @param title A string with the plot title
#' @param xlab A string with the title of the x axis
#' @param ylab A string with the title of the y axis
#' @param color Color used for dots and average model fit
#' @param fill Color used for the ribbon depicting the error around the average model fit
#' @param annotation_size Size of annotated text (R2 and p-value)
#'
plot_diversity <- function(model, data, newdata, y, 
                           title = NULL, xlab = NULL, ylab = NULL, 
                           color = "black", fill = "grey30",
                           annotation_size = 3) {
  
  if ("betareg" %in% class(model)) {
    pred <- predict(model, newdata, type = "quantile", at = c(0.025, 0.5, 0.975))
    pred <- as.data.frame(pred)
    names(pred) <- c("lower", "fit", "upper")
    pred <- cbind(newdata, pred)
  } else {
    pred <- predict(model, newdata, type = "response", se = TRUE)
    pred <- cbind(newdata, pred)
    pred$lower <- pred$fit - (2 * pred$se.fit)
    pred$upper <- pred$fit + (2 * pred$se.fit)
  }
  
  p <- ggplot() +
    geom_point(data = data, aes(x = glacier_dist, y = .data[[y]]), color = color) + 
    geom_line(data = pred, aes(x = glacier_dist, y = fit), color = color) +
    geom_ribbon(data = pred, aes(x = glacier_dist, ymin = lower, ymax = upper), fill = fill, alpha = 0.2) +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          panel.grid = element_blank())
  
  if (!is.null(xlab)) p <- p + xlab(xlab)
  
  if (!is.null(ylab)) p <- p + ylab(ylab)
  
  if (!is.null(title)) p <- p + labs(title = title) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  yrng <- range(data[, y])
  xrng <- range(data[, "glacier_dist"])
  sm <- summary(model)
  
  if ("gam" %in% class(model)) {
    r2 <- round(sm$r.sq, 2)
    s_p <- sm$s.table["s(glacier_dist)", 4]
    if (s_p >= 0.001) {
      s_p <- paste("=", round(s_p, 3))
    } else {
      s_p <- "< 0.001"
    }
    label <- paste0("*adjR<sup> 2</sup>* = ", r2, "<br>*p* ", s_p)
  } else if ("betareg" %in% class(model)) {
    r2 <- round(sm$pseudo.r.squared, 2)
    b_p <- sm$coefficients$mean["glacier_dist", 4]
    if (b_p >= 0.001) {
      b_p <- paste("=", round(b_p, 3))
    } else {
      b_p <- "< 0.001"
    }
    label <- paste0("*pseudoR<sup> 2</sup>* = ", r2, "<br>*p* ", b_p)
  }
  
  p + annotate("richtext", x = -Inf, y = Inf, label = label,
               hjust = 0, vjust = 1, size = annotation_size, 
               label.colour = NA, fill = NA)
}

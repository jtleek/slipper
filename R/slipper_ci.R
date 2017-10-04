#' Bootstrap a function that returns a single number and
#' return a non-parametric confidence interval.
#'
#' Takes a data frame, a function that returns a single number,
#' and a number of replicates and returns a data frame with the
#' function calculated on the observed sample and on B bootstrap
#' samples.
#'
#' @param df A data frame
#' @param expr A an expression with a function that operates on one or more variables from the data frame.
#' @param B the number of bootstrap samples to draw
#' @param lower the lower percentile for the confidence interval (default 2.5%)
#' @param upper the upper percentile for the confidence interval (default 97.5%)
#'
#' @return confint A vector with the lower bound and upper bound 
#'
#'
#' @examples
#'
#' # Boostrap the mean of the mpg variable in the mtcars data set and get a confidence
#' # interval 
#' slipper_ci(mtcars,mean(mpg),B=100)
#'
#' # Bootstrap the mean of the mpg variable with piping
#' mtcars %>% slipper_ci(mean(mpg),B=100)
#'
#'
#'
#' @export

slipper_ci_ = function(df,expr,B=100,lower=0.025,upper=0.975){
  obs_val = lazy_eval(expr,data=df)
  n = nrow(df)
  boot_val = replicate(B, {
    newdata = sample_n(df, n, replace = TRUE)
    lazy_eval(expr, data = newdata)
  })
  tmp = data.frame(type = c("observed",rep("bootstrap",B)),
                   value = c(obs_val,boot_val))
  out = tmp %>% filter(type=="bootstrap") %>%
    summarize(ci_low = quantile(value,lower),
              ci_high = quantile(value,upper))
  return(out)
}

#' Bootstrap a function that returns a single number and
#' return a non-parametric confidence interval.
#'
#' Takes a data frame, a function that returns a single number,
#' and a number of replicates and returns a data frame with the
#' function calculated on the observed sample and on B bootstrap
#' samples.
#'
#' @param df A data frame
#' @param expr A an expression with a function that operates on one or more variables from the data frame.
#' @param B the number of bootstrap samples to draw
#' @param lower the lower percentile for the confidence interval (default 2.5%)
#' @param upper the upper percentile for the confidence interval (default 97.5%)
#'
#' @return confint A vector with the lower bound and upper bound 
#'
#'
#' @examples
#'
#' # Boostrap the mean of the mpg variable in the mtcars data set and get a confidence
#' # interval 
#' slipper_ci(mtcars,mean(mpg),B=100)
#'
#' # Bootstrap the mean of the mpg variable with piping
#' mtcars %>% slipper_ci(mean(mpg),B=100)
#'
#'
#'
#' @export

slipper_ci = function(df, expr, B=100, lower=0.025,upper=0.975) {
  slipper_ci_(df, lazy(expr),B)
}
#' Bootstrap a function that returns a single number 
#'
#' Takes a data frame, a function that returns a single number,
#' and a number of replicates and returns a data frame with the
#' function calculated on the observed sample and on B bootstrap
#' samples. 
#'
#' @param df A data frame
#' @param expr A an expression with a function that operates on one or more variables from the data frame.
#' @param B the number of bootstrap samples to draw 
#'
#' @return out A data frame with the values, whether they come from the observed data or the bootstrapped data.
#' 
#'
#' @examples
#' 
#' # Boostrap the mean of the mpg variable in the mtcars data set
#' slipper(mtcars,mean(mpg),B=100)
#' 
#' # Bootstrap the mean of the mpg variable with piping
#' mtcars %>% slipper(mean(mpg),B=100)
#' 
#' # Calculate a confidence interval using the quantiles method
#' mtcars %>% slipper(mean(mpg),B=100) %>%
#'  filter(type="bootstrap") %>% 
#'  summarize(ci_low = quantile(value,0.025),
#'            ci_high = quantile(value,0.975))
#'
#'
#' @export

slipper_ = function(df,expr,B=100){
 obs_val = lazy_eval(expr,data=df)
 n = nrow(df)
 boot_val = replicate(B, {
   newdata = sample_n(df, n, replace = TRUE)
   lazy_eval(expr, data = newdata)
 })
 out = data.frame(type = c("observed",rep("bootstrap",B)),
                  value = c(obs_val,boot_val))
 return(out)
}

#' Bootstrap a function that returns a single number 
#'
#' Takes a data frame, a function that returns a single number,
#' and a number of replicates and returns a data frame with the
#' function calculated on the observed sample and on B bootstrap
#' samples. 
#'
#' @param df A data frame
#' @param expr A bare function that operates on one or more variables from the data frame.
#' @param B the number of bootstrap samples to draw 
#'
#' @return out A data frame with the values, whether they come from the observed data or the bootstrapped data. 
#'
#'
#' @examples
#' 
#' # Boostrap the mean of the mpg variable in the mtcars data set
#' slipper(mtcars,mean(mpg),B=100)
#' 
#' # Bootstrap the mean of the mpg variable with piping
#' mtcars %>% slipper(mean(mpg),B=100)
#' 
#' # Calculate a confidence interval using the quantiles method
#' mtcars %>% slipper(mean(mpg),B=100) %>%
#'  filter(type="bootstrap") %>% 
#'  summarize(ci_low = quantile(value,0.025),
#'            ci_high = quantile(value,0.975))
#'
#'
#' @export

slipper = function(df, expr, B=100) {
  slipper_(df, lazy(expr),B)
}
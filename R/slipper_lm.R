
#' Bootstrap a linear regression model
#'
#' Takes a data frame, and a model to fit to the data
#' and each bootstrap replicate. Bootstrapping is by
#' default resampling cases, but if you set boot_resid=TRUE
#' then resampling residuals will be performed. If you
#' pass a null model formula that includes a subset
#' of the variables in the full model (i.e. it is a 
#' nested model) then the bootstrap statistics will
#' come from the bootstrapped null data and can be
#' used for a hypothesis test. 
#' 
#' 
#' @param df A data frame
#' @param formula A an expression for a formula to pass to the lm command
#' @param null_formula (optional) If NULL, standard bootstrapping is performed. If a nested expression for a null formula is passed the bootstrapped statistics come from the null.
#' @param B the number of bootstrap samples to draw 
#' @param boot_resid If TRUE then bootstrapping residuals is performed.
#'
#' @return out A data frame with the values, whether they come from the observed data or the bootstrapped data, and the coefficient name. 
#'
#' @examples
#'                 
#' 
#' @export


slipper_lm_ = function(df,
                       formula,
                       null_formula=NULL,
                       B=100,
                       boot_resid=FALSE){
  
  lm_fit = lm(lazy_eval(formula),data=df)
  obs_val = tidy(lm_fit) %>%
    select(term,value=estimate) 
  n = nrow(df)
  
  if(!is.null(lazy_eval(null_formula))){
    lm_fit_null = lm(lazy_eval(null_formula),
                     data=df)
    
    outcome = all.vars(lazy_eval(formula)[[2]])
    pos = which(names(df) == outcome)
    res = residuals(lm_fit)
    
    boot_val = replicate(B, {
      newdata = df
      newdata[,pos] = lm_fit_null$fitted.values + 
        sample(res,replace=TRUE)
      tidy(lm(lazy_eval(formula),data=newdata)) %>%
        select(term,value=estimate)
    },simplify=F) %>% bind_rows()
    
  }else if(is.null(lazy_eval(null_formula)) & boot_resid==FALSE){
    boot_val = replicate(B, {
      newdata = sample_n(df, n, replace = TRUE)
      tidy(lm(lazy_eval(formula),data=newdata)) %>%
        select(term,value=estimate)
    },simplify=F) %>% bind_rows()
  }else if(is.null(lazy_eval(null_formula)) & boot_resid==TRUE){
    
    outcome = all.vars(lazy_eval(formula)[[2]])
    pos = which(names(df) == outcome)
    res = residuals(lm_fit)
    
    boot_val = replicate(B, {
      newdata = df
      newdata[,pos] = lm_fit$fitted.values + sample(res,replace=TRUE)
      tidy(lm(lazy_eval(formula),data=newdata)) %>%
        select(term,value=estimate)
    },simplify=F) %>% bind_rows()
  }
  out = rbind(obs_val,boot_val)
  out$type = c(rep("observed",dim(obs_val)[1]),
               rep("bootstrap",dim(boot_val)[1]))
  return(out)
 
}


#' Bootstrap a linear regression model
#'
#' Takes a data frame, and a model to fit to the data
#' and each bootstrap replicate. Bootstrapping is by
#' default resampling cases, but if you set boot_resid=TRUE
#' then resampling residuals will be performed. If you
#' pass a null model formula that includes a subset
#' of the variables in the full model (i.e. it is a 
#' nested model) then the bootstrap statistics will
#' come from the bootstrapped null data and can be
#' used for a hypothesis test. 
#' 
#' 
#' @param df A data frame
#' @param formula A bare formula to pass to the lm command
#' @param null_formula (optional) If NULL, standard bootstrapping is performed. If a bare nested null formula is passed the bootstrapped statistics come from the null.
#' @param B the number of bootstrap samples to draw 
#' @param boot_resid If TRUE then bootstrapping residuals is performed.
#'
#' @return out A data frame with the values, whether they come from the observed data or the bootstrapped data, and the coefficient name. 
#'
#'
#' @export
#' @examples
#' 
#' # Bootstrap a regression model 
#' slipper_lm(mtcars,mpg ~ cyl,B=100)
#' 
#' # Bootstrap a regression model with piping
#' mtcars %>% slipper_lm(mpg ~ cyl,B=100)
#'
#' # Bootstrap residuals for a regression model
#' mtcars %>% slipper_lm(mpg ~ cyl,B=100,boot_resid=TRUE)
#' 
#' # Bootsrap confidence intervals
#' mtcars %>% slipper_lm(mpg ~ cyl,B=100) %>% 
#' filter(type=="bootstrap",term=="cyl") %>%
#'  summarize(ci_low = quantile(value,0.025),
#'            ci_high = quantile(value,0.975))
#'            
#' # Bootstrap hypothesis test - here I've added one to the numerator
#' # and denominator because bootstrap p-values should never be zero.
#' 
#' boot = mtcars %>% slipper_lm(mpg ~ cyl, null_formula = mpg ~ 1,B=1000) %>%
#'                      filter(term=="cyl") %>%
#'                      summarize(num = sum(abs(value) >= abs(value[1])),
#'                                den = n(),
#'                                pval = num/den)




slipper_lm = function(df,
                      formula,
                      null_formula=NULL,
                      B=100,
                      boot_resid=FALSE) {
  slipper_lm_(df, lazy(formula),lazy(null_formula),B,boot_resid)
}



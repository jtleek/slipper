slipper_boot = function(df,formula,B){
  n = nrow(df)
  boot_val = replicate(B, {
      newdata = sample_n(df, n, replace = TRUE)
      tidy(lm(lazy_eval(formula),data=newdata)) %>%
      select(term,estimate)
      },simplify=F) %>% bind_rows()
  return(boot_val)
}
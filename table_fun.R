table_fun <- function(model){
  fixed <- broom::tidy(model) %>% filter(group == "fixed") %>%
    select(term, estimate) 
  ## add random effects ##
  rand <- VarCorr(model)[[1]]
  if(nrow(rand) > 1){
  rand <- rand[1:nrow(rand), 1:nrow(rand)]
  }
  colnames(rand)[colnames(rand) == "(Intercept)"] <- "Intercept"
  rownames(rand)[rownames(rand) == "(Intercept)"] <- "Intercept"
  vars <- rownames(rand)
  rand[upper.tri(rand)] <- NA
  rand <- data.frame(rand) %>% mutate(var1 = rownames(.)) %>%
    gather(key = var2, value = estimate, -var1, na.rm = T) %>%
    mutate(var1 = mapvalues(var1, vars, 0:(length(vars)-1)),
           var2 = mapvalues(var2, unique(var2), 0:(length(vars)-1))) %>%
    filter(var1 == var2) %>%
    unite(var, var1, var2, sep = "") %>%
    mutate(var = sprintf("$\\tau_{%s}$", var))
  ## get confidence intervals ##
  CI <- data.frame(confint.merMod(model, method = "boot", nsim = 100, oldNames = F)) %>%
    mutate(term = rownames(.)) %>% setNames(c("lower", "upper", "term"))
  
  CI %>% filter(term == "sigma") %>%
    mutate(estimate = sigma(model),
           term = "$\\sigma^2$",
           type = "Residuals")
  
  ## Get ICC & R2 values ##
  ICC <- reghelper::ICC(model)
  R2 <- MuMIn::r.squaredGLMM(model)
  
  ## format the fixed effects
  fixed <- fixed %>% left_join(CI %>% filter(!grepl(".sig", term))) %>%
    mutate(type = "Fixed Parts",
           term = str_replace_all(term, "_", "\\\\_"),
           term = str_replace_all(term, "[()]", ""))
  
  rand <- rand %>%
    left_join(
      CI %>% filter(grepl("sd", term)) %>%
        mutate(lower = lower^2, upper = upper^2,
               var = mapvalues(term, unique(term), 0:(length(unique(term))-1)),
               var = sprintf("$\\tau_{%s%s}$", var, var)) %>% select(-term)) %>%
    mutate(type = "Random Parts") %>% rename(term = var)
  
  mod_terms <- tribble(
    ~term, ~estimate, ~type,
    # "ICC", ICC, "Model Terms",
    "$R^2_m$", R2[1], "Model Terms",
    "$R^2_c$", R2[2], "Model Terms"
  )
  
  tab <- fixed %>%
    full_join(rand) %>%
    full_join(mod_terms)
  if ("glmerMod" %in% class(fit1)){
    tab <- tab %>% 
      mutate(OR = sprintf("%.2f", exp(estimate)),
             lower = exp(lower),
             upper = exp(upper))
  }
  tab <- tab %>%
    mutate_at(vars(lower, upper), funs(ifelse(is.na(.) == T, "", sprintf("%.2f", .)))) %>%
    mutate(CI = sprintf("[%s, %s]", lower, upper)) %>%
    select(-lower, -upper) %>%
    mutate(estimate = sprintf("%.2f", estimate)) %>%
    dplyr::rename(b = estimate)
  if ("glmerMod" %in% class(fit1)){
    tab <- tab %>% select(type, term, b, OR, CI)
  } else{tab <- tab %>% select(type, everything())}
  return(tab)
}

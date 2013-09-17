
stripFormula = function(form, dt) {
  tf <- terms.formula(form, data=dt)
  factors <- attr(tf, "factors")
  
  drop <- sapply(dt, function(x) { is.factor(x) && length(unique(x)) == 1 })
  
  if (any(drop)) {
    drop_names <- names(drop)[drop]
    term_vars <- sapply(colnames(factors),
                        function(col) {
                          c <- factors[, col]
                          names(c)[c != 0]
                        })
    tf <- sapply(term_vars, function(x) x %in% drop_names)
    has_drop <- sapply(tf, any)
    has_both <- has_drop & !sapply(tf, all)
    drops <- paste0(" - ", names(tf)[has_drop])
    adds <- if (any(has_both)) {
      paste(" + ",
            mapply(function(vars, tfs) paste(vars[!tfs], collapse=":"),
                   term_vars[has_both],
                   tf[has_both]))
    } else {
      NULL
    }
    edit <- paste(c("~ .", drops, adds), collapse="")
    
    return(update(form, edit))
  } else {
    return(form)
  }
}

decayWeight = function(v, by, every, from) {
  dist = switch(from,
    max = max(v) - v,
    min = v - min(v),
    stop("'decayWeight's 'from' argument must take value 'min' or 'max'.")
  )

  if (!is.numeric(dist))
    dist = as.numeric(dist)

  return((1-by)^(dist/every))
}

typeFit = function(type, formula, data, weight=NULL) {
  func = c(logLm=lm, poisson=glm, negativeBinomial=glm.nb)[[type]]
  
  args = c(list(poisson=c(family="poisson"))[[type]],
           list(formula=formula, data=data, weight=weight))
  
  if (is.null(func))
    stop(sprintf("Model 'type' (%s) not recognized by 'fitType", type))
  
  do.call(func, args)
}

typeFitSummary = function(type, fit) {
  fitted = switch(type,
    logLm = { mse = mean(residuals(fit)^2)
              exp(fitted(fit)*(mse/2)) - 1 },
    poisson =,
    negativeBinomial = fitted(fit),
    stop(sprintf("Model 'type' (%s) not recognized by 'fitExtract", type)))

  sum = summary(fit)
  
  rsq = sum$r.squared
  if (is.null(rsq))
    rsq = NA_real_

  return(list(coef=sum$coefficients, fitted=fitted, rsq=rsq))
  
  ## MAPE
}

typePredict = function(type, fit, newdata) {
  p = predict(fit, newdata=newdata, type="response")
   if (type != "logLm")
     return(p)
   mse = mean(residuals(fit)^2)
   return(exp(p*mse/2))
}

typeExplain = function(type, predict, newdata, huh_temporals) {
  
}

eTypeFit = errate(typeFit)
eTypePredict = errate(typePredict)
eTypeFitSummary = errate(typeFitSummary)

typeModel = function(type, formula, train, test, weight=NULL, actual="units",
                     coefs=list(elastics=c(match="(log_otd|otd)"))) {
  f = stripFormula(formula, train)

  fit = eTypeFit(type, formula=f, data=train, weight=weight)

  status = list(fit = errata(fit))
  
  if (hasError(fit)) {
    predict = list(result=NULL)
    fit_sum = list(result=NULL)
    oneliner = list(warn=status[["fit"]][["warn"]], rsq=NA_real_,
                    in_me=NA_real_, in_rmse = NA_real_,
                    #                          outfit_me = "x",
                    #                          outfit_rmse = "x",
                    outcast_me = NA_real_, outcast_rmse = NA_real_,
                    elastics = NA_character_)
  } else {              
    fit = unErrate(fit)
    
    predict = eTypePredict(type, fit=fit, newdata=test)
    fit_sum = eTypeFitSummary(type, fit=fit)
      
    status[["predict"]] = errata(predict)
    status[["summarize"]] = errata(fit_sum)
    
    predict = unErrate(predict)
    fit = unErrate(fit_sum)
    
    coef_strs = sapply(coefs,
      function(x, m_coefs) {
        match = x["match"]
        
        names = grep(match, rownames(m_coefs), value=T)
        coefs = as.list(round(m_coefs[names, "Estimate"], 2))
        
        sub = x["sub"]

        if (!is.na(sub))
          names = sub(match, sub, names)
        paste(names, coefs, sep="=", collapse=", ")  
      }, fit[["coef"]])
    
    in_errs = fit[["fitted"]] - train[[actual]]
    outcast_errs = predict - test[[actual]]
    
    in_rmse = round(sqrt(mean(in_errs^2)), 1)
    outcast_rmse = round(sqrt(mean(outcast_errs^2)), 1)
    
    in_me = round(mean(in_errs), 2)
    outcast_me = round(mean(outcast_errs), 2)
    
    oneliner = list(warn=!all(vapply(status,
                                     function(x) is.null(x[['warn']]),
                                     logical(1))),
                    rsq=fit[['rsq']], in_me=in_me, in_rmse=in_rmse,
                    #                          outfit_me = "x",
                    #                          outfit_rmse = "x",
                    outcast_me=outcast_me, outcast_rmse=outcast_rmse)

    if (!is.null(coefs))
      oneliner = c(oneliner, coef_strs)
    
  }
    
  r = list(status = status,
           detail = list(fit=fit,
                         predict=predict),
           oneliner = oneliner)
      
  return(r)
}

# caret::findLinearCombos # look where rank deficient

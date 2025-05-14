ggd_RandomImpute <- function(data, specified.columns, onbekend_values = NA){
  for(i in which(colnames(data) %in% specified.columns)){
    cat(" waardes random imputeren voor kolom: ", colnames(data)[i], ": ")
    nas <- data[[i]] %in% onbekend_values
    cat("n =",sum(nas), "\n")
    n_na <- sum(nas)
    sample_source <- unique(data[[i]]) %>% na.omit
    sample_source <- sample_source[!sample_source %in% onbekend_values]
    data[nas, i] <- sample(sample_source, n_na, T)
  }
  return(data)
}
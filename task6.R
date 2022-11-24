library(stringr)
library(dplyr)
library(tidyr)

path = '/Users/vladaefremenko/R_Lab2/data/ExpImp.RData'

get_res_table <- function(data, region){
  data <- data[complete.cases(data),]
  for (i in 2:length(names(data))) {
    data[[i]] <- gsub("-", 0, data[[i]])
    data[[i]] <- as.numeric(data[[i]])
  }
  flt <- str_detect(data$Регион, 'федеральный округ')
  rdf <- mutate(data, Округ = if_else(flt, Регион, NULL))
  rdf <- fill(rdf, Округ)
  flt2 <- !str_detect(rdf$Регион, 'Федерация|федеральный округ')
  rdf <- filter(rdf, flt2)
  
  match_exp <- select_at(rdf, vars(matches("Экспорт")))
  match_imp <- select_at(rdf, vars(matches("Импорт")))
  
  match_exp$Сумма <- rowSums(match_exp, na.rm = TRUE)
  match_imp$Сумма <- rowSums(match_imp, na.rm = TRUE)
  
  rdf$Delta <- match_exp$Сумма - match_imp$Сумма
  
  rdf <- filter(rdf, Округ == region & Delta > 0)
  
  return(rdf)
}

load(path)
result <- get_res_table(ExpImp, 'Северо-Западный федеральный округ')
print(result)

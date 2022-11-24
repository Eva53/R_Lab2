library(readr)
library(stringr)
library(dplyr)

path = '/Users/vladaefremenko/R_Lab2/data/Payment_and_value_of_care-Hospital.csv'

get_res_table <- function(data){
  flt <- !str_detect(data$Payment, 'Not Available')
  data <- filter(data, flt)
  data <- arrange(data, data[,'Value of Care Display Name'], 
                  data[,'Value of Care Category'], data$Payment)
  unique <- distinct(data, data[,'Value of Care Display Name'], 
                     data[,'Value of Care Category'],.keep_all = TRUE)
  res <- data.frame(unique$`Value of Care Display Name`,
                    unique$`Value of Care Category`,
                    unique$Payment, 
                    unique$`Facility Name`, 
                    unique$City, 
                    unique$State, 
                    unique$`County Name`)
  res <- rename_with(res, ~ gsub(".", " ", .x, fixed = TRUE))
  res <- rename_with(res, ~ gsub("unique", "", .x, fixed = TRUE))
  
  return(res)
}

df <- read_csv(path)
result <- get_res_table(df)
print(result)
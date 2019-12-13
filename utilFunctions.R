get_airline_data_list <- function(airline_list ) { # Get the info per airline
  library(dplyr)
  airlines <- fread("data/airlines.csv")
  names(airlines) <- c("Airline", "Name", "Alias", "IATA", "ICAO", "Callsign", "Country", "Active")
  
  return_df = data.frame()
  for (airline in airline_list) { 
    airline_to_use <- airlines %>% filter(Name == as.character(airline))
    return_df <- rbind(return_df, airline_to_use)
  }
  return_df
}

count_per_month <- function(airline) { # Get the total number of flights per month
  count_month <- airline %>% group_by(year = month(airline$Date)) %>% count()
  count_month$year <- paste(year(airline$Date[1]), count_month$year, "01", sep="-")
  return(count_month)
}

arr_per_month <- function(airline) { # Get the average arrival delay per month
  arr_month <- airline %>% group_by(year = month(airline$Date)) %>% summarise(mean = mean(ARR_DELAY, na.rm = TRUE))
  arr_month$year <- paste(year(airline$Date[1]), arr_month$year, "01", sep="-")
  return(arr_month)
}

dep_per_month <- function(airline) { # Get the average departure delay per month
  dep_month <- airline %>% group_by(year = month(airline$Date)) %>% summarise(mean = mean(DEP_DELAY, na.rm = TRUE))
  dep_month$year <- paste(year(airline$Date[1]), dep_month$year, "01", sep="-")
  return(dep_month)
}

make_df <- function(year, airline, variable, value) { # Convert the received information into a proper data frame
  df <- data.frame(year, airline, variable, value)
  names(df) <- c("year", "airline", "variable", "value" = numeric()) 
  return(df)
}

process_excel_file <- function(excel_file) { 
  excel_file$...2 <- NULL # Every second column was empty, so we filter that
  names(excel_file) <- c("Airline", paste("yr", 1995:2018, sep = "")) # We fix the column names
  df_wide <- na.omit(excel_file) # Omit any empty rows
  df_wide <- df_wide %>% filter(Airline == "American" | # Filter on the airlines that we want
                                  Airline == "Delta" | 
                                  Airline == "United" | 
                                  Airline == "Frontier" |
                                  Airline == "Alaska")
  
  df_long <- df_wide %>% pivot_longer(cols = starts_with("yr"), # Conver the wide data frame into a long data frame 
                                      names_to = "year", 
                                      names_prefix = "yr", 
                                      values_to = "value", 
                                      values_drop_na = TRUE)
  df_long$Date <- ymd(paste(df_long$year, "01", "01", sep = "-")) # Fix the date column
  
  return(df_long)
}
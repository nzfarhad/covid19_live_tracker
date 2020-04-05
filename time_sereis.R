library(RCurl)

clean_series_data <- function(time_data, case_death){
  
  vars <- c(state = "Province/State", country ="Country/Region")
  time_data <- dplyr::rename(time_data, !!vars)
  time_data$country<-as.character(time_data$country)
  
  # hormonize country names
  time_data$country[time_data$country=="Mainland China"]<- "People's Republic of China"
  time_data$country[time_data$country=="China"]<- "People's Republic of China"
  time_data$country[time_data$country=="Congo (Brazzaville)"]<-"Republic of the Congo"
  time_data$country[time_data$country=="Equatorial Guinea"]<-"Guinea"
  time_data$country[time_data$country=="Reunion"]<-"France"
  time_data$country[time_data$country=="Martinique"]<-"France"
  time_data$country[time_data$country=="Republic of Ireland"]<- "Ireland"
  time_data$country[time_data$country=="Congo (Kinshasa)"]<-"Democratic Republic of the Congo"
  time_data$country[time_data$country=="Faroe Islands"]<- "Faeroe Is."
  time_data$country[time_data$country=="Vatican"]<- "Vatican City"
  time_data$country[time_data$country=="occupied Palestinian territory"]<-"Palestine"
  time_data$country[time_data$country=="Cote d'Ivoire"]<-"Côte d'Ivoire"
  time_data$country[time_data$country=="Macau"]<- "Macao"
  time_data$country[time_data$country=="Holy See"]<-"Vatican"
  time_data$country[time_data$country=="Cayman Islands"]<-"Cayman Is."
  time_data$country[time_data$country=="Guadeloupe"]<-"France"
  time_data$country[time_data$country=="Curacao"]<-"Curaçao"
  time_data$country[time_data$country=="Guadeloupe"]<-"France"
  time_data$country[time_data$country=="North Macedonia"]<- "Republic of Macedonia"
  time_data$country[time_data$country=="Korea, South"]<- "South Korea"
  time_data$country[time_data$country=="UK"]<- "United Kingdom"
  time_data$country[time_data$country=="Gibraltar"]<- "United Kingdom"
  time_data$country[time_data$country=="US"]<- "United States of America"
  time_data$country[time_data$country=="Saint Barthelemy"]<- "St-Barthélemy"
  time_data$country[time_data$country=="Eswatini"]<-"eSwatini"
  time_data$country[time_data$country=="French Guiana"]<-"France"
  time_data$country[time_data$country=="Gambia"]<-"The Gambia"
  time_data$country[time_data$country=="Timor-Leste"]<-"East Timor"
  time_data$country[time_data$country=="Cabo Verde"]<-"Cape Verde"
  time_data$country[time_data$country=="Côte d'Ivoire"]<-"Ivory Coast"
  time_data$country[time_data$country=="Burma"]<-"Myanmar"
  time_data$country[time_data$country=="Bahamas"]<-"The Bahamas"
  time_data$country[time_data$country=="Czechia"]<-"Czech Republic"
  time_data$country[time_data$country=="Taiwan*"]<-"Taiwan"
  
  
  long <- tidyr::pivot_longer(time_data, names_to = "Date", values_to = case_death, cols = contains("/"))
  long$Date <- as.Date(long$Date, tryFormats = "%m/%d/%y")
  return(long)
}

join_data <- function(cases_data, death_data, recov_data){
  # create unique key
  cases_data$key <- paste(cases_data$country, cases_data$Date, cases_data$Long, cases_data$Lat, cases_data$state)
  death_data$key <- paste(death_data$country, death_data$Date, death_data$Long, death_data$Lat, death_data$state)
  recov_data$key <- paste(recov_data$country, recov_data$Date, recov_data$Long, recov_data$Lat, recov_data$state)
  
  #subset the columns
  death_data <- death_data %>% 
    select(key, deaths)
  
  recov_data <- recov_data %>% 
    select(key, recov)
  
  #join
  all_dfs <- list(cases_data, death_data, recov_data)
  all_dfs_merged <- all_dfs %>% purrr::reduce(dplyr::left_join, by = "key") %>% select(-c(key))
  all_dfs_merged$recov[is.na(all_dfs_merged$recov)] <- 0
  
  
  return(all_dfs_merged)
  
}

csse_data <- function(){
  cases_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  death_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  recov_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  cases_data <- read.csv(text = cases_url, check.names = F)
  death_data <- read.csv(text = death_url, check.names = F)
  recov_data <- read.csv(text = recov_url, check.names = F)
  
  cases_data <- clean_series_data(cases_data, "cases")
  death_data <- clean_series_data(death_data, "deaths")
  recov_data <- clean_series_data(recov_data, "recov")
  
  
  joined_data <- join_data(cases_data, death_data, recov_data)
  
  joined_data$active_cases <- joined_data$cases - joined_data$deaths - joined_data$recov
  
  joined_data <- joined_data %>% 
    group_by(country, Date) %>% 
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      recov = sum(recov),
      active_cases = sum(active_cases)
    ) 
  
  return(joined_data)
  
}





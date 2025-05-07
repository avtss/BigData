library(rvest)
library(dplyr)

years <- 2014:2021
countries <- c("France", "Germany", "Argentina", "Bulgaria", "Hungary")
base_url <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title="

# сбор данных по стране и году
get_quality_of_life <- function(year, country) {
  url <- paste0(base_url, year)
  page <- read_html(url)
  
  # чтение таблицы
  table <- page %>% html_node("table#t2") %>% html_table()
  
  # нумерация мест
  table <- table %>%
    mutate(Rank = row_number())
  
  # добавляем год
  table <- table %>% mutate(Year = year)
  
  # фильтруем нужную страну
  result <- table %>% filter(Country == country)
  
  return(result)
}

# обработка NA на всякий случай
safe_convert <- function(x) {
  # удаление запятых и в число преобразуем
  suppressWarnings({
    as.numeric(gsub(",", "", x))
  })
}

#сбор данных по всем годам одной страны
get_country_data <- function(country, years = 2014:2021) {
  all_data <- lapply(years, function(x) get_quality_of_life(x, country))
  
  all_data <- lapply(all_data, function(df) {
    df %>% mutate(across(-c(Country, Year), ~ safe_convert(.x)))
  })
  
  all_data_df <- bind_rows(all_data)
  return(all_data_df)
}

# данные для всех стран
data_france <- get_country_data("France")
data_germany <- get_country_data("Germany")
data_argentina <- get_country_data("Argentina")
data_bulgaria <- get_country_data("Bulgaria")
data_hungary <- get_country_data("Hungary")

# обьединяем
all_data_df <- bind_rows(
  data_france,
  data_germany,
  data_argentina, 
  data_bulgaria,
  data_hungary
)

# названия столбцов для анализа
stats <- c(
  "Rank",
  "Quality of Life Index",
  "Purchasing Power Index",
  "Safety Index",
  "Health Care Index",
  "Cost of Living Index",
  "Property Price to Income Ratio",
  "Traffic Commute Time Index",
  "Pollution Index",
  "Climate Index"
)

# цвета
colors <- c("France" = "blue", 
            "Germany" = "black", 
            "Argentina" = "green", 
            "Bulgaria" = "orange", 
            "Hungary" = "purple")

# графики по каждому показателю
for(stat in stats) {
  if(stat %in% colnames(all_data_df)) {
    # переворачиваем ось Y там где чем ниже тем лучше
    ylim <- range(all_data_df[[stat]], na.rm = TRUE)
    if(stat %in% c("Rank", "Pollution Index", "Property Price to Income Ratio", 
                   "Cost of Living Index", "Traffic Commute Time Index")) {
      ylim <- rev(ylim)
    }
    
    plot(NA, xlim = range(years), ylim = ylim,
         main = paste("Сравнение по показателю:", stat),
         xlab = "Год", ylab = stat)
    
    for(country in countries) {
      country_data <- all_data_df %>% filter(Country == country)
      lines(country_data$Year, country_data[[stat]], 
            col = colors[country], type = "o", pch = 16)
    }
    
    # Добавляем легенду
    legend("topright", legend = countries, 
           col = colors[countries], lty = 1, pch = 16)
  }
}

# выводим таблицу
View(all_data_df)
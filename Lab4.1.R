library(rvest)
library(dplyr)

years <- 2014:2021
countries <- c("France", "Cuba", "Argentina", "Bulgaria", "Hungary")
base_url <- "https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title="

# Функция для сбора данных по году и стране
get_quality_of_life <- function(year, country) {
  url <- paste0(base_url, year)
  page <- read_html(url)
  
  # Чтение таблицы целиком
  table <- page %>% html_node("table#t2") %>% html_table()
  
  # Ручная нумерация Rank
  table <- table %>%
    mutate(Rank = row_number())
  
  # Добавляем год
  table <- table %>% mutate(Year = year)
  
  # Фильтруем нужную страну
  result <- table %>% filter(Country == country)
  
  return(result)
}

# Улучшенная функция преобразования с обработкой NA
safe_convert <- function(x) {
  # Сначала удаляем запятые, затем преобразуем в числовой формат
  suppressWarnings({
    as.numeric(gsub(",", "", x))
  })
}

# Функция для сбора данных по всем годам для одной страны
get_country_data <- function(country, years = 2014:2021) {
  all_data <- lapply(years, function(x) get_quality_of_life(x, country))
  
  all_data <- lapply(all_data, function(df) {
    df %>% mutate(across(-c(Country, Year), ~ safe_convert(.x)))
  })
  
  all_data_df <- bind_rows(all_data)
  return(all_data_df)
}

# Собираем данные для всех стран
data_france <- get_country_data("France")
data_cuba <- get_country_data("Cuba")
data_argentina <- get_country_data("Argentina")
data_bulgaria <- get_country_data("Bulgaria")
data_hungary <- get_country_data("Hungary")

# Объединяем все данные
all_data_df <- bind_rows(
  data_france,
  data_cuba,
  data_argentina, 
  data_bulgaria,
  data_hungary
)

# Показатели для анализа
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

# Цвета для каждой страны
colors <- c("France" = "blue", 
            "Cuba" = "red", 
            "Argentina" = "green", 
            "Bulgaria" = "orange", 
            "Hungary" = "purple")

# Строим графики для каждого показателя
for(stat in stats) {
  if(stat %in% colnames(all_data_df)) {
    # Определяем нужно ли переворачивать ось Y
    ylim <- range(all_data_df[[stat]], na.rm = TRUE)
    if(stat %in% c("Rank", "Pollution Index", "Property Price to Income Ratio", 
                   "Cost of Living Index", "Traffic Commute Time Index")) {
      ylim <- rev(ylim)
    }
    
    # Создаем пустой график
    plot(NA, xlim = range(years), ylim = ylim,
         main = paste("Сравнение по показателю:", stat),
         xlab = "Год", ylab = stat)
    
    # Добавляем линии для каждой страны
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

# Выводим таблицу с данными
View(all_data_df)
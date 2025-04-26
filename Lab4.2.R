library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# Функция для получения адреса со страницы музея
get_museum_address <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Пытаемся найти адрес в infobox (обычно в таблице с классом "infobox")
    infobox <- page %>% html_nodes(".infobox")
    
    if (length(infobox) > 0) {
      # Ищем строку с "Адрес" или "Местоположение"
      rows <- infobox %>% html_nodes("tr")
      
      for (row in rows) {
        th <- row %>% html_node("th") %>% html_text(trim = TRUE)
        if (!is.na(th) && (str_detect(th, "Адрес|адрес|Местоположение|местоположение"))) {
          td <- row %>% html_node("td") %>% html_text(trim = TRUE)
          return(td)
        }
      }
    }
    
    # Если в infobox не нашли, попробуем найти в тексте статьи
    headings <- page %>% html_nodes("h2, h3, h4") %>% html_text(trim = TRUE)
    address_heading <- which(str_detect(headings, "Адрес|адрес|Местоположение|местоположение"))
    
    if (length(address_heading) > 0) {
      address_node <- page %>% html_nodes(paste0("h2:contains('", headings[address_heading[1]], "') + p"))
      if (length(address_node) > 0) {
        return(address_node %>% html_text(trim = TRUE))
      }
    }
    
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })
}

# Загружаем страницу
url <- "https://ru.wikipedia.org/wiki/Список_музеев_Москвы"
page <- read_html(url)

# Извлекаем таблицу
tables <- page %>% html_nodes("table")
museum_table <- html_table(tables[[1]], fill = TRUE)

# Извлекаем названия и ссылки
museum_nodes <- tables[[1]] %>% html_nodes("tr td:nth-child(2) a")
museum_names <- museum_nodes %>% html_text()
museum_links <- museum_nodes %>% html_attr("href")
museum_links <- paste0("https://ru.wikipedia.org", museum_links)

# Создаём таблицу с названиями и ссылками
link_table <- data.frame(
  Name = museum_names,
  Link = museum_links,
  stringsAsFactors = FALSE
)

# Приводим названия в основной таблице к виду, совпадающему с link_table
museum_table <- museum_table %>%
  rename(Name = 2, Address = 3) %>%
  mutate(Name = str_trim(Name))

# Объединяем по названию
museum_df <- left_join(museum_table, link_table, by = "Name") %>%
  select(Name, Address, Link)

# Заменяем адреса на те, что найдены на страницах музеев
museum_df$Address <- map_chr(museum_df$Link, get_museum_address)

# Удаляем столбец с ссылками, если он не нужен
museum_df <- museum_df %>% select(Name, Address)

# Смотрим результат
View(museum_df)
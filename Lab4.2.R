library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# получить адреса со страницы
get_museum_address <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # поиск адреса в infobox
    infobox <- page %>% html_nodes(".infobox")
    
    if (length(infobox) > 0) {
      # ищем "Адрес" или "Местоположение"
      rows <- infobox %>% html_nodes("tr")
      
      for (row in rows) {
        th <- row %>% html_node("th") %>% html_text(trim = TRUE)
        if (!is.na(th) && (str_detect(th, "Адрес|адрес|Местоположение|местоположение"))) {
          td <- row %>% html_node("td") %>% html_text(trim = TRUE)
          return(td)
        }
      }
    }
    
    return(NA_character_)
  }, error = function(e) {
    return(NA_character_)
  })
}

# страница вики
url <- "https://ru.wikipedia.org/wiki/Список_музеев_Москвы"
page <- read_html(url)

# сохраняем
tables <- page %>% html_nodes("table")
museum_table <- html_table(tables[[1]], fill = TRUE)

# вытаскиваем названия и ссылки
museum_nodes <- tables[[1]] %>% html_nodes("tr td:nth-child(2) a")
museum_names <- museum_nodes %>% html_text()
museum_links <- museum_nodes %>% html_attr("href")
museum_links <- paste0("https://ru.wikipedia.org", museum_links)

#таблица названий и ссылок
link_table <- data.frame(
  Name = museum_names,
  Link = museum_links,
  stringsAsFactors = FALSE
)

# приводим название в основной к виду совпадающему с таблицей ссылок что сделать джойн потом
museum_table <- museum_table %>%
  rename(Name = 2, Address = 3) %>%
  mutate(Name = str_trim(Name))

#обьединяем таблицы с адресами и ссылками
museum_df <- left_join(museum_table, link_table, by = "Name") %>%
  select(Name, Address, Link)

# вставляем адреса найденные на страницах музеев
museum_df$Address <- map_chr(museum_df$Link, get_museum_address)

#результат
View(museum_df)
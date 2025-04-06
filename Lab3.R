df <- read.csv("summer.csv", stringsAsFactors = FALSE)

# Фильтрация по велоспорту и России
cycling_rus <- subset(df, Sport == "Cycling" & Country == "RUS")
cycling_rus_medals <- subset(cycling_rus, Medal %in% c("Gold", "Silver", "Bronze"))

# Подсчёт по годам и медалям
years <- sort(unique(cycling_rus_medals$Year))
gold <- sapply(years, function(y) sum(cycling_rus_medals$Year == y & cycling_rus_medals$Medal == "Gold"))
silver <- sapply(years, function(y) sum(cycling_rus_medals$Year == y & cycling_rus_medals$Medal == "Silver"))
bronze <- sapply(years, function(y) sum(cycling_rus_medals$Year == y & cycling_rus_medals$Medal == "Bronze"))

medal_matrix <- rbind(gold, silver, bronze)

# График
barplot(medal_matrix, beside = FALSE, col = c("gold", "gray", "sienna3"),
        main = "Медали России по велоспорту",
        xlab = "Год", ylab = "Количество медалей", names.arg = years,
        cex.names = 0.7)
legend("topright", legend = c("Золото", "Серебро", "Бронза"), fill = c("gold", "gray", "sienna3"))

gold_counts <- table(cycling_rus_medals$Year[cycling_rus_medals$Medal == "Gold"])
pie(gold_counts, main = "Золотые медали России по велоспорту", col = rainbow(length(gold_counts)))

recent <- subset(cycling_rus_medals, Year >= 1994)
years <- sort(unique(recent$Year))

men <- sapply(years, function(y) sum(recent$Year == y & recent$Gender == "Men"))
women <- sapply(years, function(y) sum(recent$Year == y & recent$Gender == "Women"))

plot(years, men, type = "b", col = "blue", pch = 16, lty = 1, ylim = c(0, max(c(men, women))),
     main = "Медали по полу (последние 30 лет)", xlab = "Год", ylab = "Количество медалей")
lines(years, women, type = "b", col = "red", pch = 17, lty = 2)
legend("topleft", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), pch = c(16, 17), lty = c(1, 2))


top_countries <- c("RUS", "USA", "FRA", "GER", "GBR", "ITA", "NED")

# Последние 6 лет
years_all <- sort(unique(df$Year))
last6 <- tail(years_all, 6)

# Подсчёт
plot(NA, xlim = range(last6), ylim = c(0, 15),
     main = "Золото и бронза по велоспорту — 7 стран", xlab = "Год", ylab = "Медали")

colors <- rainbow(7)
pch_vec <- c(1, 2, 3, 4, 5, 6, 7)
i <- 1

for (country in top_countries) {
  for (medal_type in c("Gold", "Bronze")) {
    counts <- sapply(last6, function(y) {
      sum(df$Sport == "Cycling" & df$Country == country &
            df$Year == y & df$Medal == medal_type)
    })
    lines(last6, counts, type = "b", col = colors[i], pch = pch_vec[i], lty = ifelse(medal_type == "Gold", 1, 2))
  }
  i <- i + 1
}
legend("topright", legend = top_countries, col = colors, pch = pch_vec, lty = 1)


cycling_last6 <- subset(df, Sport == "Cycling" & Year %in% last6 & Medal %in% c("Gold", "Silver", "Bronze"))

# Подсчёты
years <- sort(unique(cycling_last6$Year))
men <- sapply(years, function(y) sum(cycling_last6$Year == y & cycling_last6$Gender == "Men"))
women <- sapply(years, function(y) sum(cycling_last6$Year == y & cycling_last6$Gender == "Women"))

# Устанавливаем 2x2 макет
par(mfrow = c(2, 2))

# 🔹 4.1. Функциональный график
plot(years, men, type = "b", col = "blue", pch = 16, lty = 1, ylim = c(0, max(men, women)),
     main = "Линейный график по полу", xlab = "Год", ylab = "Медали")
lines(years, women, type = "b", col = "red", pch = 17, lty = 2)
legend("topleft", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), pch = c(16, 17), lty = c(1, 2))

# 🔹 4.2. Столбчатый график
barplot(rbind(men, women), beside = TRUE, col = c("blue", "red"),
        names.arg = years, main = "Столбчатый график по полу",
        xlab = "Год", ylab = "Медали")
legend("topright", legend = c("Мужчины", "Женщины"), fill = c("blue", "red"))

# 🔹 4.3. Круговая диаграмма по 2020
pie_counts <- c(
  sum(cycling_last6$Year == 2020 & cycling_last6$Gender == "Men"),
  sum(cycling_last6$Year == 2020 & cycling_last6$Gender == "Women")
)
names(pie_counts) <- c("Мужчины", "Женщины")
pie(pie_counts, col = c("blue", "red"), main = "Медали по полу в 2020")


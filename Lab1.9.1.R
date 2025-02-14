# Создаем вектор country
country <- rep(c("France", "Italy", "Spain"), each = 5)

year <- rep(2000:2004, times = 3)

df <- data.frame(country, year)

print(df)

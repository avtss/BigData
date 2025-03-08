# Вывод таблички

data <- read.csv("result.csv", header = TRUE, sep = ",")

str(data)

df <- data[,-1]  
print(df)

#вывод без строк с na:
df2 <-df[rowSums(is.na(df)) == 0,]
print(df2)

#замена na на среднее
library(dplyr)
df3 <- df %>% na.omit
print(df3)

# max, min, mean
stats <- data.frame(
  Max = sapply(df[,-1], max, na.rm = TRUE),
  Min = sapply(df[,-1], min, na.rm = TRUE),
  Mean = sapply(df[,-1], mean, na.rm = TRUE)
)

print(stats)

# оценки выше 7 и меньше 3
high <- colSums(df[,-1] > 7, na.rm = TRUE)  
low <- colSums(df[,-1] < 3, na.rm = TRUE)  

preference <- data.frame(Выше_7 = high, Ниже_3 = low)

print(preference)

# сортировка по убыванию
rating <- sort(colMeans(df[,-1], na.rm = TRUE), decreasing = TRUE)

print(rating)

# Столбчатая диаграмма оценок
categories <- names(rating) 
scores <- as.numeric(rating)  

barplot(scores, names.arg = categories, las = 2, col = "lightblue",
        main = "Рейтинг предпочтений", xlab = "", ylab = "Средний балл", cex.names = 0.5)



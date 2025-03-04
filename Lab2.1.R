data <- read.csv("result.csv", sep = ",")

str(data)

df <- data[,-1]  


#max, min, mean
stats <- data.frame(
  Max = sapply(df[,-1], max),
  Min = sapply(df[,-1], min),
  Mean = sapply(df[,-1], mean)
)

print(stats)

#оценки выше 7 и меньше 3
high <- colSums(df[,-1] > 7)  
low <- colSums(df[,-1] < 3)  

preference <- data.frame(Выше_7 = high, Ниже_3 = low)

print(preference)

#сортировка по убыванию
rating <- sort(colMeans(df[,-1]), decreasing = TRUE)

print(rating)


#столбчатая диаграмма оценок
categories <- names(rating) 
scores <- as.numeric(rating)  

barplot(scores, names.arg = categories, las = 2, col = "lightblue",main = "Рейтинг предпочтений", xlab = "", ylab = "Средний балл", cex.names = 0.5)





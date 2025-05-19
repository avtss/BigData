library(factoextra)    # Для визуализации кластеров
library(NbClust)       # Для определения числа кластеров
library(cluster)       # Алгоритмы кластеризации
library(scatterplot3d) # 3D визуализация
library(e1071)         # Наивный Байес
library(party)         # Деревья решений
library(randomForest)  # Случайный лес
library(dplyr)  

# 1. Загрузка данных 
data <- read.csv("BRFSS_Physical_Acticity.csv", header = TRUE, sep = ",")
location_key <- read.csv("BRFSS_location_id_KEY.csv", header = TRUE, sep = ",")
columns_key <- read.csv("BRFSS_Physical_Activity_columns_KEY.csv", header = TRUE, sep = ",")

# Объединяем с ключом локаций
data <- merge(data, location_key, by.x = "location_id", by.y = "id")

# 2. Выбираем только числовые переменные с "_value" и удаляем строки с NA
numeric_data <- data[, grep("_value", names(data), value = TRUE)]
numeric_data <- na.omit(numeric_data)  # Удаляем строки с пропущенными значениями

# Проверяем на Inf и NaN
numeric_data <- numeric_data[!is.infinite(rowSums(numeric_data)) & !is.nan(rowSums(numeric_data)), ]

# 3. Boxplot
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) 
boxplot(numeric_data,
        names = c("PAINDX2_yes", "PAINDX2_no", "PASTAE2_yes", "PASTAE2_no",
                  "PASTRNG_yes", "PASTRNG_no", "TOTINDA_yes", "TOTINDA_no"),
        main = "Boxplot по показателям физической активности",
        col = "lightgray",
        las = 2,
        cex.axis = 0.8)
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1)) # Настраиваем отступы
for (col in names(numeric_data)) {
  hist(numeric_data[[col]], 
       main = paste("Распределение", col),
       xlab = "",
       col = "lightblue",
       freq = FALSE,
       breaks = 20)
}


par(mfrow = c(1, 1))
# 4. Нормализация
normalized <- scale(numeric_data)

normalized <- scale(numeric_data, center = TRUE, scale = TRUE)
boxplot(normalized, 
        main = "Boxplot (нормализованные данные)", 
        col = "lightgray",
        names = c("PAINDX2_yes", "PAINDX2_no", "PASTAE2_yes", "PASTAE2_no",
                  "PASTRNG_yes", "PASTRNG_no", "TOTINDA_yes", "TOTINDA_no"),
        las = 2)

# 5. Дескриптивный анализ
desc <- data.frame(
  Mean = sapply(numeric_data, mean, na.rm = TRUE),
  Median = sapply(numeric_data, median, na.rm = TRUE),
  Min = sapply(numeric_data, min, na.rm = TRUE),
  Max = sapply(numeric_data, max, na.rm = TRUE),
  SD = sapply(numeric_data, sd, na.rm = TRUE),
  Q1 = sapply(numeric_data, quantile, probs = 0.25, na.rm = TRUE),
  Q3 = sapply(numeric_data, quantile, probs = 0.75, na.rm = TRUE)
)
round(desc, 2)

# 6. Определение числа кластеров
fviz_nbclust(normalized, kmeans, method = "wss") +
  labs(subtitle = "Метод локтя")

fviz_nbclust(normalized, kmeans, method = "silhouette") +
  labs(subtitle = "Метод силуэта")

gap_stat <- cluster::clusGap(normalized, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

nb <- NbClust(
  data = normalized,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans",
  index = c("silhouette") 
)
fviz_nbclust(nb)

# 7. Иерархическая кластеризация
dist_mat <- dist(normalized)
hc <- hclust(dist_mat, method="ward.D2")
plot(hc, main="Дендрограмма", labels=FALSE)
rect.hclust(hc, k=3, border="red")
groups_h <- cutree(hc, k=3)

# 8. Анализ кластеров
means_h <- aggregate(numeric_data, by = list(cluster = groups_h), FUN = mean)

m2 <- t(as.matrix(means_h[, -1]))
colnames(m2) <- c("g1","g2","g3")

barplot(
  m2,
  beside = TRUE,
  names.arg = colnames(m2),
  col = c("tomato","skyblue","seagreen","gold","violet","pink","lightblue","lightgreen"),
  legend.text = rownames(m2),
  args.legend = list(x = "top", bty = "n", inset = c(0, -0.15)),
  xlab = "Кластер",
  ylab = "Среднее значение",
  main = "Средние значения по кластерам"
)

# 9. K-means кластеризация
set.seed(123)
km.res <- kmeans(normalized, centers = 3, nstart = 25)

fviz_cluster(km.res,
             data = normalized,
             geom = "point",
             ellipse.type = "norm",
             main = "k_means k=3")

# 10. Scatterplot
cols <- c("tomato","skyblue","seagreen")[km.res$cluster]
pairs(numeric_data,
      pch = 19,
      col = cols,
      main = "Scatterplot матрица")

# 11. 3D визуализация
cols3d <- c("tomato","skyblue","seagreen")[km.res$cluster]

s3d <- scatterplot3d(
  x = numeric_data[,1],
  y = numeric_data[,2],
  z = numeric_data[,3],
  color = cols3d,
  pch = 16,
  xlab = colnames(numeric_data)[1],
  ylab = colnames(numeric_data)[2],
  zlab = colnames(numeric_data)[3],
  main = "3D кластеризация"
)

legend("topright",
       legend = c("Cluster 1","Cluster 2","Cluster 3"),
       col = c("tomato","skyblue","seagreen"),
       pch = 16,
       inset = 0.05)

# 12. Классификация
# Создаем новый датафрейм с кластерами
cluster_data <- cbind(numeric_data, cluster = as.factor(km.res$cluster))

set.seed(1234)
ind <- sample(2, nrow(cluster_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- cluster_data[ind == 1, ]
testData <- cluster_data[ind == 2, ]

# Наивный Байес
model_nb <- naiveBayes(cluster ~ ., data = trainData)
pred_nb <- predict(model_nb, testData)
mean(pred_nb == testData$cluster)

# Дерево решений
model_tree <- ctree(cluster ~ ., data = trainData)
plot(model_tree)
pred_tree <- predict(model_tree, testData)
mean(pred_tree == testData$cluster)

# Случайный лес
model_rf <- randomForest(cluster ~ ., 
                         data = trainData,
                         ntree = 100)
pred_rf <- predict(model_rf, testData)
mean(pred_rf == testData$cluster)
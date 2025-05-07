# 1. Загрузка данных 
data <- read.csv("/Users/aveti/BigData/BRFSS_Physical_Activity.csv")
location_key <- read.csv("C:/Users/aveti/BigData/BRFSS_location_id_KEY.csv")
columns_key <- read.csv("C:/Users/aveti/BigData/BRFSS_Physical_Activity_columns_KEY.csv")

# Объединяем с ключом локаций
data <- merge(data, location_key, by.x = "location_id", by.y = "id")

# 2. Выбираем только числовые переменные с "_value" (аналог вашего numeric_data)
numeric_data <- data[, grep("_value", names(data), value = TRUE)]

# 3. Boxplot (точный аналог вашего boxplot)
boxplot(numeric_data,
        names = c("PAINDX2_yes", "PAINDX2_no", "PASTAE2_yes", "PASTAE2_no",
                  "PASTRNG_yes", "PASTRNG_no", "TOTINDA_yes", "TOTINDA_no"),
        main = "Boxplot по показателям физической активности",
        col = "lightgray",
        las = 2)

# 4. Нормализация (точная копия вашего кода)
normalized <- scale(numeric_data)

boxplot(normalized, 
        main = "Boxplot (нормализованные данные)", 
        col = "lightgray",
        names = c("PAINDX2_yes", "PAINDX2_no", "PASTAE2_yes", "PASTAE2_no",
                  "PASTRNG_yes", "PASTRNG_no", "TOTINDA_yes", "TOTINDA_no"),
        las = 2)

# 5. Дескриптивный анализ (точная копия)
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

# 6. Определение числа кластеров (точные копии ваших графиков)
fviz_nbclust(normalized, kmeans, method = "wss") +
  labs(subtitle = "Метод локтя")

fviz_nbclust(normalized, kmeans, method = "silhouette") +
  labs(subtitle = "Метод силуэта")

gap_stat <- cluster::clusGap(normalized, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

nb <- NbClust(data = normalized, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
fviz_nbclust(nb) + labs(subtitle = "Консенсус-анализ")

# 7. Иерархическая кластеризация (точная копия)
dist_mat <- dist(normalized)
hc <- hclust(dist_mat, method="ward.D2")
plot(hc, main="Дендрограмма")
rect.hclust(hc, k=3, border="red")
groups_h <- cutree(hc, k=3)
data$cluster_h <- groups_h

plot(hc,
     horiz = TRUE,
     labels = FALSE,
     xlim = c(0, 40))
rect.hclust(hc, k = 3, border = "red", horiz = TRUE)

# 8. Анализ кластеров (точная копия)
means_h <- aggregate(numeric_data, by = list(cluster = data$cluster_h), FUN = mean)

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

par(mfrow = c(2,4), mar = c(4,4,2,1))
for(col in colnames(numeric_data)) {
  boxplot(numeric_data[[col]] ~ cluster_h, data = data, main = col)
}
par(mfrow = c(1,1))

# 9. K-means кластеризация (точная копия)
set.seed(123)
km.res <- kmeans(normalized, centers = 3, nstart = 25)
data$cluster_km <- km.res$cluster

fviz_cluster(km.res,
             data = normalized,
             geom = "point",
             ellipse.type = "norm",
             main = "k_means k=3")

# 10. Scatterplot (точная копия)
cols <- c("tomato","skyblue","seagreen")[data$cluster_h]
pairs(numeric_data,
      pch = 19,
      col = cols,
      main = "Scatterplot матрица")

# 11. 3D визуализация (точная копия)
library(scatterplot3d)

cols3d <- c("tomato","skyblue","seagreen")[data$cluster_h]

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

# 12. Классификация (точная копия)
data$cluster_km <- as.factor(data$cluster_km)

set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- data[ind == 1, ]
testData <- data[ind == 2, ]

# Наивный Байес
model_nb <- naiveBayes(cluster_km ~ ., data = trainData[, c(colnames(numeric_data), "cluster_km")])
pred_nb <- predict(model_nb, testData)
mean(pred_nb == testData$cluster_km)

# Дерево решений
model_tree <- ctree(cluster_km ~ ., data = trainData[, c(colnames(numeric_data), "cluster_km")])
plot(model_tree)
pred_tree <- predict(model_tree, testData)
mean(pred_tree == testData$cluster_km)

# Случайный лес
model_rf <- randomForest(cluster_km ~ ., 
                         data = trainData[, c(colnames(numeric_data), "cluster_km")],
                         ntree = 100)
pred_rf <- predict(model_rf, testData)
mean(pred_rf == testData$cluster_km)
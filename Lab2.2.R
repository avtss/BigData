# 1. Импорт данных из CSV
data <- read.csv("result.csv", header = TRUE, sep = ",")

df <- data[,-c(1,2)]
df_numeric <- apply(df, 2, as.numeric)
df_numeric <- as.data.frame(df_numeric)

# 2. Дескриптивный анализ
summary_stats <- summary(df_numeric)
print("Дескриптивная статистика:")
print(summary_stats)

# 3. Сортировка данных по "Во все тяжкие"
sorted_df <- df_numeric[order(-df_numeric$`Во.все.тяжкие`), ]
print("Сортировка по 'Во все тяжкие':")
print(head(sorted_df))

# 4. Формирование подмножеств по "Острые козырьки" > 7
selected_subset <- subset(df_numeric, `Острые.козырьки` > 7)
print("Студенты с 'Острые козырьки' > 7:")
print(head(selected_subset))
print(dim(selected_subset))


hist(df_numeric$`Острые.козырьки`)

par(mar = c(12, 6, 4, 2)) 

boxplot(df_numeric, las = 2, cex.axis = 0.8, main = "Боксплот оценок по всем сериалам", ylab = "Оценка", col = "lightblue")

df1 <- data.frame(ID = c(1, 2, 3), Name = c("John", "Eve", "Nick"))
df2 <- data.frame(ID = c(2, 3, 4), Age = c(25, 30, 22))

# Слияние по ID (INNER JOIN)
merged_df <- merge(df1, df2, by = "ID")
print(merged_df)

new_row <- data.frame(ID =4, Name = "Julius")
df <- rbind(df1, new_row)
print(df)

subset_df <- subset(df2, Age > 23)
print(subset_df)

df <- subset(merged_df, select = -Name)
print(df)

write.csv(subset_df, "processed_result.csv", row.names = TRUE)
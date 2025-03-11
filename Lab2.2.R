# Устанавливаем seed для воспроизводимости
set.seed(123)



library(readxl)
library(ggplot2)
library(dplyr)

# 1. Импорт данных из CSV
data <- read.csv("result.csv", header = TRUE, sep = ",")

# Удаляем первые два столбца (время, фамилия) и преобразуем в числовой формат
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

# Визуализация: гистограмма и боксплот для "Острые козырьки"
ggplot(df_numeric, aes(x = as.factor(`Острые.козырьки`))) +
  geom_bar(fill = "blue", color = "black", position = "dodge") +
  labs(title = "Рис.1. Гистограмма всех оценок 'Острые козырьки'", 
       x = "Оценка", y = "Частота")

ggplot(df_numeric, aes(y = `Острые козырьки`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_y_continuous(breaks = seq(1, 10, by = 1), labels = seq(1, 10, by = 1)) +  
  labs(title = "Рис.2. Боксплот всех оценок 'Острые козырьки'", y = "Оценка")

# 5. Операции с таблицами
# Слияние таблиц (пример с самим собой)
merged_data <- merge(df_numeric, df_numeric, by = "row.names", all = TRUE)
print("Слияние таблиц:")
print(head(merged_data))

# Добавление строк (добавляем случайного студента)
new_row <- data.frame(t(rep(5, ncol(df_numeric))))
names(new_row) <- names(df_numeric)
df_extended <- rbind(df_numeric, new_row)
print("Добавлена строка:")
print(tail(df_extended))

# Исключение переменной (убираем "Во все тяжкие")
df_no_breaking_bad <- subset(df_numeric, select = -`Во.все.тяжкие`)
print("Таблица без 'Во все тяжкие':")
print(head(df_no_breaking_bad))

# Подмножество данных (например, только "Очень странные дела" и "Игра в кальмара")
subset_data <- df_numeric[, c("Очень.странные.дела", "Игра.в.кальмара")]
print("Выбранные сериалы ('Очень странные дела' и 'Игра в кальмара'):")
print(head(subset_data))

# Сохранение обработанных данных
write.csv(df_extended, "processed_result.csv", row.names = TRUE)

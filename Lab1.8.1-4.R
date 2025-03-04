#Задание 1
# Создание векторов
p <- c(7, 6, 5, 4)
q <- c(0, 1, 2, 3)

sum_result <- p + q        
sub_result <- p - q      
mul_result <- p * q       
div_result <- p / q        
pow_result <- p ^ q        

# Вывод результатов
print("Сложение p + q:")
print(sum_result)

print("Вычитание p - q:")
print(sub_result)

print("Умножение p * q:")
print(mul_result)

print("Деление p / q:")
print(div_result)

print("Возведение в степень p ^ q:")
print(pow_result)


#Задание 2
# Вектор чередующихся чисел
v1 <- rep(0:10 * 2, each = 2)
print(v1)


# Вектор из первых 20 степеней двойки
v2 <- 2^(0:19)
v2


# Вектор из чисел 1, 10, 100, 1000, 10000
v3 <- 10^(0:4)
v3

#Задание 3

seq1 <- 1 / (2:51)
sum1 <- sum(seq1)


seq2 <- 1 / (2^(0:20))
sum2 <- sum(seq2)  

# Третья последовательность
numerator <- seq(1, by = 3, length.out = 15)
denominator <- 3^(0:14)
seq3 <- numerator / denominator
sum3 <- sum(seq3) 

# Числа больше 0.5 в третьей последовательности
count_greater_than_half <- length(seq3[seq3 > 0.5])

# Вывод результатов
sum1
sum2
sum3
count_greater_than_half

#Задание 4
# Создание вектора
vec3 <- seq(3, 27, by = 3)

# Извлечение 2-го, 5-го и 7-го значений
extracted_values <- vec3[c(2, 5, 7)]

# Предпоследнее значение
last_value <- vec3[length(vec3) - 1]

# Все значения, кроме предпоследнего
all_but_last <- vec3[-length(vec3)]

# Все элементы, кроме шестого
all_but_sixth <- vec3[-6]

# Попытка извлечь сотое значение
hundredth_value <- vec3[100]

# Все значения, кроме первого и последнего
all_but_first_last <- vec3[-c(1, length(vec3))]

# Значения больше 4, но меньше 10
values_between_4_and_10 <- vec3[vec3 > 4 & vec3 < 10]

# Значения меньше 4 или больше 10
values_less_than_4_or_greater_than_10 <- vec3[vec3 < 4 | vec3 > 10]

# Вывод результатов
print(extracted_values)
print(last_value)
print(all_but_last)
print(all_but_sixth)
print(hundredth_value)
print(all_but_first_last)
print(values_between_4_and_10)
print(values_less_than_4_or_greater_than_10)


vec11 <- c("A", "B", "C", "D", "F")
vec22 <- c(1, 2, 3, 4, 5)
df <- data.frame(vec11, vec22)
print(df)
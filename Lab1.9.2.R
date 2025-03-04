weight <- c(1500, 1800, 1400, 1600, 1750)  
engine_capacity <- c(2.0, 1.6, 2.0, 1.8, 1.6)  
transmission <- c("Auto", "Manual", "Manual", "Auto", "Manual")  
max_speed <- c(220, 200, 230, 210, 190)  
body_type <- c("Sedan", "Hatchback", "SUV", "Coupe", "Sedan")

car_data <- data.frame(weight, engine_capacity, transmission, max_speed, body_type, row.names = c("Toyota", "Honda", "BMW", "Audi", "Ford"))

# фильтруем автомобили с одинаковым объемом двигателя
filtered_cars <- subset(car_data, engine_capacity == 2.0)

# упорядоченные по алфавиту
sorted_cars <- filtered_cars[order(rownames(filtered_cars)), ]

num_rows <- nrow(sorted_cars)
num_cols <- ncol(sorted_cars)

# Выводим результат
print(car_data)
print(sorted_cars)
cat("Число строк:", num_rows, "\n")
cat("Число столбцов:", num_cols, "\n")

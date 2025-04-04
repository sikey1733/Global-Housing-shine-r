# Загрузка библиотек
library(tidyverse)
library(readr)
library(caret)
library(smotefamily)

# Загрузка данных
Telco_proj <- read_csv("Telco-Customer-Churn.csv")
str(Telco_proj)

# Проверка пропущенных значений
sapply(Telco_proj, function(x) sum(is.na(x)))

# Замена пропущенных значений
Telco_proj$TotalCharges[is.na(Telco_proj$TotalCharges)] <- median(Telco_proj$TotalCharges, na.rm = TRUE)

# Преобразование в факторы и удаление customerID
Telco_proj <- Telco_proj %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-customerID)

# Проверка баланса классов
table(Telco_proj$Churn)

# Сохранение оригинальных уровней факторов
factor_levels <- Telco_proj %>%
  select(where(is.factor)) %>%
  map(levels)

# Создание числовой версии
numeric_data <- Telco_proj %>%
  mutate(across(where(is.factor), as.numeric))

# Разделение на обучающую и тестовую выборки
set.seed(123)
train_idx <- sample(1:nrow(numeric_data), size = 0.8 * nrow(numeric_data))
train_numeric <- numeric_data[train_idx, ]
test <- Telco_proj[-train_idx, ]  

# Устранение дисбаланса и создание сбалансированного датафрейма
smote_result <- SMOTE(
  X = train_numeric[, -which(names(train_numeric) == "Churn")],
  target = train_numeric$Churn,
  K = 5,
  dup_size = 1)

train_balanced <- as.data.frame(smote_result$data)

# Преобразование обратно в факторы
for (col in names(factor_levels)) {
  if(col %in% names(train_balanced)) {
    train_balanced[[col]] <- factor(
      round(as.numeric(train_balanced[[col]])),  
      levels = 1:length(factor_levels[[col]]),
      labels = factor_levels[[col]]
    )
  }
}

# Обработка целевой переменной
train_balanced$Churn <- factor(
  ifelse(train_balanced$class == 1, "No", "Yes"),
  levels = c("No", "Yes")
)
train_balanced$class <- NULL

# Проверка результата
str(train_balanced)
table(train_balanced$Churn)

# Обучение
# Настройка сетки
exp_g <- expand.grid(.mtry = c(2, 4, 6, 8))

# Настройка кросс-валидации
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

# Обучение с настройкой
set.seed(123)
rf_tuned <- train(Churn ~ .,
                  data = train_balanced,
                  method = "rf",
                  trControl = ctrl,
                  tune_grid = exp_g,
                  ntree = 500, 
                  metric = "ROC",
                  importance = TRUE)
print(rf_tuned)
plot(rf_tuned)

# Важность переменных
ggplot(varImp(rf_tuned))

# Проверка модели на контрольной выборке
pred_model <- predict(rf_tuned, test)
confusionMatrix(pred_model, test$Churn)

# Улучшение качества модели
importance_score <- caret::varImp(rf_tuned)
print(importance_score)

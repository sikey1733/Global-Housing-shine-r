library(shiny)
library(shinythemes)
library(shinycssloaders)
library(caret)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Прогнозирование оттока клиентов"),
  sidebarLayout(
    sidebarPanel(
      h4("Демографические данные"),
      selectInput("gender", "Пол:", choices = c("Male", "Female")),
      selectInput("SeniorCitizen", "Пожилой гражданин:", choices = c("Нет" = "0", "Да" = "1")),  
      selectInput("Partner", "Партнер:", choices = c("Yes", "No")),
      selectInput("Dependents", "Иждивенцы:", choices = c("Yes", "No")),
      
      h4("Услуги связи"),
      selectInput("PhoneService", "Телефонная служба:", choices = c("Yes", "No")),
      selectInput("MultipleLines", "Многоканальные линии:", 
                  choices = c("No", "Yes", "No phone service")),
      selectInput("InternetService", "Тип интернет-сервиса:", 
                  choices = c("DSL", "Fiber optic", "No")),
      
      h4("Дополнительные услуги"),
      selectInput("OnlineSecurity", "Онлайн-безопасность:", 
                  choices = c("Yes", "No", "No internet service")),
      selectInput("OnlineBackup", "Онлайн-резервное копирование:", 
                  choices = c("Yes", "No", "No internet service")),
      selectInput("DeviceProtection", "Защита устройства:", 
                  choices = c("Yes", "No", "No internet service")),
      selectInput("TechSupport", "Техподдержка:", 
                  choices = c("Yes", "No", "No internet service")),
      selectInput("StreamingTV", "Потоковое TV:", 
                  choices = c("Yes", "No", "No internet service")),
      selectInput("StreamingMovies", "Потоковые фильмы:", 
                  choices = c("Yes", "No", "No internet service")),
      
      h4("Финансовые данные"),
      numericInput("tenure", "Срок подписки (месяцы):", value = 12, min = 0),
      selectInput("Contract", "Тип договора:", 
                  choices = c("Month-to-month", "One year", "Two year")),
      selectInput("PaperlessBilling", "Безбумажный биллинг:", choices = c("Yes", "No")),
      selectInput("PaymentMethod", "Способ оплаты:", 
                  choices = c("Electronic check", "Mailed check", 
                              "Credit card (automatic)", "Bank transfer (automatic)")),
      numericInput("MonthlyCharges", "Ежемесячные платежи:", value = 50, min = 0),
      numericInput("TotalCharges", "Общая сумма платежей:", value = 1000, min = 0),
      
      actionButton("predict", "Рассчитать риск", class = "btn-primary")
    ),
    mainPanel(
      h4("Результат:"),
      withSpinner(verbatimTextOutput("prediction"), type = 4, color = "#0dc5c1"),
      br(),
      plotOutput("prob_plot")  
    )
  )
)

server <- function(input, output) {
  prob_value <- reactiveVal(NULL)
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      gender = factor(input$gender, levels = c("Male", "Female")),
      SeniorCitizen = as.numeric(input$SeniorCitizen),  
      Partner = factor(input$Partner, levels = c("Yes", "No")),
      Dependents = factor(input$Dependents, levels = c("Yes", "No")),
      tenure = input$tenure,
      PhoneService = factor(input$PhoneService, levels = c("Yes", "No")),
      MultipleLines = factor(input$MultipleLines, levels = c("No", "Yes", "No phone service")),
      InternetService = factor(input$InternetService, levels = c("DSL", "Fiber optic", "No")),
      OnlineSecurity = factor(input$OnlineSecurity, levels = c("Yes", "No", "No internet service")),
      OnlineBackup = factor(input$OnlineBackup, levels = c("Yes", "No", "No internet service")),
      DeviceProtection = factor(input$DeviceProtection, levels = c("Yes", "No", "No internet service")),
      TechSupport = factor(input$TechSupport, levels = c("Yes", "No", "No internet service")),
      StreamingTV = factor(input$StreamingTV, levels = c("Yes", "No", "No internet service")),
      StreamingMovies = factor(input$StreamingMovies, levels = c("Yes", "No", "No internet service")),
      Contract = factor(input$Contract, levels = c("Month-to-month", "One year", "Two year")),
      PaperlessBilling = factor(input$PaperlessBilling, levels = c("Yes", "No")),
      PaymentMethod = factor(input$PaymentMethod, 
                             levels = c("Electronic check", "Mailed check", 
                                        "Credit card (automatic)", "Bank transfer (automatic)")),
      MonthlyCharges = input$MonthlyCharges,
      TotalCharges = input$TotalCharges,
      stringsAsFactors = TRUE
    )
    
    prob <- predict(rf_tuned, new_data, type = "prob")[, "Yes"]
    prob_value(prob)  
    
    output$prediction <- renderText({
      paste("Вероятность оттока:", round(prob*100, 1), "%")
    })
  })
  
  output$prob_plot <- renderPlot({
    req(prob_value())  
    
    ggplot(data.frame(Probability = prob_value()), aes(x = Probability)) +
      geom_segment(aes(x = prob_value(), xend = prob_value(), y = 0, yend = 0.5), 
                   color = "steelblue", size = 1.5) +
      geom_point(aes(x = prob_value(), y = 0.5), 
                 color = "red", size = 5) +
      geom_text(aes(x = prob_value(), y = 0.6, 
                    label = paste0(round(prob_value()*100, 1), "%")),
                size = 6, color = "darkred") +
      scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(title = "Вероятность оттока клиента",
           x = "Вероятность оттока",
           y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })
}

shinyApp(ui = ui, server = server)
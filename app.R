library(shiny)
library(randomForest)

# 1. Load and prepare data (run this BEFORE training model)
data <- read.csv("student-mat.csv", sep = ";")

# Create 'pass' variable
data$pass <- ifelse(data$G3 >= 10, "Pass", "Fail")
data$pass <- as.factor(data$pass)

# Convert character columns to factors - including schoolsup, sex
categorical_cols <- c("school", "sex", "address", "famsize", "Pstatus", "Mjob", 
                      "Fjob", "reason", "guardian", "schoolsup", "famsup", 
                      "paid", "activities", "nursery", "higher", "internet", 
                      "romantic")

for(col in categorical_cols){
  data[[col]] <- as.factor(data[[col]])
}

# Train the model (random forest)
model <- randomForest(pass ~ studytime + failures + absences + internet + famsup + Medu + Fedu + schoolsup + sex,
                      data = data)

# 2. UI part
ui <- fluidPage(
  titlePanel("Student Performance Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("studytime", "Study Time (1-4):", min = 1, max = 4, value = 2),
      sliderInput("failures", "Failures (0-10):", min = 0, max = 10, value = 0),
      sliderInput("absences", "Absences (0-30):", min = 0, max = 30, value = 5),
      
      # Add selectInputs for categorical variables
      selectInput("internet", "Internet Access:", choices = c("yes", "no")),
      selectInput("famsup", "Family Support:", choices = c("yes", "no")),
      selectInput("schoolsup", "School Support:", choices = c("yes", "no")),
      selectInput("sex", "Sex:", choices = c("F", "M")),
      
      sliderInput("Medu", "Mother's Education (0-4):", min = 0, max = 4, value = 2),
      sliderInput("Fedu", "Father's Education (0-4):", min = 0, max = 4, value = 2),
      
      actionButton("predictBtn", "Predict")
    ),
    
    mainPanel(
      h3("Prediction Result:"),
      verbatimTextOutput("result")
    )
  )
)

# 3. Server part
server <- function(input, output) {
  
  observeEvent(input$predictBtn, {
    new_data <- data.frame(
      studytime = as.integer(input$studytime),
      failures = as.integer(input$failures),
      absences = as.integer(input$absences),
      internet = factor(input$internet, levels = levels(data$internet)),
      famsup = factor(input$famsup, levels = levels(data$famsup)),
      schoolsup = factor(input$schoolsup, levels = levels(data$schoolsup)),
      sex = factor(input$sex, levels = levels(data$sex)),
      Medu = as.integer(input$Medu),
      Fedu = as.integer(input$Fedu)
    )
    
    # Fill missing columns with first row values to avoid errors
    missing_cols <- setdiff(names(data), c("pass", names(new_data)))
    for(col in missing_cols){
      new_data[[col]] <- data[[col]][1]
    }
    
    # Predict!
    prediction <- predict(model, new_data)
    
    output$result <- renderText({
      paste("The student is predicted to:", prediction)
    })
  })
  
}

shinyApp(ui = ui, server = server)

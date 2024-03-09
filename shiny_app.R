# Load necessary libraries
library(shiny)
library(randomForest)

# Load the model code (source the file)
source("disease_prediction_model.R")

# Define the encode_user_input function
encode_user_input <- function(user_input) {
  # Convert factors to characters
  user_input$C1 <- as.character(user_input$C1)
  user_input$C5 <- as.character(user_input$C5)
  user_input$C7 <- as.character(user_input$C7)
  user_input$C8 <- as.character(user_input$C8)
  user_input$T1 <- as.character(user_input$T1)
  user_input$T12 <- as.character(user_input$T12)
  user_input$A1 <- as.character(user_input$A1)
  user_input$D1 <- as.character(user_input$D1)
  user_input$D3 <- as.character(user_input$D3)
  user_input$D5 <- as.character(user_input$D5)
  user_input$D12 <- as.character(user_input$D12)
  user_input$P1 <- as.character(user_input$P1)
  user_input$P7 <- as.character(user_input$P7)
  user_input$P10 <- as.character(user_input$P10)
  user_input$F1A <- as.character(user_input$F1A)
  user_input$F1B <- as.character(user_input$F1B)
  user_input$F1C <- as.character(user_input$F1C)
  user_input$F1D <- as.character(user_input$F1D)
  user_input$F1E <- as.character(user_input$F1E)
  user_input$F1F <- as.character(user_input$F1F)
  user_input$F1G <- as.character(user_input$F1G)
  
  # Convert numeric columns to numeric
  user_input$C3 <- as.numeric(user_input$C3)
  user_input$M11 <- as.numeric(user_input$M11)
  user_input$M12 <- as.numeric(user_input$M12)
  
  return(user_input)
}

# Shiny App UI
ui <- fluidPage(
  titlePanel("Health Risk Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Input fields for user data
      selectInput("gender", "Gender:", c("Male" = "1", "Female" = "2"), selected = "1"),
      numericInput("age", "Age:", value = 40),
      selectInput("education", "Education Level:",
                  c("No formal schooling" = "1", 
                    "Less than primary school" = "2", 
                    "Primary school completed" = "3", 
                    "Secondary school completed" = "4", 
                    "High school completed" = "5", 
                    "College/University completed" = "6", 
                    "Postgraduate degree" = "7", 
                    "Refused" = "88"), selected = "5"),
      selectInput("marital_status", "Marital Status:",
                  c("Never married" = "1", 
                    "Currently married" = "2", 
                    "Separated" = "3", 
                    "Divorced" = "4", 
                    "Widowed" = "5", 
                    "Cohabitating" = "6", 
                    "Refused" = "88"), selected = "2"),
      selectInput("employment_status", "Employment Status:",
                  c("Government employee" = "1", 
                    "Non-government employee" = "2", 
                    "Self-employed" = "3", 
                    "Non-paid" = "4", 
                    "Student" = "5", 
                    "Homemaker" = "6", 
                    "Retired" = "7", 
                    "Unemployed (able to work)" = "8", 
                    "Unemployed (unable to work)" = "9", 
                    "Refused" = "88"), selected = "3"),
      selectInput("tobacco_use", "Tobacco Use:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("smokeless_tobacco", "Smokeless Tobacco Use:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("alcohol_consumption", "Alcohol Consumption:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("fruit_consumption", "Fruit Consumption:", c("Yes" = "1", "No" = "2"), selected = "1"),
      selectInput("vegetable_consumption", "Vegetable Consumption:", c("Yes" = "1", "No" = "2"), selected = "1"),
      selectInput("salt_usage", "Salt Usage:",
                  c("Always" = "1", 
                    "Often" = "2", 
                    "Sometimes" = "3", 
                    "Rarely" = "4", 
                    "Never" = "5", 
                    "Don't know" = "77"), selected = "3"),
      selectInput("sunflower_oil_usage", "Sunflower Oil Usage:",
                  c("Mustard oil" = "1", 
                    "Soybean oil" = "2", 
                    "Butter or Pure ghee" = "3", 
                    "Sunflower oil" = "4", 
                    "Other" = "5", 
                    "None in particular" = "6", 
                    "None used" = "7", 
                    "Don’t know" = "77"), selected = "4"),
      selectInput("exercise", "Exercise:", c("Yes" = "1", "No" = "2"), selected = "1"),
      selectInput("cycling", "Cycling:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("sports", "Sports:", c("Yes" = "1", "No" = "2"), selected = "1"),
      selectInput("family_history_diabetes", "Family History of Diabetes:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_bp", "Family History of Blood Pressure:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_stroke", "Family History of Stroke:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_cancer", "Family History of Cancer:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_cholesterol", "Family History of Cholesterol:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_heart_attack", "Family History of Heart Attack:", c("Yes" = "1", "No" = "2"), selected = "2"),
      selectInput("family_history_kidney_disease", "Family History of Kidney Disease:", c("Yes" = "1", "No" = "2"), selected = "2"),
      numericInput("height", "Height (cm):", value = 175),
      numericInput("weight", "Weight (kg):", value = 75),
      
      # Button to trigger prediction
      actionButton("predictButton", "Predict"),
      
      # Button to reset input fields and predictions
      actionButton("resetButton", "Reset"),
    ),
    
    mainPanel(
      # Output to display the prediction results
      verbatimTextOutput("predictionText"),
      
      # Add more panels or visualizations as needed...
    )
  )
)

# Shiny App Server
server <- function(input, output, session) {
  # Event handler for the predict button
  observeEvent(input$predictButton, {
    # Create a data frame with user input
    user_input <- data.frame(
      C1 = as.character(input$gender),  # Gender
      C3 = as.numeric(input$age),  # Age
      C5 = as.character(input$education),  # Education Level
      C7 = as.character(input$marital_status),  # Marital Status
      C8 = as.character(input$employment_status),  # Employment Status
      T1 = as.character(input$tobacco_use),  # Tobacco Use
      T12 = as.character(input$smokeless_tobacco),  # Smokeless Tobacco Use
      A1 = as.character(input$alcohol_consumption),  # Alcohol Consumption
      D1 = as.character(input$fruit_consumption),  # Fruit Consumption
      D3 = as.character(input$vegetable_consumption),  # Vegetable Consumption
      D5 = as.character(input$salt_usage),  # Salt Usage
      D12 = as.character(input$sunflower_oil_usage),  # Sunflower Oil Usage
      P1 = as.character(input$exercise),  # Exercise
      P7 = as.character(input$cycling),  # Cycling
      P10 = as.character(input$sports),  # Sports
      F1A = as.character(input$family_history_diabetes),  # Family History of Diabetes
      F1B = as.character(input$family_history_bp),  # Family History of Blood Pressure
      F1C = as.character(input$family_history_stroke),  # Family History of Stroke
      F1D = as.character(input$family_history_cancer),  # Family History of Cancer
      F1E = as.character(input$family_history_cholesterol),  # Family History of Cholesterol
      F1F = as.character(input$family_history_heart_attack),  # Family History of Heart Attack
      F1G = as.character(input$family_history_kidney_disease),  # Family History of Kidney Disease
      M11 = as.numeric(input$height),  # Height
      M12 = as.numeric(input$weight)  # Weight
    )
    
    # Encode user input
    user_input_encoded <- encode_user_input(user_input)
    
    # List to store predictions for each target variable
    predictions_list <- list()
    probabilities_list <- list()  # Store probability values
    
    for (target_variable in disease_target_variables) {
      # Predict for the current target variable
      predictions <- predict(disease_rf_models[[target_variable]]$model, newdata = user_input_encoded, type = "response")
      
      # Map predictions to meaningful labels
      predictions_mapped <- ifelse(predictions == 1, "Yes","No" )
      
      # Store the predictions in the list
      predictions_list[[target_variable]] <- predictions_mapped
      
      # Store the probability values
      probabilities_list[[target_variable]] <- if (!is.na(predictions_mapped) && length(predictions_mapped) > 0) {
        paste("Probability:", as.character(round(as.numeric(predictions[predictions == "2"]) * 100, 2)), "%")

      } else {
        "Probability: NA"
      }
    }
    
    # Display the predictions and probability values for each target variable
    output$predictionText <- renderText({
      result <- ""
      for (i in seq_along(disease_target_variables)) {
        target_variable <- disease_target_variables[i]
        prediction <- predictions_list[[target_variable]]
        probability <- probabilities_list[[target_variable]]
        
        # Mapping between variable names and health conditions
        health_condition_map <- c("H2a" = "Raised Blood Pressure",
                                  "H7a" = "Diabetes",
                                  "H13a" = "Cholesterol",
                                  "H17" = "Cardiovascular Diseases",
                                  "X1" = "Chronic Kidney Diseases")
        
        # Replace variable names with health conditions
        condition_name <- health_condition_map[target_variable]
        
        if (is.character(prediction) && length(prediction) > 0) {
          result <- paste(result, paste("Prediction for", condition_name, ":", prediction, "\n", probability, "\n"), "\n")
        } else {
          result <- paste(result, paste("Prediction for", condition_name, ": NA", "\nProbability: NA", "\n"), "\n")
        }
      }
      result
    })
    
    

  })
  
  
  
  
  # Event handler for the reset button
  observeEvent(input$resetButton, {
    # Reset all input fields to default values
    updateSelectInput(session, "gender", selected = "1")
    updateNumericInput(session, "age", value = 40)
    updateSelectInput(session, "education", selected = "5")
    updateSelectInput(session, "marital_status", selected = "2")
    updateSelectInput(session, "employment_status", selected = "3")
    updateSelectInput(session, "tobacco_use", selected = "2")
    updateSelectInput(session, "smokeless_tobacco", selected = "2")
    updateSelectInput(session, "alcohol_consumption", selected = "2")
    updateSelectInput(session, "fruit_consumption", selected = "1")
    updateSelectInput(session, "vegetable_consumption", selected = "1")
    updateSelectInput(session, "salt_usage", selected = "3")
    updateSelectInput(session, "sunflower_oil_usage", selected = "4")
    updateSelectInput(session, "exercise", selected = "1")
    updateSelectInput(session, "cycling", selected = "2")
    updateSelectInput(session, "sports", selected = "1")
    updateSelectInput(session, "family_history_diabetes", selected = "2")
    updateSelectInput(session, "family_history_bp", selected = "2")
    updateSelectInput(session, "family_history_stroke", selected = "2")
    updateSelectInput(session, "family_history_cancer", selected = "2")
    updateSelectInput(session, "family_history_cholesterol", selected = "2")
    updateSelectInput(session, "family_history_heart_attack", selected = "2")
    updateSelectInput(session, "family_history_kidney_disease", selected = "2")
    updateNumericInput(session, "height", value = 175)
    updateNumericInput(session, "weight", value = 75)
    
    # Clear the prediction text
    output$predictionText <- renderText("")
  })
}

# Run the Shiny App
shinyApp(ui, server)
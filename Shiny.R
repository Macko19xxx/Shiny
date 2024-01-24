# Instalacja i ładowanie pakietu Shiny, jeśli jeszcze nie jest zainstalowany
if (!require("shiny")) {
  install.packages("shiny")
}
library(shiny)

# Instalacja i ładowanie pakietu ggplot2, jeśli jeszcze nie jest zainstalowany
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

# Przykładowe dane i funkcja do generowania wyników i wykresów
generate_results_and_plots <- function(category) {
  # Tutaj dodaj kod do generowania wyników i wykresów dla danej kategorii
  # Możesz dostosować to do swoich potrzeb
  if (category == "Category A") {
    data <- data.frame(
      Variable = c("Var1", "Var2", "Var3"),
      Value = c(10, 20, 15)
    )
    results <- c("Result A1", "Result A2", "Result A3")
  } else if (category == "Category B") {
    data <- data.frame(
      Variable = c("Var1", "Var2", "Var3"),
      Value = c(5, 15, 25)
    )
    results <- c("Result B1", "Result B2", "Result B3")
  } else {
    data <- NULL
    results <- NULL
  }
  
  # Generowanie wykresu słupkowego
  bar_plot <- ggplot(data, aes(x = Variable, y = Value)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Bar Plot", x = "Variable", y = "Value")
  
  # Generowanie wykresu punktowego
  scatter_plot <- ggplot(data, aes(x = Variable, y = Value)) +
    geom_point(color = "orange") +
    labs(title = "Scatter Plot", x = "Variable", y = "Value")
  
  return(list(results, bar_plot, scatter_plot))
}

# Definicja interfejsu Shiny
ui <- fluidPage(
  titlePanel("Data Quality Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Przyciski do wyboru kategorii
      radioButtons("category", "Choose a category:",
                   choices = c("Category A", "Category B"),
                   selected = "Category A"),
      
      # Przycisk do rozpoczęcia obliczeń
      actionButton("startButton", "Start Calculations")
    ),
    
    mainPanel(
      # Wyświetlanie wyników
      verbatimTextOutput("resultsOutput"),
      
      # Wyświetlanie wykresów
      plotOutput("barPlot"),
      plotOutput("scatterPlot"),
      
      # Wyświetlanie tabeli z wynikami
      tableOutput("resultTable"),
      # Opis tabeli
      HTML("<p><b>Table with Results:</b> This table displays the results for the selected category.</p>")
    )
  )
)

# Definicja serwera Shiny
server <- function(input, output, session) {
  # Obliczenia i wyświetlanie wyników, wykresów w zależności od wybranej kategorii
  output$resultsOutput <- renderPrint({
    category <- input$category
    results_and_plots <- generate_results_and_plots(category)
    return(results_and_plots[[1]])
  })
  
  # Wyświetlanie wykresów
  output$barPlot <- renderPlot({
    category <- input$category
    results_and_plots <- generate_results_and_plots(category)
    print(results_and_plots[[2]])
  })
  
  output$scatterPlot <- renderPlot({
    category <- input$category
    results_and_plots <- generate_results_and_plots(category)
    print(results_and_plots[[3]])
  })
  
  # Wyświetlanie tabeli z wynikami
  output$resultTable <- renderTable({
    category <- input$category
    results_and_plots <- generate_results_and_plots(category)
    data.frame(Variable = c("Result 1", "Result 2", "Result 3"),
               Value = results_and_plots[[1]])
  })
  
  # Obsługa przycisku do rozpoczęcia obliczeń
  observeEvent(input$startButton, {
    # Okno dialogowe do wprowadzenia ścieżki do skryptu
    path <- shiny::modalDialog(
      textInput("scriptPath", "Enter the path to the script:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmScriptPath", "Confirm", class = "btn-primary")
      )
    )
    
    # Wyświetlenie okna dialogowego
    showModal(path)
    
    # Obsługa naciśnięcia przycisku "Confirm"
    observeEvent(input$confirmScriptPath, {
      # Pobranie wprowadzonej ścieżki
      script_path <- input$scriptPath
      
      # Wczytanie kodu z innego skryptu
      source(script_path, local = TRUE)
      
      # Dodaj kod z innego skryptu, który chcesz uruchomić po naciśnięciu przycisku
      # Na przykład, jeśli masz funkcję calculate_data() w innym skrypcie, możesz ją teraz wywołać
      # calculate_data()
      
      # Sprawdź, czy obiekt o nazwie "XXX" istnieje i jeśli tak, wyświetl go
      if (exists("XXX")) {
        print(XXX)
      }
      
      # Ukrycie okna dialogowego po wykonaniu obliczeń
      removeModal()
    })
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui, server)

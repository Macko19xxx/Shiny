# Dodatkowe dane
dane_techniczny_poprzedni_miesiac <- data.frame(x = 1:10, y = rnorm(10), grupa = "Techniczny", miesiac = "Poprzedni")
dane_biznesowy_poprzedni_miesiac <- data.frame(x = 1:10, y = rnorm(10), grupa = "Biznesowy", miesiac = "Poprzedni")

dane_techniczny_biezacy_miesiac <- data.frame(x = 1:10, y = rnorm(10), grupa = "Techniczny", miesiac = "Bieżący")
dane_biznesowy_biezacy_miesiac <- data.frame(x = 1:10, y = rnorm(10), grupa = "Biznesowy", miesiac = "Bieżący")

# Aktualizacja interfejsu Shiny
ui <- fluidPage(
  titlePanel("Wybór opcji"),
  sidebarLayout(
    sidebarPanel(
      selectInput("grupa", "Wybierz grupę:",
                  choices = c("Techniczny", "Biznesowy")),
      selectInput("opcja", "Wybierz opcję:",
                  choices = c("Opcja 1", "Opcja 2", "Opcja 3"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabela", 
                 tableOutput("tabela"),
                 textOutput("opis")),
        tabPanel("Wykres", 
                 plotOutput("wykres"),
                 textOutput("opis_wykresu"))
      )
    )
  )
)

# Aktualizacja serwera Shiny
server <- function(input, output) {
  
  dane_wybrane <- reactive({
    switch(input$grupa,
           "Techniczny" = switch(input$opcja,
                                 "Opcja 1" = dane_techniczny_poprzedni_miesiac,
                                 "Opcja 2" = NULL,
                                 "Opcja 3" = dane_techniczny_biezacy_miesiac),
           "Biznesowy" = switch(input$opcja,
                                "Opcja 1" = dane_biznesowy_poprzedni_miesiac,
                                "Opcja 2" = dane_biznesowy_biezacy_miesiac,
                                "Opcja 3" = dane_biznesowy_biezacy_miesiac))
  })
  
  output$tabela <- renderTable({
    dane_wybrane()
  })
  
  output$wykres <- renderPlot({
    if (!is.null(dane_wybrane())) {
      ggplot(dane_wybrane(), aes(x = x, y = y, color = miesiac)) + 
        geom_point() +
        ggtitle("Wykres dla wybranej opcji, grupy i miesiąca") +
        theme_minimal()
    } else {
      NULL
    }
  })
  
  output$opis <- renderText({
    paste("Wybrane opcje: Grupa -", input$grupa, ", Opcja -", input$opcja)
  })
  
  output$opis_wykresu <- renderText({
    if (!is.null(dane_wybrane())) {
      paste("Wybrane opcje: Grupa -", input$grupa, ", Opcja -", input$opcja, ", Miesiąc -", input$miesiac)
    } else {
      "There is no plot available for this option"
    }
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui, server)

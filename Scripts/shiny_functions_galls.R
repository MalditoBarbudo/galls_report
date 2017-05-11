## Funciones para los frameworks interactivos del anáĺisis de agallas
## 

galls_paired_t_test <- function(){
  
  require(shiny)
  require(dplyr)
  require(ggplot2)
  
  shinyApp(
    
    
    ui = fluidPage(
      fluidRow(
        style = "padding-bottom: 20px;",
        column(4, selectInput('hormonas', label = 'Hormona a visualizar',
                              choices = list("ABA" = "ABA", "BA" = "BA",
                                             "BK" = "BK", "DHZ" = "DHZ",
                                             "GA9" = "GA9", "IAA" = "IAA",
                                             "iP" = "IP", "iPA" = "IPA",
                                             "JA" = "JA", "RDHZ" = "RDHZ",
                                             "SA" = "SA", "Z" = "Z"),
                              selected = 'ABA')),
        column(4, selectInput('dataset', label = 'Datos',
                              choices = list("Cantidad (ng/g Pf)" = 'data_pgrs_cantidad_or',
                                             "Concentración (ng/mL)" = 'data_pgrs_conc_or'),
                              selected = "data_pgrs_cantidad_or"))
      ),
      fluidRow(
        verbatimTextOutput('t_test')
      ),
      fluidRow(
        plotOutput('boxplot', height = '250px', width = '400px')
      )
    ),
    
    server = function(input, output, session){
      load('t_test_data.Rdata')
      dataset <- reactive({
        switch(input$dataset,
               'data_pgrs_cantidad_or' = data_pgrs_cantidad_or,
               'data_pgrs_conc_or' = data_pgrs_conc_or) %>%
          select_(input$hormonas, 'ID') %>%
          rename_(hormona = input$hormonas) %>%
          mutate_(ID = quote(as.factor(ID)))
        })
      output$t_test <- renderPrint({
        t.test(hormona~ID, data = dataset(),
               paired = TRUE)
      })
      
      output$boxplot <- renderPlot({
        dataset() %>%
          ggplot(aes(x = ID, y = hormona, fill = ID)) +
          geom_boxplot(outlier.size = 0) +
          geom_point(aes(colour = ID),
                     position = position_jitter(width = .25, height = 0),
                     shape = 19, size = 2) +
          labs(x = '', y = 'Regulador') +
          scale_fill_manual(values = c('#EF4836', '#446CB3')) +
          scale_colour_manual(values = c('#96281B', '#1F3A93')) +
          theme_bw() +
          theme(axis.ticks.x = element_line(size = 1, colour = 'black'),
                axis.line = element_line(size = 1, colour = "black"),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.border = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(size = .5, colour = 'grey80',
                                                  linetype = 'dotted'),
                panel.grid.major.x = element_blank(),
                panel.background = element_rect(fill = 'grey95'),
                legend.background = element_blank(),
                legend.key = element_blank(),
                text = element_text(family = "Raleway", size = 12))
      })
    },
    options = list(height = 600)
  )
}

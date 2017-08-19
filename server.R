library(shiny)
library(plotly)
library(data.table)



function(input, output, session) {
  source('my_funct.R')
  output$my_data <- renderDataTable(
    DT::datatable(get_data(),extensions = c('Buttons','FixedHeader'),class = 'cell-border stripe',rownames = FALSE,
                  filter = 'top', options = list(dom = 'Blfrtip', fixedHeader = TRUE,pageLength = 50,lengthMenu = c(10,50,500,5000, 10000, 25000 ),
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                 columnDefs = list())) 
    
  )
  
}
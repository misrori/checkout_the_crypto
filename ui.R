library(shiny)
library(plotly)
library(DT)
library(data.table)
library(plotly)
library(shinycssloaders)


# for spinners 2-3 match the background color of wellPanel
options(spinner.color.background="#F5F5F5")

navbarPage(
  title="Crypto report",theme = "cosmo",
  tabPanel("Analysis",
           withSpinner(dataTableOutput('my_data'),type = 4)
           #dataTableOutput('my_data')
  ), tabPanel("Detailed results", 
              withSpinner(dataTableOutput('my_data2'),type = 4))
  
  
)#nav

library(shinydashboard)
header <- dashboardHeader( title = "COVID-19 BAZA OKUŽB", uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<- dashboardPage(header, sidebar, body, skin = "blue")



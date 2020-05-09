
library(knitr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(maptools)
library(rmarkdown)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(shinyalert)
library(RPostgreSQL)
library(shinyBS)
library(dbplyr)
library(sodium)
library(DT)
library(lubridate)
library(scales)

db = 'sem2020_jank'
host = 'baza.fmf.uni-lj.si'
user = 'jank'
password = 'vajinakodazapostgre'


#===========================================Generiram Login Page======================================================
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Prijava", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Uporabnik", label = tagList(icon("user"), "Uporabnik")),
                   passwordInput("passwd", placeholder="Geslo", label = tagList(icon("unlock-alt"), "Geslo")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "Prijava", style = "color: white; background-color:#DF3232;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Napačno uporabniško ime ali geslo!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Uporabnik: gost  Geslo: gost"),
                     br()
                   ))
)

#===========================================Dodelim app access======================================================
credentials = data.frame(
  username_id = c("jan", "aljosa","filip","gost"),
  password   = sapply(c("opb", "opb","opb","gost"),password_store),
  permission  = c("advanced", "advanced","advanced","basic"), 
  stringsAsFactors = F
)

#===========================================lepotni nastavki=====================================================

header <- dashboardHeader( title = "COVID-19 BAZA OKUŽB", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

#===========================================Kar bo obvezno za dopolnit v obrazec======================================================
fieldsMandatory <- c("ime")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  #===========================================DB conneciton==============================================================
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
  
  dbGetQuery(conn, "SET CLIENT_ENCODING TO 'utf8'; SET NAMES 'utf8'")
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    dbDisconnect(conn) 
  })
  
  #===========================================Access checkpoint==============================================================
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["password"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }
  })
  
  #===========================================logout button================================================================
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #ffffff !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  #===========================================Ločeno kaj vidi akreditiran uporabnik in kaj vsi======================================================
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("Statistični podatki", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Seznam bolnisnic", tabName = "Bolnisnice", icon = icon("th")),
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Hospitalizirani", tabName = "Hospitalizirani", icon = icon("th")),
          menuItem("Simptomi", tabName = "Simptomi", icon = icon("th")),
          menuItem("Ima", tabName = "Ima", icon = icon("th")),
          menuItem("Obrazec", tabName = "Obrazec", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
        menuItem("Statistični podatki", tabName = "dashboard", icon = icon("dashboard"))
         
        )
        
      }
    }
  })
  
  #===========================================Plot stevilo okuzb na dan======================================================
  
  output$ggp_st_okuzb <- renderPlot({ 
    st_okuzb <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT datum_testiranja FROM oseba WHERE stanje='bolnik'", con=conn))))
    colnames(st_okuzb)<-c("Datum","St_poz")
    st_okuzb$Datum <- as.Date(factor(st_okuzb$Datum))
      ggp_st_okuzb <- ggplot((data=st_okuzb),aes(x=st_okuzb$Datum,y=st_okuzb[,2])) +
      geom_point() +geom_line() + theme_classic()+ geom_hline(yintercept=0, linetype="dashed", color = "blue")+scale_x_date("Datum",labels = date_format("%Y-%m-%d"))+ scale_y_continuous("Št.Pozitivnih st_okuzbov")
      plot(ggp_st_okuzb)})
  
  #===========================================Plot stevilo okuzb skupaj======================================================
  
  output$ggp_st_okuzb_skupaj <- renderPlot({ 
    st_okuzb <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT datum_testiranja FROM oseba WHERE stanje='bolnik'", con=conn))))
    sestevek<-c(0)
    for (i in 2:(length(rownames(st_okuzb))+1)){
      sestevek <- append(sestevek, as.numeric(sestevek[i-1])+as.numeric(st_okuzb$Freq[i-1]))
    }
    sestevek<-sestevek[-1]
    st_okuzb$sestevek <- sestevek
    colnames(st_okuzb)<-c("Datum","St_poz","sestevek")
    st_okuzb$Datum <- as.Date(factor(st_okuzb$Datum))
    ggp_st_okuzb <- ggplot((data=st_okuzb),aes(x=st_okuzb$Datum,y=st_okuzb[,3])) +
      geom_point() +geom_line() + theme_classic()+ geom_hline(yintercept=0, linetype="dashed", color = "blue")+scale_x_date("Datum",labels = date_format("%Y-%m-%d"))+ scale_y_continuous("Št.Pozitivnih st_okuzbov")
    plot(ggp_st_okuzb)})
  
  #===========================================Lista zdravnikov==========================================================
  zdravniki <- dbGetQuery(conn, build_sql("SELECT ime FROM oseba WHERE stanje = 'zd_delavec_na_dolznosti'", con=conn))
  zdravniki <- zdravniki$ime
  
  #===========================================Generiram zdravnikov page======================================================
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        #===========================================Sidebar======================================================      
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            div(fluidRow(
              box(width = 12, title = 'Število novih okužb z COVID-19', plotOutput("ggp_st_okuzb"))),
              fluidRow(
                box(width = 12, title = 'Skupno število potrjenih okužb z COVID-19', plotOutput("ggp_st_okuzb_skupaj")))
            ))
          ,
          tabItem(
            tabName ="Bolnisnice",
            fluidRow(
              box(width = 12, title = "Podatki o bolnisnicah", dataTableOutput('results_b'))
            )
          ),
          tabItem(
            tabName ="Pacienti",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih", dataTableOutput('results'))
            )
          ),
          tabItem(
            tabName ="Simptomi",
            fluidRow(
              box(width = 12, title = "Simptomi", dataTableOutput('results_p'))
            )
          ),
          tabItem(
            tabName ="Ima",
            fluidRow(
              box(width = 12, title = "Ima", dataTableOutput('results_k'))
            )
          ),
          tabItem(
            tabName ="Hospitalizirani",
            fluidRow(
              box(width = 12, title = "Hospitalizirani", dataTableOutput('results_h'))
            )
          ),
          tabItem(
            tabName ="Obrazec",
            fluidPage(
              titlePanel("Obrazec za dodajanje osebe"),
              h4("Vsa polja so obvezna!"),
              div(
                tags$style(HTML("
                                input:invalid {
                                background-color: #FFCCCC;
                                }")),
                
                #==========================================Obrazec za dodajanje ljudi====================================================== 

                useShinyjs(),
                shinyjs::inlineCSS(appCSS),
                

                bsAlert("alert"),
                id = "form",
                textInput("ime", "Ime in priimek", ""),
                
                textInput("naslov", "Naslov"),
                
                textInput("davcna", "Davčna števiilka"),
                
                selectInput("zdravnik", "Zdravnik", zdravniki),
                
                dateInput("dat", "Datum vpisa v evidenco", format = "dd-mm-yyyy"),
                
                selectInput("stanje", "Stanje", c("bolnik", "zd_delavec_na_dolznosti", "zdrav")),
                
                checkboxInput("hospitalizacija", "Ali je potrebna hospitalizacija?", FALSE),
                
                checkboxInput("glavobol", "Glavobol", FALSE),
                numericInput("glavobol_j", "Jakost glavobola", value = NULL, min = 1, max = 10, step =  1),
                dateInput("glavobol_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("vrocina", "Vročina", FALSE),
                numericInput("vrocina_j", "Jakost vročine", value = NULL, min = 1, max = 10, step =  1),
                dateInput("vrocina_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("tekoce_blato", "Tekoče blato", FALSE),
                numericInput("tekoce_blato_j", "Jakost tekočega blata", value = NULL, min = 1, max = 10, step =  1),
                dateInput("tekoce_blato_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("sibkost", "šibkost", FALSE),
                numericInput("sibkost_j", "Jakost šibkosti", value = NULL, min = 1, max = 10, step =  1),
                dateInput("sibkost_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("mialgija", "Mialgija", FALSE),
                numericInput("mialgija_j", "Jakost mialgije", value = NULL, min = 1, max = 10, step =  1),
                dateInput("mialgija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("angina_pectoris", "Angina pectoris", FALSE),
                numericInput("angina_pectoris_j", "Jakost angine pectoris", value = NULL, min = 1, max = 10, step =  1),
                dateInput("angina_pectoris_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("dispneja", "Dispneja", FALSE),
                numericInput("dispneja_j", "Jakost dispneje", value = NULL, min = 1, max = 10, step =  1),
                dateInput("dispneja_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("anzomija", "Anzomija", FALSE),
                numericInput("anzomija_j", "Jakost anzomije", value = NULL, min = 1, max = 10, step =  1),
                dateInput("anzomija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                checkboxInput("agevzija", "Agevzija", FALSE),
                numericInput("agevzija_j", "Jakost agevzije", value = NULL, min = 1, max = 10, step =  1),
                dateInput("agevzija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
                
                useShinyalert(),
                actionButton("submit", "Dodaj v bazo", class = "btn-primary")
                )
                )
              )
        )
      } 
      else {
        #===========================================Generiram page za javnost======================================================
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            div(fluidRow(
              box(width = 12, title = 'Število novih okužb z COVID-19', plotOutput("ggp_st_okuzb"))),
              fluidRow(
                box(width = 12, title = 'Skupno število potrjenih okužb z COVID-19', plotOutput("ggp_st_okuzb_skupaj")))
            ))
        )
        
      }
      
    }
    else {
      loginpage
    }
  })
  #===========================================Pridobim tabele iz baze======================================================
  output$results <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM oseba", con=conn))
  })
  output$results_b <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM lokacije", con = conn))
  })
  output$results_p <-  DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM simptom", con = conn))
  })
  output$results_k <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM ima", con=conn))
  })
  output$results_h <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM bolnik", con=conn))
  })
  
  #==========================================Polnenje DB======================================================
  observe({
    #===========================================Checkpoint obvezna polja======================================================
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)


    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  observeEvent(input$submit, {
    
      dbGetQuery(conn, build_sql("INSERT INTO oseba (ime, davcna_st, naslov, datum_testiranja, stanje)
                                             VALUES (", input$ime, ",", input$davcna, ", ", input$naslov, ",", input$dat, ", ", input$stanje,")", con = conn))
    #===========================================ta del je treba popravit======================================================
        # if (input$hospitalizacija == TRUE){
        #   c<-paste0("SELECT davcna_st FROM oseba WHERE ime = ","'",as.name('Nolly Glazzard'),"'",sep="")
        #   id_zdravnika <- dbGetQuery(conn, build_sql(c, con = conn))
        #   id_zdravnika <- id_zdravnika$id_zdravnika[1]
        #   dbGetQuery(conn, build_sql("INSERT INTO bolnik (id_pacienta, id_zdravnika, hospitalizacija)
        #                              VALUES (", input$davcna, ",", id_zdravnika, ",", 1,")", con = conn))
        # }
    #===========================================================================================================================  
        if (input$agevzija == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                             VALUES (", input$davcna, ",", 9, ",", input$agevzija_j, ",", input$agevzija_dat,")", con = conn))
        }
        
    
        if (input$anzomija == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 8, ",", input$anzomija_j, ",", input$anzomija_dat,")", con = conn))
        }
      
        if (input$angina_pectoris == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",",6 , ",", input$angina_pectoris_j, ",", input$angina_pectoris_dat,")", con = conn))
        }
        
        if (input$dispneja == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 7, ",", input$dispneja_j, ",", input$dispneja_dat,")", con = conn))
        }
        
        if (input$glavobol == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 1, ",", input$glavobol_j, ",", input$glavobol_dat,")", con = conn))
        }
        
        if (input$mialgija == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 5, ",", input$mialgija_j, ",", input$mialgija_dat,")", con = conn))
        }
        
        if (input$sibkost == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 4, ",", input$sibkost_j, ",", input$sibkost_dat,")", con = conn))
        }
        
        if (input$tekoce_blato == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                     VALUES (", input$davcna, ",", 3, ",", input$tekoce_blato_j, ",", input$tekoce_blato_dat,")", con = conn))
        }
        
        if (input$vrocina == TRUE){
          dbGetQuery(conn, build_sql("INSERT INTO ima (id_pacienta, id_simptomi, jakost, datum_pojavitve)
                                         VALUES (", input$davcna, ",", 2, ",", input$vrocina_j, ",", input$vrocina_dat,")", con = conn))
        }

      
      
      shinyalert("OK!", "Oseba je dodana v sistem.", type = "success")
    
    
  })
  observeEvent(input$submit, {
    reset("body")
  })

  }

runApp(list(ui = ui, server = server))




 
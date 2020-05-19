library("knitr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maptools")
library("rmarkdown")
library("shiny")
library("shinyjs")
library("shinydashboard")
library("shinyFeedback")
library("shinyalert")
library("RPostgreSQL")
library("shinyBS")
library("dbplyr")
library("sodium")
library("DT")
library("lubridate")
library("scales")
library("RColorBrewer")
library("grid")

options(encoding = 'UTF-8')

db='sem2020_jank'
host='baza.fmf.uni-lj.si'
user='javnost'
password='javnogeslo'


fte_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

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
  
  
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  #===========================================DB conneciton==============================================================
  DB_PORT <- as.integer(Sys.getenv("POSTGRES_PORT"))
  if (is.na(DB_PORT)) {
    DB_PORT <- 5432
  }
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password, port=DB_PORT)
  
  dbGetQuery(conn, "SET CLIENT_ENCODING TO 'utf8'; SET NAMES 'utf8'")
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    sapply(dbListResults(conn), dbClearResult)
    dbDisconnect(conn) 
  })
  credentials <- dbGetQuery(conn, build_sql("SELECT * FROM credentials", con=conn))
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
          menuItem("Obrazec", tabName = "Obrazec", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Statistični podatki", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Seznam bolnisnic", tabName = "Bolnisnice", icon = icon("th"))
          
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
      geom_point(color="#c0392b", alpha=0.90, size = 2) + geom_path(color = "black")+ 
      fte_theme() + geom_hline(yintercept=0, linetype="solid", color = "black")+ 
      scale_x_date(labels = date_format("%d-%m-%Y"))+ scale_y_continuous() + xlab("Datum") + ylab("Število novo okuženih") + 
      theme(axis.title.x = element_text(color="black", size=11, face="bold"),
            axis.title.y = element_text(color="black", size=11, face="bold"))
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
      geom_point(color="#c0392b", alpha=0.9, size = 2) + geom_path(color="black") +
      fte_theme() + geom_hline(yintercept=0, linetype="solid", color = "black") + 
      scale_x_date(labels = date_format("%Y-%m-%d"))+ scale_y_continuous() +
      xlab("Datum") + ylab("Število pozitivnih okužb") + 
      theme(axis.title.x = element_text(color="black", size=11, face="bold"),
            axis.title.y = element_text(color="black", size=11, face="bold"))
    plot(ggp_st_okuzb)})
  
  #===========================================stolpicni diagram zasedenost bolnisnic =======================================================
  
  output$diagram_zasedenost <- renderPlot({ 
    st_pacientov_zdravnik <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT id_zdravnika FROM bolnik", con=conn))))
    zdravnik_lokacija <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT * FROM zd_delavec_na_dolznosti", con=conn))))
    lokacije_postelje <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT * FROM lokacije", con=conn))))
    lokacija <- as.data.frame(table(dbGetQuery(conn, build_sql("SELECT id, lokacija FROM lokacije", con=conn))))
    zdravnik_lokacija <- filter(zdravnik_lokacija, Freq == 1 )
    lokacije_postelje <- filter(lokacije_postelje, Freq == 1)
    zdravnik_lokacija <- zdravnik_lokacija %>% select(id, zd_ustanova_id_c)
    lokacija <- filter(lokacija, Freq == 1)
    lokacija <- lokacija %>% select(id, lokacija)
    podatki1 <- merge(y = lokacija,x = zdravnik_lokacija, by.x='zd_ustanova_id_c', by.y = 'id')
    podatki2 <- merge(y = podatki1,x = st_pacientov_zdravnik, by.x='Var1', by.y = 'id')
    podatki3 <- podatki2 %>% group_by(lokacija) %>% summarise(stevilo_pacientov = sum(Freq))
    podatki <- merge(y = podatki3,x = lokacije_postelje, by.x='lokacija', by.y = 'lokacija')
    podatki <- podatki %>% select(lokacija, st_postelj, stevilo_pacientov)
    podatki <- as.data.frame(podatki)
    podatki_diag <- gather(podatki, -lokacija, key = "lastnost", value = "vrednost")
    podatki_diag <- as.data.frame(podatki_diag)
    podatki_diag[,3] <- as.numeric(podatki_diag[,3])
    podatki_diag$lastnost[podatki_diag$lastnost == 'st_postelj'] <- 'Število postelj'
    podatki_diag$lastnost[podatki_diag$lastnost == 'stevilo_pacientov'] <- 'Število pacientov'
    diagram_zasedenost <- ggplot(data = podatki_diag, aes(x = lokacija, y = vrednost, fill = lastnost)) +
      geom_bar(stat = 'identity',position = 'dodge2') + xlab("Lokacija") + ylab("Število") + 
      theme(axis.title.x = element_text(color="black", size=11, face="bold"),
            axis.title.y = element_text(color="black", size=11, face="bold"))
    plot(diagram_zasedenost)})
  
  
  
  
  
  output$obrazec.glavobol <- renderUI({
    validate(need(!is.null(input$glavobol), ""))
    div({
      if (input$glavobol) {
        div(
          numericInput("glavobol_j", "Jakost cefalgije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("glavobol_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.vrocina <- renderUI({
    validate(need(!is.null(input$vrocina), ""))
    div({
      if (input$vrocina) {
        div(
          numericInput("vrocina_j", "Jakost hipertermije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("vrocina_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.tekoce_blato <- renderUI({
    validate(need(!is.null(input$tekoce_blato), ""))
    div({
      if (input$tekoce_blato) {
        div(
          numericInput("tekoce_blato_j", "Jakost diareje", value = NULL, min = 1, max = 10, step =  1),
          dateInput("tekoce_blato_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.sibkost <- renderUI({
    validate(need(!is.null(input$sibkost), ""))
    div({
      if (input$sibkost) {
        div(
          numericInput("sibkost_j", "Jakost astenije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("sibkost_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.mialgija <- renderUI({
    validate(need(!is.null(input$mialgija), ""))
    div({
      if (input$mialgija) {
        div(
          numericInput("mialgija_j", "Jakost mialgije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("mialgija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  
  
  output$obrazec.angina_pectoris <- renderUI({
    validate(need(!is.null(input$angina_pectoris), ""))
    div({
      if (input$angina_pectoris) {
        div(
          numericInput("angina_pectoris_j", "Jakost angine pectoris", value = NULL, min = 1, max = 10, step =  1),
          dateInput("angina_pectoris_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  
  output$obrazec.dispneja <- renderUI({
    validate(need(!is.null(input$dispneja), ""))
    div({
      if (input$dispneja) {
        div(
          numericInput("dispneja_j", "Jakost dispneje", value = NULL, min = 1, max = 10, step =  1),
          dateInput("dispneja_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.anzomija <- renderUI({
    validate(need(!is.null(input$anzomija), ""))
    div({
      if (input$anzomija) {
        div(
          numericInput("anzomija_j", "Jakost anzomije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("anzomija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  output$obrazec.agevzija <- renderUI({
    validate(need(!is.null(input$agevzija), ""))
    div({
      if (input$agevzija) {
        div(
          numericInput("agevzija_j", "Jakost agevzije", value = NULL, min = 1, max = 10, step =  1),
          dateInput("agevzija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy")
        )
      }
    })
  })
  
  
  
  output$obrazec.pogojno <- renderUI({
    validate(need(!is.null(input$stanje), ""))
    div({
      if (input$stanje == "bolnik") {
        div(
          selectInput("zdravnik", "Zdravnik", zdravniki),
          checkboxInput("hospitalizacija", "Ali je potrebna hospitalizacija?", FALSE),
          checkboxInput("glavobol", "Cefalgija", FALSE),
          uiOutput("obrazec.glavobol"),
          checkboxInput("vrocina", "Hipertermija", FALSE),
          uiOutput("obrazec.vrocina"),
          checkboxInput("tekoce_blato", "Diareja", FALSE),
          uiOutput("obrazec.tekoce_blato"),
          checkboxInput("sibkost", "Astenija", FALSE),
          uiOutput("obrazec.sibkost"),
          checkboxInput("mialgija", "Mialgija", FALSE),
          uiOutput("obrazec.mialgija"),
          checkboxInput("angina_pectoris", "Angina pectoris", FALSE),
          uiOutput("obrazec.angina_pectoris"),
          checkboxInput("dispneja", "Dispneja", FALSE),
          uiOutput("obrazec.dispneja"),
          checkboxInput("anzomija", "Anzomija", FALSE),
          uiOutput("obrazec.anzomija"),
          checkboxInput("agevzija", "Agevzija", FALSE),
          uiOutput("obrazec.agevzija")
        )
      } else if (input$stanje == "zd_delavec_na_dolznosti") {
        div(
          selectInput("ustanova", "Ustanova", c(1:10))
        )
      }
    })
  })
  
  
  
  #===========================================Lista zdravnikov==========================================================
  
  zdravniki <- dbGetQuery(conn, build_sql("SELECT ime FROM oseba WHERE stanje = 'zd_delavec_na_dolznosti'", con=conn))
  zdravniki <- zdravniki$ime
  
  #===========================================Generiram zdravnikov page=================================================
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        
        #===========================================Sidebar===================================================================    
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            div(fluidRow(
              box(width = 12, title = 'Število novih okužb z COVID-19', plotOutput("ggp_st_okuzb"))),
              fluidRow(
                box(width = 12, title = 'Skupno število potrjenih okužb z COVID-19', plotOutput("ggp_st_okuzb_skupaj"))),
              fluidRow(
                box(width = 12, title = 'Stolpični diagram zasedenosti bolnišnic', plotOutput("diagram_zasedenost")))
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
              box(width = 12, title = "Simptomi", dataTableOutput('results_k'))
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
                
                #==========================================Obrazec za dodajanje ljudi=========================================================
                
                #==========================================Obrazec za dodajanje ljudi=========================================================
                
                useShinyjs(),
                shinyjs::inlineCSS(appCSS),
                bsAlert("alert"),
                id = "form",
                textInput("ime", "Ime in priimek", ""),
                textInput("naslov", "Naslov"),
                textInput("davcna", "Davčna števiilka"),
                dateInput("dat", "Datum vpisa v evidenco", format = "dd-mm-yyyy"),
                selectInput("stanje", "Stanje", c("bolnik", "zd_delavec_na_dolznosti", "zdrav")),
                uiOutput("obrazec.pogojno"),
                useShinyalert(),
                actionButton("submit", "Dodaj v bazo", class = "btn-primary")
              )
              
              #           useShinyjs(),
              #           shinyjs::inlineCSS(appCSS),
              #           
              # 
              #           bsAlert("alert"),
              #           id = "form",
              #           textInput("ime", "Ime in priimek", ""),
              #           
              #           textInput("naslov", "Naslov"),
              #           
              #           textInput("davcna", "Davčna števiilka"),
              #           
              #           selectInput("zdravnik", "Zdravnik", zdravniki),
              #           
              #           dateInput("dat", "Datum vpisa v evidenco", format = "dd-mm-yyyy"),
              #           
              #           selectInput("stanje", "Stanje", c("bolnik", "zd_delavec_na_dolznosti", "zdrav")),
              #           
              #           checkboxInput("hospitalizacija", "Ali je potrebna hospitalizacija?", FALSE),
              #           
              #           checkboxInput("glavobol", "Glavobol", FALSE),
              #           numericInput("glavobol_j", "Jakost glavobola", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("glavobol_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("vrocina", "Vročina", FALSE),
              #           numericInput("vrocina_j", "Jakost vročine", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("vrocina_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("tekoce_blato", "Tekoče blato", FALSE),
              #           numericInput("tekoce_blato_j", "Jakost tekočega blata", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("tekoce_blato_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("sibkost", "šibkost", FALSE),
              #           numericInput("sibkost_j", "Jakost šibkosti", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("sibkost_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("mialgija", "Mialgija", FALSE),
              #           numericInput("mialgija_j", "Jakost mialgije", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("mialgija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("angina_pectoris", "Angina pectoris", FALSE),
              #           numericInput("angina_pectoris_j", "Jakost angine pectoris", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("angina_pectoris_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("dispneja", "Dispneja", FALSE),
              #           numericInput("dispneja_j", "Jakost dispneje", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("dispneja_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("anzomija", "Anzomija", FALSE),
              #           numericInput("anzomija_j", "Jakost anzomije", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("anzomija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           checkboxInput("agevzija", "Agevzija", FALSE),
              #           numericInput("agevzija_j", "Jakost agevzije", value = NULL, min = 1, max = 10, step =  1),
              #           dateInput("agevzija_dat", "Datum prve pojavitve zgornjega simptoma", format = "dd-mm-yyyy"),
              #           
              #           useShinyalert(),
              #           actionButton("submit", "Dodaj v bazo", class = "btn-primary")
              #           )
            )
          )
        )
      }
      else {
        
        #===========================================Generiram page za javnost======================================
        
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            div(fluidRow(
              box(width = 12, title = 'Število novih okužb z COVID-19', plotOutput("ggp_st_okuzb"))),
              fluidRow(
                box(width = 12, title = 'Skupno število potrjenih okužb z COVID-19', plotOutput("ggp_st_okuzb_skupaj"))),
              fluidRow(
                box(width = 12, title = 'Stolpični diagram zasedenosti bolnišnic', plotOutput("diagram_zasedenost")))
            )),
          tabItem(
            tabName ="Bolnisnice",
            fluidRow(
              box(width = 12, title = "Podatki o bolnisnicah", dataTableOutput('results_b'))
            )
          )
        )
      }
      
    }
    else {
      loginpage
    }
  })
  
  #===========================================Pridobim tabele iz baze=========================================
  
  output$results <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM oseba", con=conn))
  })
  output$results_b <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM lokacije", con = conn))
  })
  output$results_k <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM ima", con=conn))
  })
  output$results_h <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT 
                                  id_bolnika, ime_bolnika, id_zdravnika, ime_zdravnika
                                FROM bolnik 
                                  LEFT JOIN (SELECT ime AS ime_bolnika, davcna_st FROM oseba WHERE stanje = 'bolnik') as oseba1 ON bolnik.id_bolnika=oseba1.davcna_st
                                  LEFT JOIN (SELECT ime AS ime_zdravnika, davcna_st FROM oseba WHERE stanje = 'zd_delavec_na_dolznosti') as oseba2 ON bolnik.id_zdravnika=oseba2.davcna_st", con=conn))
  })
  
  #==========================================Polnenje DB======================================================
  observe({
    #===========================================Checkpoint obvezna polja========================================
    
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
    
    #============================================================================================================  
    
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
    
    if (input$stanje == "zd_delavec_na_dolznosti"){
      dbGetQuery(conn, build_sql("INSERT INTO 'zd_delavec_na_dolznosti (id, zd_ustanova_id_c)
                                         VALUES (", input$davcna, ",", input$ustanova, ")", con = conn))
    }
    
    shinyalert("OK!", "Oseba je dodana v sistem.", type = "success")
    
    
  })
  observeEvent(input$submit, {
    reset("body")
  })
  
}



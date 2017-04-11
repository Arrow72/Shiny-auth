library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

# ОПРЕДЕЛЯЕМ РАБОЧУЮ ФОРМУ ПРИЛОЖЕНИЯ #
mainbody <- div(
            tabItems(tabItem("ReportTab",class = "active",
                             box(title='График 1', footer='Lorem ipsum...',plotOutput("plot1"),solidHeader = TRUE, width=6,status = "info")
                             )
                     )
            )
# ОПРЕДЕЛЯЕМ ФОРМУ ЛОГИНА #
loginbody <-   tagList(
                  div(id = "logindiv",
                      wellPanel(
                        tags$img(src='https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/Skynet.svg/145px-Skynet.svg.png', 
                                  height='145px',  width = "145px", align = "center"),
                        tags$br(),tags$br(),                        
                        tags$b("Введите Ваш логин и пароль"),
                        tags$br(),tags$br(),
                        textInput("userName","Логин"),
                        passwordInput("passWord","Пароль"),
                        tags$br(),
                        actionButton("loginbutton","Войти")
                      )),
                  tags$style(HTML("#logindiv {font-size:12px; text-align:left; position:absolute; top:20%; left:50%; margin-top:-100px;margin-left:-150px;}"
                  )))



# body - используется не как всегда, а просто выводит отрендеренный в server() UI
body    <- dashboardBody(
  ###### Две строки ниже - реализация на js функции отключения заголовка ######
                        shinyjs::useShinyjs(), 
                        extendShinyjs(text = "shinyjs.hidehead = function(parm){$('header').css('display', parm);}"),
  ###### Тут выводим отрендеренный body
                        uiOutput("body1")
                        )

# Заголовок и боковая панель - отключаются на экране логина, поэтому можно определить сразу
header  <- dashboardHeader()
sidebar <- dashboardSidebar(sidebarMenu(menuItem("Отчет", tabName="ReportTab",icon=icon("dashboard"))),
                            tags$br(),tags$br(),tags$br(),
                            actionButton("PrintUser","Пользователь", width = 160),
                            tags$br(),tags$br(),
                            actionButton("logoutButton","Выйти", width = 160)
                            )

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  user <- reactiveValues(Logged = FALSE, usrName='') # определяем "объект" пользователь со статусом и именем
  

  observeEvent(input$logoutButton, # если нажат выход - сбрасываем "пользователя"
               {  
                 user$Logged <- FALSE
                 user$usrName <- ''
               })  
  
  observeEvent(input$loginbutton,{ # если нажат вход - сверяем пароль и заполняем "пользователя"
    if (user$Logged == FALSE){
          Username <- as.character(isolate(input$userName))
          Password <- as.character(isolate(input$passWord))
          
          if (Username==Password){
            user$Logged <- TRUE
            user$usrName <- Username
      }
    }
  })
  
  observeEvent(input$PrintUser,{ # Проверка имени пользователя под которым вошли
    showModal(modalDialog(
        title="Вы вошли под пользователем:",
        div(tags$h1(user$usrName, style = "color: red; size: 36px")),      
        easyClose = TRUE, size = 's',
        footer = NULL
    ))
  })  
  

  
  
observe({ # главный обработчик. Если пользователь вошел - показываем основное окно. Если пользователь не определен - показываем логин
  if(user$Logged != TRUE)     
  {
    js$hidehead('none')
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    output$body1 <- renderUI({loginbody})
  }
  if(user$Logged == TRUE)     
  {
    js$hidehead('')
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    output$plot1 <- renderPlot(ggplot()+geom_point(data=cars, aes(x=speed, y=dist, color=dist)))
    output$body1 <- renderUI({mainbody})    
  }
 
})


}

shinyApp(ui, server)

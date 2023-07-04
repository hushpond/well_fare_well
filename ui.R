library(shiny)
library(bslib)
library(shinydashboard)
library(dplyr)
library(highcharter)

# 데이터 프레임 예시 (수급자 데이터)
df <- data.frame(
  수급자 = c("김철수", "이영희", "박민수"),
  생년월일 = c("1990-01-01", "1985-02-03", "1978-06-10"),
  전화번호 = c("010-1234-5678", "010-9876-5432", "010-5555-5555"),
  홍삼 = c("2023-06-25", NA, "2023-06-25"),
  라면 = c(NA, "2023-06-25", NA),
  이불 = c(NA, "2023-06-25", NA)
)

# Shiny 앱 UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, theme = "lux"),
  navbarPage(
    "물품 수령 현황 대시보드",
    id = "dashboard",
    tabPanel("수급자별 물품 수령 현황", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("수급자", "수급자 선택", choices = df$수급자),
                 textInput("수급자검색", "수급자 검색", placeholder = "수급자 이름 검색")
               ),
               mainPanel(
                 uiOutput("물품수령현황")
               )
             )
    ),
    tabPanel("성품별 미수령자",
             sidebarLayout(
               sidebarPanel(
                 selectInput("물품", "물품 선택", choices = c("홍삼", "라면", "이불"))
               ),
               mainPanel(
                 tableOutput("미수령자")
               )
             )
    ),
    tabPanel("성품별 수령 현황",
             mainPanel(
               highchartOutput("수령현황")
             )
    )
  )
)

# Shiny 앱 서버
server <- function(input, output) {
  # 물품 수령 현황 출력
  output$물품수령현황 <- renderUI({
    수급자 <- input$수급자검색
    if (is.null(수급자) || 수급자 == "") {
      수급자 <- input$수급자
    }
    
    df_selected <- df[df$수급자 == 수급자, ]
    
    fluidRow(
      column(4,
             h5("홍삼: "),
             conditionalPanel(
               condition = !is.na(df_selected$홍삼),
               style = "color: white; padding: 10px;",
               tags$div(df_selected$홍삼, style = "background-color: green; padding: 10px;")
             ),
             conditionalPanel(
               condition = is.na(df_selected$홍삼),
               style = "color: white; padding: 10px;",
               tags$div("미수령", style = "background-color: red; padding: 10px;")
             )
      ),
      column(4,
             h5("라면: "),
             conditionalPanel(
               condition = !is.na(df_selected$라면),
               style = "color: white; padding: 10px;",
               tags$div(df_selected$라면, style = "background-color: green; padding: 10px;")
             ),
             conditionalPanel(
               condition = is.na(df_selected$라면),
               style = "color: white; padding: 10px;",
               tags$div("미수령", style = "background-color: red; padding: 10px;")
             )
      ),
      column(4,
             h5("이불: "),
             conditionalPanel(
               condition = !is.na(df_selected$이불),
               style = "color: white; padding: 10px;",
               tags$div(df_selected$이불, style = "background-color: green; padding: 10px;")
             ),
             conditionalPanel(
               condition = is.na(df_selected$이불),
               style = "color: white; padding: 10px;",
               tags$div("미수령", style = "background-color: red; padding: 10px;")
             )
      )
    )
  })
  
  # 미수령자 목록 출력
  output$미수령자 <- renderTable({
    물품 <- input$물품
    df_unreceived <- df[is.na(df[[물품]]), ]
    df_unreceived
  })
  
  # 수령 현황 그래프 출력
  output$수령현황 <- renderHighchart({
    성품별_수령현황 <- df %>%
      select(홍삼, 라면, 이불) %>%
      summarise(홍삼 = sum(!is.na(홍삼)),
                라면 = sum(!is.na(라면)),
                이불 = sum(!is.na(이불)))
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "성품별 수령 현황") %>%
      hc_xAxis(categories = c("홍삼", "라면", "이불")) %>%
      hc_yAxis(title = list(text = "수급자 수")) %>%
      hc_add_series(data = 성품별_수령현황) %>%
      hc_series(
        list(name = "수급자 수", data = c(df$홍삼, df$라면, df$이불))
      )
    hc
  })
}

# Shiny 앱 실행
shinyApp(ui, server)

data

library(shiny)
library(highcharter)

# 예시 데이터 생성
df <- data.frame(
  수급자 = c("김철수", "홍길동", "이영희", "박영수"),
  나이 = c(35, 45, 28, 52),
  홍삼 = c("2023-06-20", "", "2023-06-25", ""),
  라면 = c("", "2023-06-23", "2023-06-25", ""),
  이불 = c("2023-06-20", "", "", "2023-06-24")
)

# UI
ui <- fluidPage(
  titlePanel("물품 수령 현황 대시보드"),
  
  # 탭
  tabsetPanel(
    # 수급자별 물품 수령 현황 탭
    tabPanel("수급자별 물품 수령 현황",
             fluidRow(
               column(6, selectInput("수급자", "수급자 선택:", choices = unique(df$수급자))),
               column(6, textInput("수급자검색", "수급자 검색:"))
             ),
             uiOutput("수급자정보"),
             uiOutput("물품수령현황")
    ),
    
    # 성품별 미수령자 탭
    tabPanel("성품별 미수령자",
             fluidRow(
               column(6, selectInput("물품", "물품 선택:", choices = c("홍삼", "라면", "이불")))
             ),
             tableOutput("미수령자")
    ),
    
    # 성품별 수령 현황 탭
    tabPanel("성품별 수령 현황",
             highchartOutput("수령현황")
    )
  )
)

# 서버
server <- function(input, output) {
  
  # 수급자 정보 출력
  output$수급자정보 <- renderUI({
    수급자 <- input$수급자검색
    if (is.null(수급자) || 수급자 == "") {
      수급자 <- input$수급자
    }
    
    df_selected <- df[df$수급자 == 수급자, ]
    
    fluidRow(
      column(6, h5("이름: "), h5(df_selected$수급자)),
      column(6, h5("나이: "), h5(df_selected$나이))
    )
  })
  
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
               style = "color: white; padding: 10px",
               tags$div(df_selected$홍삼, style = "background-color: green")
             ),
             conditionalPanel(
               condition = is.na(df_selected$홍삼),
               style = "color: white; padding: 10px",
               tags$div("미수령", style = "background-color: red")
             )
      ),
      column(4,
             h5("라면: "),
             conditionalPanel(
               condition = !is.na(df_selected$라면),
               style = "color: white; padding: 10px",
               tags$div(df_selected$라면, style = "background-color: green")
             ),
             conditionalPanel(
               condition = is.na(df_selected$라면),
               style = "color: white; padding: 10px",
               tags$div("미수령", style = "background-color: red")
             )
      ),
      column(4,
             h5("이불: "),
             conditionalPanel(
               condition = !is.na(df_selected$이불),
               style = "color: white; padding: 10px",
               tags$div(df_selected$이불, style = "background-color: green")
             ),
             conditionalPanel(
               condition = is.na(df_selected$이불),
               style = "color: white; padding: 10px",
               tags$div("미수령", style = "background-color: red")
             )
      )
    )
  })
  
  # 성품별 미수령자 목록 출력
  output$미수령자 <- renderTable({
    물품 <- input$물품
    df_unreceived <- df[is.na(df[[물품]]), "수급자"]
    data.frame("미수령자" = df_unreceived)
  })
  
  # 성품별 수령 현황 막대그래프
  output$수령현황 <- renderHighchart({
    수급자별_수령여부 <- apply(df[, 3:5], 1, function(x) any(!is.na(x)))
    수급자별_수령여부 <- ifelse(수급자별_수령여부, "O", "")
    성품별_수령현황 <- table(수급자별_수령여부)
    
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = c("수령", "미수령")) %>%
      hc_yAxis(title = list(text = "수급자 수")) %>%
      hc_add_series(data = 성품별_수령현황) %>%
      hc_tooltip(shared = TRUE, valueSuffix = "명")
    
    hc
  })
}

# Shiny 앱 실행
shinyApp(ui, server)

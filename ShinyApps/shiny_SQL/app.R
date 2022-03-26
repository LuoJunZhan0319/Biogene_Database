#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RSQLite)
library(sqldf)
library(shinycssloaders)
library(magrittr)
options(spinner.size=0.5)
library(shinythemes)
library(plotly)
library(dplyr)

ui = fluidPage(
    #主題
    theme = shinytheme("darkly"),
    
    #application title
    titlePanel("Shiny-SQL Exercise"),
    
    #分割選擇區(小區)以及主頁面區(大區)
    sidebarLayout(
        
        #小區
        sidebarPanel(
            #第一個參數很重要用來後面server端使用，value就是設定一開始的預設值以免有人不知道怎麼下語法。
            textInput("sql", label = "Enter SQL", value = "select*from tcga_laml"),
            #設定查詢按鈕，如果沒有的話就會編輸入邊查詢(大部分時間會跳錯誤)
            actionButton("action", label = "Run")
        ),
        
        #大區
        mainPanel(
            #在大區中分開頁面
            tabsetPanel(
                #以表表示,後面是原先的輸出形式
                tabPanel("Table",tableOutput('table') %>% withSpinner(color = "green",type = 8)),
                #以圖表示
                tabPanel("Chart", plotlyOutput('plot'))
            )
            
            #output的結果用table變數設定、並且供於server端輸入資料
            #tableOutput('table') %>% withSpinner(color = "green",type = 8) -->往上移所以註解掉
        )
    )
    
)

server = function(input,output)
{
    #作互動查詢!
    data = eventReactive(input$action,
        {
            #連接資料庫
            con = dbConnect(RSQLite::SQLite(),"laml.db")
            #向數據庫發送查詢命令的函數，它會讓數據庫執行一個查詢，但本身並不能提取出查詢結果
            res = dbSendQuery(con, input$sql)
            #使用dbFetch()，選擇提取全部或者部分查詢結果
            result = dbFetch(res)
            #斷開連線
            dbDisconnect(con)
            #將結果回傳回data變數
            return(result)
            
        }
    )
    #如果不想用互動式的查詢也可以直接寫死renderDataTable(data)
    output$table = renderTable(data())
    
    #回傳作圖的資料
    output$plot = renderPlotly(
        data() %>% group_by(Chromosome) %>% summarize(count=n()) %>% plot_ly(x = ~Chromosome, y = ~count, type="bar")
    )
}


# Run the application 
shinyApp(ui = ui, server = server)


#建立、連接、資料庫
#con = dbConnect(RSQLite::SQLite(),"laml.db")
#dbWriteTable(con, "tcga_laml", mutations)
#dbDisconnect(con)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(maftools)
library(TCGAmutations)
library(DT)
library(shinycssloaders)

#建立一個dashboard
ui = dashboardPage(
    #標頭
    dashboardHeader(title = "Web Maftools"),
    #列表
    dashboardSidebar(
        #建立圖標列表
        sidebarMenu(
        #建立圖標對應的資料表
            menuItem("TCGA", tabName = "tcga", icon = icon("table")),
            menuItem("CoMutPlot", tabName = "comut", icon = icon("apple")),
            menuItem("CoMutOncoplots", tabName = "oncoplots", icon = icon("star"))
        
            )
    ),
    
    #主體
    dashboardBody(
        #將多個圖標各自對應到各自的資料主體
        tabItems(
            tabItem(tabName = "tcga",
                    DTOutput('tcga') %>% withSpinner()
                   ),

            tabItem(tabName = "comut",
                    valueBoxOutput("sample"),valueBoxOutput("gene"),
                    selectInput("tcgaid", label = "Select TCGA Project",
                            choices = tcga_available()$Study_Abbreviation,
                            selected = "ACC"),
                    plotOutput('plot1') %>% withSpinner(),
                    selectInput("gene",label = "Select Gene", choices = NULL),
                    plotOutput("lolliplot") %>% withSpinner()
                    ),
            
            tabItem(tabName = "oncoplots",
                    valueBoxOutput("sample2"),valueBoxOutput("gene2"),
                    selectInput("tcgaid2", label = "Select TCGA Project",
                                choices = tcga_available()$Study_Abbreviation,
                                selected = "ACC") ,
                    plotOutput('polt2') %>% withSpinner()
                   )
            )
    )
)

server = function(input, output, session)
{
    maf = reactive(
        {
            #tcga_load()查縮寫表
            #output = tcga_load("ACC")  -->這種寫法會限定ACC
            
            #互動查詢表
            output = tcga_load(input$tcgaid)
            
            #回傳maf物件
            return(output)
        }
    )
    
    maf2 = reactive(
        {
            #互動查詢表
            output = tcga_load(input$tcgaid2)
            
            #回傳maf物件
            return(output)
        }
    )
    
    observeEvent(maf(),{
      choices = maf()@gene.summary$Hugo_Symbol
      
      updateSelectInput(inputId = "gene", choices = choices)
    })
    
    
    
    #tcga所對應到的output資料
    output$tcga = renderDT(tcga_available())
    
    
    #plot1所對應到的output資料，renderPlot()圖形
    output$plot1 = renderPlot(
        #plotmafSummary()這個maf的全圖形繪製
        plotmafSummary(maf())
    )
    
    output$lolliplot = renderPlot(
      lollipopPlot(maf = maf(), gene = input$gene, showMutationRate = TRUE)
    )

    output$sample = renderValueBox({
        valueBox(
            as.numeric(maf()@summary[3,2]), "nSample", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$gene = renderValueBox({
        valueBox(
            as.numeric(maf()@summary[4,2]), "nGene", icon = icon("thumbs-up"),
            color = "yellow"        
        )
    })
    
    
    
    
    output$polt2 = renderPlot(
      
      oncoplot(maf2(), top = 10)
    )
    
    output$sample2 = renderValueBox({
        valueBox(
            as.numeric(maf2()@summary[3,2]), "nSample", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$gene2 = renderValueBox({
        valueBox(
            as.numeric(maf2()@summary[4,2]), "nGene", icon = icon("thumbs-up"),
            color = "yellow"        
        )
    })
    
    
    
}


# Run the application 
shinyApp(ui, server)

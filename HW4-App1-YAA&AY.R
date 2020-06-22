#### event reactive usage
library(shiny)
library(rhandsontable)
library(sp)
library(data.table)
library(tidyverse)
library(dplyr)

consumption_raw=read.csv("/Users/yagizarslan/Downloads/ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")

consumption_raw$Date= as.Date(consumption_raw$Date)
consumption_raw=consumption_raw[,-(4:10)]
str(consumption_raw)

df.consumption_raw= data.frame(consumption_raw)
str(consumption_raw)

ui <- fluidPage(
  titlePanel("Turkey Electricity Consumption Analysis for Selected 7 Cities"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "daterange",
                   label="Select Date",
                   start = min(consumption_raw$Date), 
                   end= as.Date('2017-01-01'),
                   min= min(consumption_raw$Date),
                   max= max(consumption_raw$Date),
                   format ="mm-dd-yyyy",
                   )),
    mainPanel(
      textOutput("startdate"),
      textOutput("enddate"),
      textOutput("range"),
      rHandsontableOutput("tablex")
      )
    )
  )

server =function(input, output ,session) {
  output$tablex <- renderRHandsontable({
    table_to_show <- subset(consumption_raw, consumption_raw$Date>= input$daterange[1] & consumption_raw$Date<= input$daterange[2])
    table_to_show = table_to_show[1:4]
    rhandsontable(table_to_show)
  })
}

shinyApp(ui = ui, server = server)

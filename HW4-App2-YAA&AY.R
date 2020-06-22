library(shiny)
library(data.table)
library(ArArRedux)

consumption_raw=read.csv("/Users/yagizarslan/Downloads/ETM58D_Spring20 HW 4_electricity_load_Turkey.csv")
consumption_raw$Date= as.Date(consumption_raw$Date)

df.consumption_raw= data.frame(consumption_raw)
##df.consumption_raw= df.consumption_raw[,-(4:11)]

hours=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)

ui <- fluidPage(
  headerPanel("Turkey Electricity Consumption Analysis for Selected 7 Cities"),
  
  mainPanel(
    # input section
    dateRangeInput(inputId = "daterange",
                   label="Please select the date range",
                   start = as.Date('2017-01-01'), 
                   end= as.Date('2017-01-01'),
                   min= as.Date('2017-01-01'),
                   max= as.Date('2019-12-29'),
                   format = "yyyy-mm-dd",
    ),
    plotOutput("hourlyconsumption"),
    plotOutput("hourlytemperature"),
  ))

server <- function(input, output) {
  
  output$hourlyconsumption <- renderPlot({
    edit.consumption=subset(df.consumption_raw, df.consumption_raw$Date>= input$daterange[1] & df.consumption_raw$Date<= input$daterange[2])
    target0_logic=edit.consumption$Hour == 0
    target0=edit.consumption[target0_logic,]
    mean0=mean(target0$Consumption)
    
    target1_logic=edit.consumption$Hour == 1
    target1=edit.consumption[target1_logic,]
    mean1=mean(target1$Consumption)
    
    target2_logic=edit.consumption$Hour == 2
    target2=edit.consumption[target2_logic,]
    mean2=mean(target2$Consumption)
    
    target3_logic=edit.consumption$Hour == 3
    target3=edit.consumption[target3_logic,]
    mean3=mean(target3$Consumption)
    
    target4_logic=edit.consumption$Hour == 4
    target4=edit.consumption[target4_logic,]
    mean4=mean(target4$Consumption)
    
    target5_logic=edit.consumption$Hour == 5
    target5=edit.consumption[target5_logic,]
    mean5=mean(target5$Consumption)
    
    target6_logic=edit.consumption$Hour == 6
    target6=edit.consumption[target6_logic,]
    mean6=mean(target6$Consumption)
    
    target7_logic=edit.consumption$Hour == 7
    target7=edit.consumption[target7_logic,]
    mean7=mean(target7$Consumption)
    
    target8_logic=edit.consumption$Hour == 8
    target8=edit.consumption[target8_logic,]
    mean8=mean(target8$Consumption)
    
    target9_logic=edit.consumption$Hour == 9
    target9=edit.consumption[target9_logic,]
    mean9=mean(target9$Consumption)
    
    target10_logic=edit.consumption$Hour == 10
    target10=edit.consumption[target10_logic,]
    mean10=mean(target10$Consumption)
    
    target11_logic=edit.consumption$Hour == 11
    target11=edit.consumption[target11_logic,]
    mean11=mean(target11$Consumption)
    
    target12_logic=edit.consumption$Hour == 12
    target12=edit.consumption[target12_logic,]
    mean12=mean(target12$Consumption)
    
    target13_logic=edit.consumption$Hour == 13
    target13=edit.consumption[target13_logic,]
    mean13=mean(target13$Consumption)
    
    target14_logic=edit.consumption$Hour == 14
    target14=edit.consumption[target14_logic,]
    mean14=mean(target14$Consumption)
    
    target15_logic=edit.consumption$Hour == 15
    target15=edit.consumption[target15_logic,]
    mean15=mean(target15$Consumption)
    
    target16_logic=edit.consumption$Hour == 16
    target16=edit.consumption[target16_logic,]
    mean16=mean(target16$Consumption)
    
    target17_logic=edit.consumption$Hour == 17
    target17=edit.consumption[target17_logic,]
    mean17=mean(target17$Consumption)
    
    target18_logic=edit.consumption$Hour == 18
    target18=edit.consumption[target18_logic,]
    mean18=mean(target18$Consumption)
    
    target19_logic=edit.consumption$Hour == 19
    target19=edit.consumption[target19_logic,]
    mean19=mean(target19$Consumption)
    
    target20_logic=edit.consumption$Hour == 20
    target20=edit.consumption[target20_logic,]
    mean20=mean(target20$Consumption)
    
    target21_logic=edit.consumption$Hour == 21
    target21=edit.consumption[target21_logic,]
    mean21=mean(target21$Consumption)
    
    target22_logic=edit.consumption$Hour == 22
    target22=edit.consumption[target22_logic,]
    mean22=mean(target22$Consumption)
    
    target23_logic=edit.consumption$Hour == 23
    target23=edit.consumption[target23_logic,]
    mean23=mean(target23$Consumption)
    
    all_means=c(mean0,mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9,mean10,mean11,mean12,
                mean13,mean14,mean15,mean16,mean17,mean18,mean19,mean20,mean21,mean22,mean23)
    
    hourly_average_consumption=as.list(all_means)
    plot(hours,hourly_average_consumption,type="o")
    })
  
  
  output$hourlytemperature <- renderPlot({
    edit.consumption=subset(df.consumption_raw, df.consumption_raw$Date>= input$daterange[1] & df.consumption_raw$Date<= input$daterange[2])
    TAV_target0_logic=edit.consumption$Hour == 0
    TAV_target0=edit.consumption[TAV_target0_logic,]
    TAV_mean0=mean(TAV_target0$T_AV)
    
    TAV_target1_logic=edit.consumption$Hour == 1
    TAV_target1=edit.consumption[TAV_target1_logic,]
    TAV_mean1=mean(TAV_target1$T_AV)
    
    TAV_target2_logic=edit.consumption$Hour == 2
    TAV_target2=edit.consumption[TAV_target2_logic,]
    TAV_mean2=mean(TAV_target2$T_AV)
    
    TAV_target3_logic=edit.consumption$Hour == 3
    TAV_target3=edit.consumption[TAV_target3_logic,]
    TAV_mean3=mean(TAV_target3$T_AV)
    
    TAV_target4_logic=edit.consumption$Hour == 4
    TAV_target4=edit.consumption[TAV_target4_logic,]
    TAV_mean4=mean(TAV_target4$T_AV)
    
    TAV_target5_logic=edit.consumption$Hour == 5
    TAV_target5=edit.consumption[TAV_target5_logic,]
    TAV_mean5=mean(TAV_target5$T_AV)
    
    TAV_target6_logic=edit.consumption$Hour == 6
    TAV_target6=edit.consumption[TAV_target6_logic,]
    TAV_mean6=mean(TAV_target6$T_AV)
    
    TAV_target7_logic=edit.consumption$Hour == 7
    TAV_target7=edit.consumption[TAV_target7_logic,]
    TAV_mean7=mean(TAV_target7$T_AV)
    
    TAV_target8_logic=edit.consumption$Hour == 8
    TAV_target8=edit.consumption[TAV_target8_logic,]
    TAV_mean8=mean(TAV_target8$T_AV)
    
    TAV_target9_logic=edit.consumption$Hour == 9
    TAV_target9=edit.consumption[TAV_target9_logic,]
    TAV_mean9=mean(TAV_target9$T_AV)
    
    TAV_target10_logic=edit.consumption$Hour == 10
    TAV_target10=edit.consumption[TAV_target10_logic,]
    TAV_mean10=mean(TAV_target10$T_AV)
    
    TAV_target11_logic=edit.consumption$Hour == 11
    TAV_target11=edit.consumption[TAV_target11_logic,]
    TAV_mean11=mean(TAV_target11$T_AV)
    
    TAV_target12_logic=edit.consumption$Hour == 12
    TAV_target12=edit.consumption[TAV_target12_logic,]
    TAV_mean12=mean(TAV_target12$T_AV)
    
    TAV_target13_logic=edit.consumption$Hour == 13
    TAV_target13=edit.consumption[TAV_target13_logic,]
    TAV_mean13=mean(TAV_target13$T_AV)
    
    TAV_target14_logic=edit.consumption$Hour == 14
    TAV_target14=edit.consumption[TAV_target14_logic,]
    TAV_mean14=mean(TAV_target14$T_AV)
    
    TAV_target15_logic=edit.consumption$Hour == 15
    TAV_target15=edit.consumption[TAV_target15_logic,]
    TAV_mean15=mean(TAV_target15$T_AV)
    
    TAV_target16_logic=edit.consumption$Hour == 16
    TAV_target16=edit.consumption[TAV_target16_logic,]
    TAV_mean16=mean(TAV_target16$T_AV)
    
    TAV_target17_logic=edit.consumption$Hour == 17
    TAV_target17=edit.consumption[TAV_target17_logic,]
    TAV_mean17=mean(TAV_target17$T_AV)
    
    TAV_target18_logic=edit.consumption$Hour == 18
    TAV_target18=edit.consumption[TAV_target18_logic,]
    TAV_mean18=mean(TAV_target18$T_AV)
    
    TAV_target19_logic=edit.consumption$Hour == 19
    TAV_target19=edit.consumption[TAV_target19_logic,]
    TAV_mean19=mean(TAV_target19$T_AV)
    
    TAV_target20_logic=edit.consumption$Hour == 20
    TAV_target20=edit.consumption[TAV_target20_logic,]
    TAV_mean20=mean(TAV_target20$T_AV)
    
    TAV_target21_logic=edit.consumption$Hour == 21
    TAV_target21=edit.consumption[TAV_target21_logic,]
    TAV_mean21=mean(TAV_target21$T_AV)
    
    TAV_target22_logic=edit.consumption$Hour == 22
    TAV_target22=edit.consumption[TAV_target22_logic,]
    TAV_mean22=mean(TAV_target22$T_AV)
    
    TAV_target23_logic=edit.consumption$Hour == 23
    TAV_target23=edit.consumption[TAV_target23_logic,]
    TAV_mean23=mean(TAV_target23$T_AV)
    
    TAV_all_means=c(TAV_mean0,TAV_mean1,TAV_mean2,TAV_mean3,TAV_mean4,TAV_mean5,TAV_mean6,
                    TAV_mean7,TAV_mean8,TAV_mean9,TAV_mean10,TAV_mean11,TAV_mean12,TAV_mean13,
                    TAV_mean14,TAV_mean15,TAV_mean16,TAV_mean17,TAV_mean18,TAV_mean19,TAV_mean20,
                    TAV_mean21,TAV_mean22,TAV_mean23)
    
    hourly_average_temperature=as.list(TAV_all_means)
    plot(hours,hourly_average_temperature,type="o")
    })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(plotly)
Income <- c(1:250000)

TaxThreshold1 <- 18200
TaxThreshold2 <- 45000
TaxThreshold3 <- 135000
TaxThreshold4 <- 190000

TaxRate1 <- 0.16
TaxRate2 <- 0.30
TaxRate3 <- 0.37
TaxRate4 <- 0.45

LITOThreshold1 <- 37500
LITOThreshold2 <- 45000
LITOThreshold3 <- 66666

MLThreshold1 <- 26000
MLThreshold2 <- 32500

MLSThreshold1 <- 97000
MLSThreshold2 <- 113000
MLSThreshold3 <- 151000

HECS <- tribble(
    ~Income, ~Percent,
    #--|--|
    54435, 0,
    62850, 0.01,
    66620, 0.02,
    70618, 0.025,
    74855, 0.03,
    79346, 0.035,
    84107, 0.04,
    89154, 0.045,
    94503, 0.05,
    100174, 0.055,
    106185, 0.06,
    112556, 0.065,
    119309, 0.07,
    126467, 0.075,
    134056, 0.08,
    142100, 0.085,
    150626, 0.09,
    159663, 0.095)
HECS$HECSPayable <- HECS$Income*HECS$Percent

TaxOffSetLow <- case_when(Income <= LITOThreshold1 ~ 700,
                          Income <= LITOThreshold2 ~ 700-0.05*(Income-LITOThreshold1),
                          Income <= LITOThreshold3 ~ 325-0.015*(Income-LITOThreshold2),
                          is.numeric(Income) ~ 0)

HECSPercent <- case_when(Income <= as.numeric(HECS[1,1]) ~ as.numeric(HECS[1,2]),
                  Income <= as.numeric(HECS[2,1]) ~ as.numeric(HECS[2,2]),
                  Income <= as.numeric(HECS[3,1]) ~ as.numeric(HECS[3,2]),
                  Income <= as.numeric(HECS[4,1]) ~ as.numeric(HECS[4,2]),
                  Income <= as.numeric(HECS[5,1]) ~ as.numeric(HECS[5,2]),
                  Income <= as.numeric(HECS[6,1]) ~ as.numeric(HECS[6,2]),
                  Income <= as.numeric(HECS[7,1]) ~ as.numeric(HECS[7,2]),
                  Income <= as.numeric(HECS[8,1]) ~ as.numeric(HECS[8,2]),
                  Income <= as.numeric(HECS[9,1]) ~ as.numeric(HECS[9,2]),
                  Income <= as.numeric(HECS[10,1]) ~ as.numeric(HECS[10,2]),
                  Income <= as.numeric(HECS[11,1]) ~ as.numeric(HECS[11,2]),
                  Income <= as.numeric(HECS[12,1]) ~ as.numeric(HECS[12,2]),
                  Income <= as.numeric(HECS[13,1]) ~ as.numeric(HECS[13,2]),
                  Income <= as.numeric(HECS[14,1]) ~ as.numeric(HECS[14,2]),
                  Income <= as.numeric(HECS[15,1]) ~ as.numeric(HECS[15,2]),
                  Income <= as.numeric(HECS[16,1]) ~ as.numeric(HECS[16,2]),
                  Income <= as.numeric(HECS[17,1]) ~ as.numeric(HECS[17,2]),
                  Income <= as.numeric(HECS[18,1]) ~ as.numeric(HECS[18,2]),
                  is.numeric(Income) ~ 0.10)
GrossTaxPayable <- case_when(Income <= TaxThreshold1 ~ 0,
                             Income <= TaxThreshold2 ~ (Income - TaxThreshold1)*TaxRate1,
                             Income <= TaxThreshold3 ~ (Income - TaxThreshold2)*TaxRate2 + 
                                 (TaxThreshold2 - TaxThreshold1)*TaxRate1,
                             is.numeric(Income)      ~ (Income - TaxThreshold3)*TaxRate3 +
                                 (TaxThreshold3 - TaxThreshold2)*TaxRate2 + 
                                 (TaxThreshold2 - TaxThreshold1)*TaxRate1)
MedicareLevy <- case_when(Income <= MLThreshold1 ~ 0,
                          Income <= MLThreshold2 ~ (Income - MLThreshold1)*0.1,
                          is.numeric(Income)     ~ Income*0.02)
MLSPercent <- case_when(Income <= MLSThreshold1 ~ 0,
                         Income <= MLSThreshold2 ~ 0.01,
                         Income <= MLSThreshold3 ~ 0.0125,
                         is.numeric(Income)     ~ 0.015)

df <- as.data.frame(cbind(Income, GrossTaxPayable, TaxOffSetLow))
df <- df %>% 
  mutate(TaxOffSetTotal = TaxOffSetLow ,
         NetTaxPayable =  case_when(GrossTaxPayable - TaxOffSetTotal < 0 ~ 0,
                           is.numeric(GrossTaxPayable) ~ GrossTaxPayable - TaxOffSetTotal),
         TaxPercent = round(NetTaxPayable/Income*100,2),
         LagNetTaxPayable = lag(NetTaxPayable),
         TaxPerDollar = NetTaxPayable - LagNetTaxPayable,
         MedicareLevy = MedicareLevy,
         MLSPercent = MLSPercent,
         MLSurcharge = MLSPercent*Income,
         MedicarePayable = NA,
         HECSPercent = HECSPercent,
         HECSPayable = NA
         )

## Define UI

ui <- fluidPage(

    # Application title
    titlePanel("Australian Tax Calculator"),
    
    fluidRow(
        column(4,
               numericInput("income", "Income ($):", 1, min = 1, max = 250000),
               radioButtons("privatehealth", 
                                  "Do you have private health insurance?", 
                                  choices = list("Yes" = 1, 
                                                 "No" = 2),
                                  selected = 2),
               radioButtons("partner", 
                                  "Partner Status:", 
                                  choices = list("Single" = 1, 
                                                 "Married/De Facto" = 2),
                                  selected = 1),
               numericInput("partnerincome", "Partner's Income ($):", 1, min = 1, max = 250000),
               numericInput("hecsdebt", "HECS Debt ($):", 0, min = 0, max = 150000),
               textOutput("nettax"),
               textOutput("mlsurcharge"),
               textOutput("medicarecontribution"),               
               textOutput("hecspercent"),
               textOutput("hecscontribution"),
               textOutput("totaltax"),
               textOutput("taxpercent"),
               textOutput("taxperdollar"),
               helpText("Note: These figures have not been verified and should not be solely used for tax purposes,
                        please read the ATO and Treasury sites for first-hand information that is up-to-date. ")),
        column(8,
               plotlyOutput("scatter1"),
               plotlyOutput("scatter2"),
               plotlyOutput("scatter3"))
                )
)

# Define server logic
server <- function(input, output) {
  income <- reactive({case_when(is.na(as.double(input$income)) ~ 0,
                                T ~ as.double(input$income))})
    dfreact <- reactive({
        df %>% 
        mutate(MedicarePayable = case_when(as.double(input$privatehealth) == 1 ~ MedicareLevy,
                                           as.double(input$privatehealth) == 2 & 
                                             as.double(input$partner) == 1 & 
                                             as.double(income()) > 90000 ~ MedicareLevy + MLSurcharge,
                                           as.double(input$privatehealth) == 2 & 
                                             as.double(input$partner) == 2 & 
                                             (as.double(income())+as.double(input$partnerincome)) > 180000 ~ MedicareLevy + MLSurcharge,
                                           is.numeric(Income) ~ MedicareLevy),
               MLSPercent = case_when(as.double(input$privatehealth) == 1 ~ 0,
                                      as.double(input$privatehealth) == 2 & 
                                        as.double(input$partner) == 1 & 
                                        as.double(income()) > 90000 ~ MLSPercent,
                                      as.double(input$privatehealth) == 2 & 
                                        as.double(input$partner) == 2 & 
                                        (as.double(income())+as.double(input$partnerincome)) > 180000 ~ MLSPercent,
                                      is.numeric(Income) ~ 0),
               TaxPlusMedicare = NetTaxPayable + MedicarePayable,
               HECSPayable = case_when(HECSPercent*Income > as.double(input$hecsdebt) ~ as.double(input$hecsdebt),
                                       is.numeric(Income) ~ HECSPercent*Income),           
               TaxPlusHECS = TaxPlusMedicare + HECSPayable,
               TaxPercentHECS = round(TaxPlusHECS/Income*100,2),
               TaxPerDollarHECS = lead(TaxPlusHECS)- TaxPlusHECS)
            })
    output$nettax <- renderText({
      paste0("Base Tax Payable: $",dfreact()[Income == as.numeric(income()),]$NetTaxPayable)})
    output$mlsurcharge <- renderText({
      paste0("Medicare Levy Surcharge: ",dfreact()[Income == as.numeric(income()),]$MLSPercent*100,"%")})
    output$medicarecontribution <- renderText({
      paste0("Medicare Contribution $",dfreact()[Income == as.numeric(income()),]$MedicarePayable)})
    output$hecspercent <- renderText({
        paste0("HECS Band: ",dfreact()[Income == income(),]$HECSPercent*100,"%")})
    output$hecscontribution <- renderText({
        paste0("HECS contribution: $",dfreact()[Income == income(),]$HECSPayable)})
    output$totaltax <- renderText({
        paste0("Total Tax: $",dfreact()[Income == income(),]$TaxPlusHECS)})
    output$taxpercent <- renderText({
        paste0("Tax as % of Income: ",dfreact()[Income == income(),]$TaxPercentHECS,"%")})
    output$taxperdollar <- renderText({
        paste0("Tax Per Extra $ Earned: $",round(dfreact()[Income == income(),]$TaxPerDollarHECS,digits = 3))})
    
    output$scatter1 <- renderPlotly({
            df2 <- dfreact() %>%
                mutate(Income = as.integer(Income),
                       TaxPlusHECS = as.integer(TaxPlusHECS),
                       NetTaxPayable = as.integer(NetTaxPayable)) %>%
                filter(Income %% 100 == 0 | Income %in% HECS$Income | df$Income %in% as.vector(HECS$Income+1))
          xlab <- list(
              title = "Income",
              showticklabels = TRUE)
          ylab <- list(
              title = "Tax Payable",
              showticklabels = TRUE)
          plot_ly(
          x = df2$Income,
          y = df2$TaxPlusHECS,
          type = 'scatter',
          mode = 'lines') %>% 
          layout(xaxis = xlab, yaxis = ylab, showlegend = FALSE)
        })
    output$scatter2 <- renderPlotly({
        df2 <- dfreact() %>% 
            filter(Income %% 100 == 0 | Income %in% HECS$Income | df$Income %in% as.vector(HECS$Income+1))
        xlab <- list(
          title = "Income",
          showticklabels = TRUE)
        ylab <- list(
          title = "Percent of Income Paid As Tax",
          showticklabels = TRUE)
        plot_ly(
          x = df2$Income,
          y = df2$TaxPercentHECS,
          type = 'scatter',
          mode = 'lines') %>% 
          layout(xaxis = xlab, yaxis = ylab, showlegend = FALSE)
    })
    output$scatter3 <- renderPlotly({
        df2 <- dfreact() %>% 
            filter(Income %% 10 == 0 | Income %in% HECS$Income | df$Income %in% as.vector(HECS$Income+1))
        xlab <- list(
          title = "Income",
          showticklabels = TRUE)
        ylab <- list(
          title = "Tax Per Extra Dollar Earned",
          showticklabels = TRUE)
        plot_ly(
          x = df2$Income,
          y = df2$TaxPerDollarHECS,
          type = 'scatter',
          mode = 'lines') %>% 
          layout(xaxis = xlab, yaxis = ylab, showlegend = FALSE)
        })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

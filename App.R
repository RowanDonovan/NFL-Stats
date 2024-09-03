# Important packages to Load----
library(shinythemes)
library(shiny)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(data.table)
library(randomForest)

# Read in Datasets----

playoffData <- read.csv('playoffData.csv')
regularData <- read.csv('regularData.csv')
thirdModel <- readRDS('thirdModel.RDS')

#User Interface----
ui <- fluidPage(theme = shinytheme('darkly'),

                navbarPage('NFL Statistics Hub',

                           ##ABOUT----
                           tabPanel('Home',
                                    titlePanel('About this App'),
                                    div(includeMarkdown('README.md'),
                                        align = 'justify')
                           ), #tab Panel About

                           ##TEXTBOX----
                           tabPanel('Cite 1',
                                    sidebarPanel(
                                        tags$label(h3('Input')),
                                        textInput('txt1', 'Anything', ''),
                                        textInput('txt2', 'Something Else', '')
                                    ), # sidebarPanel

                                    mainPanel(
                                        h1('Header 1'),

                                        h4('Output 1'),
                                        verbatimTextOutput('txtout')
                                    ) #main Panel
                           ), #Tab Panel Regression

                           ##REGULAR SEASON----
                           tabPanel('Regular Season Scatter Plots',
                                    sidebarPanel(
                                        tags$label(h3('Data to Compare:')),
                                        selectInput('rstat', label = 'Statistic:',
                                                    choices = list('Points per Game' = 'perRegGame',
                                                                   'Points Allowed per Game' = 'avgAllowed',
                                                                   'Average Timeouts at Two Minute Warning' = 'avgTimeouts',
                                                                   'Third Down Conversion Rate' = 'conversionRate',
                                                                   'Division Win Percentage' = 'percentageDiv'))
                                    ), #Side Bar Panel

                                    mainPanel(
                                        tags$label(h3('Status/Output')),
                                        plotOutput('dataplot2')

                                    ) #Main Panel
                           ), #Tab Panel Regular Season

                           ##PLAYOFFS----
                           tabPanel('Playoff Scatter Plots',
                                    sidebarPanel(
                                        tags$label(h3('Data to compare:')),
                                        selectInput('stat', label = 'Statistic:',
                                                    choices = list('Third Down Conversion Rate' = 'conversionRate',
                                                                   'Average Timeouts at Two Minute Warning' = 'avgTimeouts',
                                                                   'Red Zone Efficiency' = 'efficiency',
                                                                   'Percent of Fourth Downs Attempted' = 'fourthAttemptedPercentage',
                                                                   'Points Allowed per Game' = 'avgAllowed')),
                                        actionButton('submitbutton', 'Submit',
                                                     class = 'btn btn-primary')
                                    ), # Side Bar Panel

                                    mainPanel(
                                        tags$label(h3('Status/Output')),
                                        verbatimTextOutput('contents'),
                                        plotOutput('dataplot')

                                    ) # Main Panel
                           ), # Tab Panel

                           ##PREDICTION MODEL----
                           tabPanel('Third Down Prediction Model',

                                    ###side panel----
                                    sidebarPanel(
                                        tags$label(h3('Input Parameters')),

                                        selectInput('Q', label = 'Quarter:',
                                                    choices = list('First' = '1',
                                                                   'Second' = '2',
                                                                   'Third' = '3',
                                                                   'Fourth' = '4'),
                                                    selected = 'First'),

                                        sliderInput('Min', label = 'Minutes Remaining:',
                                                    min = 0, max = 14,
                                                    value = 7),

                                        sliderInput('Sec', label = 'Seconds Remaining:',
                                                    min = 0, max = 59,
                                                    value = 30),

                                        sliderInput('Dist', label = 'Distance to First Down:',
                                                    min = 1, max = 50,
                                                    value = 30),

                                        sliderInput('YrdLin', label = 'Yardline:',
                                                    min = 0, max = 99,
                                                    value = 50),

                                        sliderInput('Rush', label = 'Current Rushing Yards:',
                                                    min = 0, max = 300,
                                                    value = 120),

                                        sliderInput('Pas', label = 'Current Passing Yards:',
                                                    min = 0, max = 550,
                                                    value = 200),

                                        sliderInput('Pts', label = 'Points Scored:',
                                                    min = 0, max = 80,
                                                    value = 30),

                                        selectInput('FDR', label = 'First Down Reached?',
                                                    choices = list('Yes!' = 'y',
                                                                   'No :(' = 'n'),
                                                    selected = 'Yes!'),

                                        actionButton('gyat', 'Submit!',
                                                     class = 'btn btn-primary')

                                    ),#Side Bar Panel

                                    ###main panel----
                                    mainPanel(
                                        tags$label(h3('Predicted Play Type')), #Status/Output Textbox
                                        verbatimTextOutput('megumi'),
                                        tableOutput('inumaki') #Prediction Results Table
                                    ) #Main Panel
                ) #Tab Panel
)#nav Bar Page
)# fluid Page

#Define Server Function----
server <- function(input, output, session) {

    output$contents <- renderPrint({
        if (input$submitbutton > 0) {
            isolate('Calculation Complete!')
        } else {
            return('Server is awaiting input.')
        }
    }) #Contents

    output$txtout <- renderText({
        paste(input$txt1, input$txt2, sep = ' ')
    }) # render Text

    datasetInput <- reactive({

        if(input$submitbutton > 0) {

            if (input$stat == 'conversionRate') {

                df <- data.frame(x = playoffData$conversionRate,
                                 y = playoffData$perGame,
                                 team = playoffData$team)
                print(df)
                df

            } else if (input$stat == 'avgTimeouts') {

                df <- data.frame(x = playoffData$avgTimeouts,
                                 y = playoffData$perGame,
                                 team = playoffData$team)
                print(df)
                df

            } else if (input$stat == 'efficiency'){

                df <- data.frame(x = playoffData$efficiency,
                                 y = playoffData$perGame,
                                 team = playoffData$team)
                print(df)
                df

            } else if (input$stat == 'fourth Attempted percentage') {

                df <- data.frame(x = playoffData$fourthAttemptedPercentage,
                                 y = playoffData$perGame,
                                 team = playoffData$team)
                print(df)
                df

            } else {

                df <- data.frame(x = playoffData$avgAllowed,
                                 y = playoffData$perGame,
                                 team = playoffData$team)
                print(df)
                df
            }
        } #Submit

        print(df)
        df

    }) # Data Set Input

    datasetInputReg <- reactive({
        if (input$rstat == 'perRegGame') {

            dfr <- data.frame(x = regularData$perRegGame,
                              y = regularData$percentage,
                              team = regularData$team,
                              qPerc = regularData$aboveAvgPerc,
                              qPerReg = regularData$aboveAvgPerReg)

            dfr <- dfr |> mutate(quadrant = case_when(qPerc == 1 & qPerReg == 1 ~ 'Q1',
                                                      qPerc == 0 & qPerReg == 1 ~ 'Q2',
                                                      qPerc == 1 & qPerReg == 0 ~ 'Q3',
                                                      qPerc == 0 & qPerReg == 0 ~ 'Q4')) |>
                select(x, y, team, quadrant)

        } else if (input$rstat == 'avgAllowed') {

            dfr <- data.frame(x = regularData$avgAllowed,
                              y = regularData$percentage,
                              team = regularData$team,
                              qPerc = regularData$aboveAvgPerc,
                              qAllowed = regularData$aboveAvgAllowed)

            dfr <- dfr |> mutate(quadrant = case_when(qPerc == 1 & qAllowed == 1 ~ 'Q1',
                                                      qPerc == 0 & qAllowed == 1 ~ 'Q2',
                                                      qPerc == 1 & qAllowed == 0 ~ 'Q3',
                                                      qPerc == 0 & qAllowed == 0 ~ 'Q4')) |>
                select(x, y, team, quadrant)

        } else if (input$rstat == 'avgTimeouts') {

            dfr <- data.frame(x = regularData$avgTimeouts,
                              y = regularData$percentage,
                              team = regularData$team,
                              qPerc = regularData$aboveAvgPerc,
                              qTimeouts = regularData$aboveAvgTimeouts)

            dfr <- dfr |> mutate(quadrant = case_when(qPerc == 1 & qTimeouts == 1 ~ 'Q1',
                                                      qPerc == 0 & qTimeouts == 1 ~ 'Q2',
                                                      qPerc == 1 & qTimeouts == 0 ~ 'Q3',
                                                      qPerc == 0 & qTimeouts == 0 ~ 'Q4')) |>
                select(x, y, team, quadrant)

        } else if (input$rstat == 'conversionRate') {

            dfr <- data.frame(x = regularData$conversionRate,
                              y = regularData$percentage,
                              team = regularData$team,
                              qPerc = regularData$aboveAvgPerc,
                              qConv = regularData$aboveAvgConv)

            dfr <- dfr |> mutate(quadrant = case_when(qPerc == 1 & qConv == 1 ~ 'Q1',
                                                      qPerc == 0 & qConv == 1 ~ 'Q2',
                                                      qPerc == 1 & qConv == 0 ~ 'Q3',
                                                      qPerc == 0 & qConv == 0 ~ 'Q4')) |>
                select(x, y, team, quadrant)

        } else {

            dfr <- data.frame(x = regularData$percentageDiv,
                              y = regularData$percentage,
                              team = regularData$team,
                              qPerc = regularData$aboveAvgPerc,
                              qDiv = regularData$aboveAvgDiv)

            dfr <- dfr |> mutate(quadrant = case_when(qPerc == 1 & qDiv == 1 ~ 'Q1',
                                                      qPerc == 0 & qDiv == 1 ~ 'Q2',
                                                      qPerc == 1 & qDiv == 0 ~ 'Q3',
                                                      qPerc == 0 & qDiv == 0 ~ 'Q4')) |>
                select(x, y, team, quadrant)

        }
    }) #DatasetInputReg

    output$dataplot <- renderPlot({

        ggplot(data = datasetInput(), aes(x = x,
                                          y = y,
                                          label = team)) +
            geom_point(color = 'deeppink',
                       size = 4) +
            geom_text(hjust = -0.5, vjust = -0.5) +
            labs(title = 'Playoff Points Per Game vs Selected Statistic',
                 x = 'Selected Statistic',
                 y = 'Points Per Game')
    }) #Render Plot

    output$dataplot2 <- renderPlot({

        ggplot(data = datasetInputReg(), aes(x = x,
                                             y = y,
                                             label = team,
                                             color = quadrant)) +
            geom_point(size = 3) +
            geom_text_repel() +
            labs(title = 'Regular Season Win Percentage vs Selected Statistic',
                 x = 'Selected Statistic',
                 y = 'Win Percentage')
    }) #dataplot2

    ##Prediction Model----

    gojo <- reactive({

        quarter <- as.numeric(input$Q)
        minutesLeft <- as.numeric(input$Min)
        secondsLeft <- as.numeric(input$Sec)

        totalQuarterSecs <- minutesLeft*60 + secondsLeft

        totalSecondsRemaining <- case_when(
            quarter == 1 ~ totalQuarterSecs + 2700,
            quarter == 2 ~ totalQuarterSecs + 1800,
            quarter == 3 ~ totalQuarterSecs + 900,
            quarter == 4 ~ totalQuarterSecs,
        )

        df <- data.frame(
            name = c('game_seconds_remaining',
                     'ydstogo',
                     'posteam_score',
                     'first_down',
                     'runningRush',
                     'runningPass',
                     'yardline_100'),
            value = as.character(c(totalSecondsRemaining,
                                   input$Dist,
                                   input$Pts,
                                   input$FDR,
                                   input$Rush,
                                   input$Pas,
                                   input$YrdLin)),
            stringsAsFactors = FALSE)

        play_type <- 0
        df <- rbind(df, play_type)
        input <- transpose(df)
        write.table(input, 'input.csv', sep = ',', quote = FALSE, row.names = FALSE, col.names = FALSE)

        test <- read.csv(paste('input', '.csv', sep = ''), header = TRUE)

        Output <- data.frame(Prediction = predict(thirdModel, test),
                             round(predict(thirdModel, test, type = 'prob'), 3))

        print(Output)
    })

    #Status Textbox
    output$megumi <- renderPrint({
        if (input$gyat >0) {
            isolate('Calculation Complete!')
        } else {
            return('Server is ready for calculation.')
        }
    }) #render Print

    #Prediction Results Table for UI
    output$inumaki <- renderTable({
        if (input$gyat > 0) {
            isolate(gojo())
        }
    })
} # Server function

#Create Shiny App----
shinyApp(ui = ui, server = server)

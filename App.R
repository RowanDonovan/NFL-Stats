# Important packages to Load
library(shinythemes)
library(shiny)

# Read in Datasets

playoffData <- read.csv('playoffData.csv')


ui <- fluidPage(theme = shinytheme('darkly'),

                navbarPage('NFL Statistics Hub',

                           tabPanel('Home',
                                    titlePanel('About this App'),
                                    div(includeMarkdown('README.md'),
                                        align = 'justify')
                           ), #tab Panel About

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

                           tabPanel('Playoff Scatter Plots',
                                    sidebarPanel(
                                        tags$label(h3('Data to compare:')),
                                        selectInput('stat', label = 'Statistic:',
                                                    choices = list('Third Down Conversion Rate' = 'conversionRate',
                                                                   'Average Timeouts at Two Minute Warning' = 'avgTimeouts')),
                                        actionButton('submitbutton', 'Submit',
                                                     class = 'btn btn-primary')
                                    ), # Side Bar Panel

                                    mainPanel(
                                        tags$label(h3('Status/Output')),
                                        verbatimTextOutput('contents'),
                                        plotOutput('dataplot')

                                    ) # Main Panel
                           ) # Tab Panel
                ) #nav Bar Page
) # fluid Page

#Define Server Function
server <- function(input, output) {

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

    output$dataplot <- renderPlot({

        if(input$submitbutton > 0) {

            if (input$stat == 'conversionRate') {

                x <- playoffData$conversionRate
                y <- playoffData$perGame
                plot(x,y)

            } else {

                x <- playoffData$avgTimeouts
                y <- playoffData$perGame
                plot(x,y)

            }
        } #Submit
    }) #Render Plot
} # Server function

#Create Shiny App
shinyApp(ui = ui, server = server)

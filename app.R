library(dplyr)
library(ggplot2)

statevar = "state"
agevar='age_group'
datevar = "date"
outcome='all_cause'
#yaxis=statevar
#facet=agevar

df_oe <- readRDS('obs_exp_heat.rds')

dates <- as.Date(unique(df_oe[[datevar]]))
states <- unique(df_oe[[statevar]])
age_groups <- unique(df_oe[[agevar]])
last.date <- max(dates)
last.date.format <- format(last.date, "%b %d, %Y")

ui <- fluidPage(shiny::titlePanel(paste0("Data through ", 
                                         last.date.format)), shiny::sidebarLayout(shiny::sidebarPanel(shiny::selectInput(input = "set.states", 
                                                                                                                         label = "State:", choice = states, selected = c("CA","NY"), 
                                                                                                                         multiple = T), shiny::selectInput(input = "set.ages", 
                                                                                                                                                           label = "Age group:", choice = age_groups, selected = c("0-25 years","25-44 years","45-64 years","65-74 years","75-84 years","85 years and older"), multiple = T), 
                                                                                                      shiny::sliderInput(input = "display.dates", label = "Earliest date to display", 
                                                                                                                         min = min(dates), max = dates[length(dates) - 2], 
                                                                                                                         step = 7, value = dates[length(dates) - round(length(dates)/5)]), 
                                                                                                      shiny::selectInput(input='y.display', label='Group by', choices=list('State','Age'), selected='Age' )),
                                                                                shiny::mainPanel(shiny::plotOutput("plot"),
                                                                                                 shiny::span("These heatmaps show the Rate Ratio (Observed deaths due to any cause/Expected deaths) in each week. Darker colors indicate a larger increase compared to expected. The expected deaths are calculated using a model similar to the one described by Weinberger et al JAMA Internal Medicine 2020. The data are from the CDC/NCHS https://data.cdc.gov/api/views/y5bj-9g5w Note that 'NY' refers to New York state, excluding New York City"), 
                                                                                                 shiny::hr(), shiny::span("This app was developed jointly between The Public Health Modeling Unit at Yale School of Public Health (based on ExcessILI app by Marcus Russi, Dan Weinberger) and Aledade Inc (Amy Graves, Farzad Mostashari). Underlying analysis code can be found at https://github.com/DanWeinberger/CDC_test/")
                                                                                                 )))                                                                        
                
server <- function(input, output) {
  dates_states_ages_sel <- reactive({
    req(input$display.dates, input$set.states, input$set.ages)
    df_oe %>% filter(get(datevar) >= input$display.dates & 
                       get(statevar) %in% c(input$set.states) & get(agevar) %in% 
                       c(input$set.ages))
  })
  output$plot = renderPlot({
    if(input$y.display=='State'){
      yaxis<-statevar
      facet<- agevar
    }else{
      yaxis<-agevar
      facet<- statevar
      }
    ggplot(data = dates_states_ages_sel(), aes(x = factor(get(datevar)), 
                                               y = get(yaxis))) + geom_raster(aes(fill = oe_fac_rev), 
                                                                              interpolate = F) + scale_fill_manual(values = c(`>10.0` = "#5c0900", 
                                                                                                                              `10.0` = "#850d00", `8.0` = "#a31000", 
                                                                                                                              `6.0` = "#c21300", `4.0` = "#eb1800", 
                                                                                                                              `3.5` = "#ff3e29", `3.0` = "#ff7014", 
                                                                                                                              `2.5` = "#ff9049", `2.0` = "#ffaf35", 
                                                                                                                              `1.75` = "#ffd230", `1.5` = "#a3a3cc", 
                                                                                                                              `1.25` = "#b7b7d7", `1.0` = "#cacae2", 
                                                                                                                              `0.5` = "#dbdbeb")) + xlab("Time") + 
      labs(fill = "O/E Ratio") + scale_x_discrete(expand = c(0, 
                                                             0)) + scale_y_discrete(expand = c(0, 0)) + facet_grid(get(facet) ~ 
                                                                                                                     .) + theme_bw() + theme(axis.title.y = element_blank(), 
                                                                                                                                             axis.text.x = element_text(size = 7, vjust = 1, 
                                                                                                                                                                        hjust = 0, angle = 90))
  })
}
shiny::shinyApp(ui, server)

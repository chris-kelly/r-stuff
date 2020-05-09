## LIBRARIES

library(shiny)
library(plotly)

## FUNCTIONS

run_test <- function(true_dist_1, sample_size_1, true_dist_2, sample_size_2, type_1_error = 0.05, alternative = "two sided") {
  
  # Take samples from true distribution
  sample_dist_1 = sample(true_dist_1, sample_size_1)
  sample_dist_2 = sample(true_dist_2, sample_size_2) 
  
  # summary stats (1)
  sample_n_1 = length(sample_dist_1)
  sample_mean_dist_1 = sum(sample_dist_1)/sample_n_1
  sample_sd_dist_1 = sqrt(sum((sample_dist_1 - sample_mean_dist_1)^2)/sample_n_1)
  sample_se_dist_1 = sample_sd_dist_1/sqrt(sample_n_1)
  
  # summary stats (2)
  sample_n_2 = length(sample_dist_2)
  sample_mean_dist_2 = sum(sample_dist_2)/sample_n_2
  sample_sd_dist_2 = sqrt(sum((sample_dist_2 - sample_mean_dist_2)^2)/sample_n_2)
  sample_se_dist_2 = sample_sd_dist_2/sqrt(sample_n_2)
  
  # Check if detected
  Z = (sample_mean_dist_2 - sample_mean_dist_1)/sqrt(sample_se_dist_1^2+sample_se_dist_2^2)
  p_value = pnorm(Z)
  
  if(alternative == "two sided") {
    stat_sig = p_value < type_1_error/2 | p_value > 1-(type_1_error/2)
  } else if(alternative == "less") {
    stat_sig = p_value < type_1_error
  } else if(alternative == "greater") {
    stat_sig = p_value > type_1_error
  }
  
  return(c(
    #   sample_dist_1 = sample_dist_1
    # , sample_dist_2 = sample_dist_2
    sample_mean_dist_1 = sample_mean_dist_1 
    , sample_mean_dist_2 = sample_mean_dist_2
    , sample_se_dist_1 = sample_se_dist_1
    , sample_se_dist_2 = sample_se_dist_2
    , Z = Z
    , p_value = p_value
    , stat_sig = stat_sig
  ))
  
}

render_plotly_dist <- function(true_dist_1, true_dist_2, results) {
  
  p1 <- plot_ly(alpha = 0.3) %>%
    add_histogram(x = ~true_dist_1, name = 'True control distribution') %>%
    add_histogram(x = ~true_dist_2, name = 'True variant distribution') %>% 
    layout(barmode = "overlay", xaxis = list(title=''), title = 'True distributions', legend = list(orientation = 'h'))
  
  p2 <- plot_ly(alpha = 0.3) %>%
    add_histogram(x = ~results['sample_mean_dist_1',], name = 'Sample mean of control distribution') %>%
    add_histogram(x = ~results['sample_mean_dist_2',], name = 'Sample mean variant distribution') %>%
    layout(barmode = "overlay", xaxis = list(title=''), title = 'Distribution of sample means', legend = list(orientation = 'h'))
  
  p3 <- plot_ly(alpha = 0.3, color = ~as.logical(results['stat_sig',]), colors = c("green", "red")) %>%
    add_histogram(x = ~results['Z',], name = 'Normalized difference in sample means') %>%
    # add_histogram(x = ~results['Z',][as.logical(results['stat_sig',])], name = 'Stat sig') %>%
    layout(xaxis = list(title=''), title = 'Distribution of normalized difference in sample means',legend = list(orientation = 'h'))
  
  return(list(p1=p1,p2=p2,p3=p3))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Power Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textAreaInput('true_dist_1'
                    , 'True control Distribution'
                    , value = 'rbinom(n=100000,size=10,prob=0.2)')
      , textAreaInput('true_dist_2'
                      , 'True variant Distribution'
                      , value = 'rbinom(n=100000,size=10,prob=0.2)')
      , selectInput('alternative'
                    , 'Alternative hypothesis'
                    , choices = c("two sided", "less", "greater")
                    , selected = "two sided")
      , textInput('type_1_error'
                  , 'Type 1 error (e.g. 0.05)'
                  , value = '0.05')
      , textInput('sample_size_1'
                  , 'Size of sample from control'
                  , value = '1000')
      , textInput('sample_size_2'
                  , 'Size of sample from variant'
                  , value = '2000')
      , textInput('num_samples_to_simulate'
                  , 'Number of samples to simulate'
                  , value = '10000')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      HTML(markdown::markdownToHTML(text = 
                                      "It's a bit slow to run the first time, be patient...
                   <ul>
                        <li>The default inputs give an example to show type 1 error (no true difference in means).</li>
                        <li>Try `rbinom(n=100000,size=10,prob=0.21)` to observe type 2 error.</li>
                        <li>Vary other parameters (other distibutions, sample sizes, accepted type 1 error etc.) to see how it impacts power.</li>
                   </ul>"
      ))
      , plotlyOutput('true_dist')
      , br()
      , HTML(markdown::markdownToHTML(text = 
              '**Analysing the variant and control distributions:** <br>
               Two factors from the control and variant distributions impact whether an effect is detected: 
                   <ul>
                        <li>The difference in the means of the two distributions ($X_1$ and $X_2$)</li>
                        <li>The variability (standard deviation) of the two distributions ($\\sigma_1$ and $\\sigma_2$)</li>
                   </ul>
               The greater the difference in means, the more likely the effect is detectable.<br>
               The smaller the random variability in the two distributions, the more confident we can be that an observed difference is true.'
      ))
      , DT::DTOutput('true_dist_summary')
      , br()
      , br()
      , HTML(markdown::markdownToHTML(text = 
                                        "Lets say we take many samples (as per the 'frequentist' paradigm) and take the mean each time:"
      ))
      , br()
      , br()
      , plotlyOutput('sample_mean_dist')
      , br()
      , HTML(markdown::markdownToHTML(text = 
                                        "Note two things (as per the central limit theorem):
                <ul>
                    <li>The sample mean distributions have the same averages as the means of the true distributions (as per the central limit theorem).<br></li>
                    <li>The sample mean distributions follow a normal distribution (even though the distributions they are sampling from might be non-normal)<br></li>
                </ul>
                This allows us to exploit what we know about the normal distribution to determine if there is a difference between test and control.<br><br>
                We now note one further factor during sampling impacts whether an effect is detected:
                   <ul>
                        <li>The size of the samples ($n_1$ and $n_2$) </li>
                   </ul>
                The greater the sample size, the smaller the estimated standard error (the standard deviation of the sampling means). The lower this is, the more confident we can be that our sample means reflect the true distributions - and this is reflected in observing a more narrow the distribution of the sampling means.<br><br>
                The estimated standard error from one sample $i$ is given by the standard deviation of the sample divided by the square root of the sample size (i.e. $SE_i=\\frac{\\sigma_i}{\\sqrt{n_i}}$)
                "
      ))
      , DT::DTOutput('sample_mean_summary')
      , br()
      , br()
      , HTML(markdown::markdownToHTML(text = 
                                        "Finally, we compare how frequently the difference in sample means appears signficant. We need to compare how  different the sample means were, while taking into account the random error involved in sampling.<br>
              To do this, we create a Z statistic that follows a normal distribution with mean of 0 and standard deviation of 1: <br>
              $$
              Z=\\frac{X_1-X_2}{\\sqrt{SE_1^2+SE_2^2}}\\sim N(0,1)
              $$
              If we compute the Z statistic for every simulated sample, then we would derive the plot below:
              "
      ))
      , br()
      , plotlyOutput('Z_test')
      , br()
      , HTML(markdown::markdownToHTML(text = 
                                        "If there is no difference in the means of the control and variant distributions, we expect the proportion of results that were statistically significant to be equal to the defined type 1 error, $\\alpha$ (the false positive rate). <br><br>
             However, if there is a difference in the means of the two distributions, then the proportion of false negatives from our simulated samples is defined as the type 2 error, $\\beta$. <br>
             The power is defined as the likelihood of detecting an effect as statistically significant given there is a genuine difference (hence is $1-\\beta$). As a rule of thumb, experiments aim for the power to be 80%.
             "
      ))
      , DT::DTOutput('Z_test_summary')
      , HTML(markdown::markdownToHTML(text = 
                                        "In summary, there are three things that increase the power of a test:
             <ul>
                <li>There being a greater difference in mean betewen the true distributions from the control and variant</li>
                <li>Choosing metrics that are less variable (their true distributions having lower standard deviations)</li>
                <li>Taking a larger sample size (lowers the estimated random sampling error, given by the standard error)</li>
             </ul>
             "
      ))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  results <- reactive({
    true_dist_1 = eval(parse(text = input$true_dist_1)) # control
    true_dist_2 = eval(parse(text = input$true_dist_2)) # variant
    results <- sapply(1:as.numeric(input$num_samples_to_simulate)
                      , FUN = function(x) {
                        run_test(true_dist_1 = true_dist_1 
                                 , sample_size_1 = as.numeric(input$sample_size_1)
                                 , true_dist_2 = true_dist_2
                                 , sample_size_2 = as.numeric(input$sample_size_2)
                                 , type_1_error = as.numeric(input$type_1_error)
                                 , alternative = input$alternative)
                      })
    return(results)
  })
  
  plotly_graphs <- reactive({
    if (is.null(results())) return(NULL)
    graphs <- render_plotly_dist(true_dist_1 = eval(parse(text = input$true_dist_1))
                                 , true_dist_2 = eval(parse(text = input$true_dist_2))
                                 , results = results())
    return(graphs)
  })
  
  output$true_dist_summary <- DT::renderDT({
    if (is.null(results())) return(NULL)
    return(DT::datatable(data = data.frame(`Distribution mean` = c(mean(eval(parse(text = input$true_dist_1)))
                                                                   , mean(eval(parse(text = input$true_dist_2))))
                                           , `Distribution SD` = c(sd(eval(parse(text = input$true_dist_1)))
                                                                   ,sd(eval(parse(text = input$true_dist_2))))
                                           , row.names = c('Control', 'Variant')
    )
    , rownames = TRUE, options = list(dom = 't'))
    )
  })
  
  output$sample_mean_summary <- DT::renderDT({
    if (is.null(results())) return(NULL)
    return(DT::datatable(data = data.frame(`Sample average mean` = c(mean(results()['sample_mean_dist_1',])
                                                                     ,mean(results()['sample_mean_dist_2',]))
                                           , `True standard error` = c(sd(results()['sample_mean_dist_1',])
                                                                       ,sd(results()['sample_mean_dist_2',]))
                                           , `Average estimated standard error` = c(mean(results()['sample_se_dist_1',])
                                                                                    ,mean(results()['sample_se_dist_2',]))
                                           , row.names = c('Control', 'Variant')
    )
    , rownames = TRUE, options = list(dom = 't'))
    )
  })
  
  output$Z_test_summary <- DT::renderDT({
    if (is.null(results())) return(NULL)
    return(DT::datatable(data = data.frame(`Proportion stat-sig` = sum(results()['stat_sig',])/as.numeric(input$num_samples_to_simulate)
                                           , `Proportion not stat-sig` = sum(!results()['stat_sig',])/as.numeric(input$num_samples_to_simulate)
    )
    , rownames = TRUE, options = list(dom = 't')))
  })
  
  output$true_dist <- renderPlotly({
    if (is.null(plotly_graphs())) return(NULL)
    return(plotly_graphs()$p1)
  })
  
  output$sample_mean_dist <- renderPlotly({
    if (is.null(plotly_graphs())) return(NULL)
    return(plotly_graphs()$p2)
  })
  
  output$Z_test <- renderPlotly({
    if (is.null(plotly_graphs())) return(NULL)
    return(plotly_graphs()$p3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

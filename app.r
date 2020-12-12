library(shiny)
library(shinydashboard)
library(tidyverse)
library(gridExtra)

data = read_csv("https://raw.githubusercontent.com/jordanisaacs/house_census_data/main/house_census_data.csv",
         col_types=cols("n","c","n","f","n","n","n","n","n","n","n","n","n"))

voteshare = function(state_filter = "PA", year_filter = 2012) {
  temp_data <- data %>% filter(state == state_filter, year == year_filter, party != "other")
  order <- temp_data %>% filter(party == "republican") %>%
    arrange(desc(votes)) %>% select(district) %>% `[[`(1)
  temp_data$district = factor(temp_data$district,
                              levels = order)
  plot1 <- temp_data %>%
    ggplot(., aes(x = district, y = votes, fill = party)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    ggtitle("Proportion of Votes for Parties by District")

  
  return (plot1)
}

voteshare_proportions = function(state_filter = "PA", year_filter = 2012) {
  temp_data <- data %>% filter(state == state_filter, year == year_filter, party != "other")
  
  total_votes = sum(temp_data$votes)
  proportions = temp_data %>% group_by(party) %>%
    summarise(proportion = sum(votes) / total_votes)
  proportions$type = "votes"
  
  district_num = max(temp_data$district)
  dem_won = pivot_wider(temp_data, id_cols = "district", values_from = "votes", names_from = "party") %>%
    mutate(won = democrat > republican) %>% select(won) %>% `[[`(1)

  dem_won = sum(dem_won) / district_num
  repub_won = 1 - dem_won
  temp_data <- rbind(proportions, tibble(party = c("democrat", "republican"), proportion = c(dem_won, repub_won), type = "seats"))

  plot1 <- temp_data %>%
    ggplot(., aes(x = type, y = proportion, fill = party)) +
    geom_bar(stat = "identity", position = "stack")

  return (plot1)
}

population = function(state_filter = "PA", year_filter = 2012) {
  temp_data <- data %>% filter(state == state_filter, year == year_filter)
  order <- temp_data %>% filter(party == "republican") %>%
    arrange(desc(votes)) %>% select(district) %>% `[[`(1)
  temp_data$district = factor(temp_data$district,
                              levels = order)
  
  plot1 <- ggplot(temp_data, aes(x = district, y = population)) +
    geom_bar(stat = "identity")
  
  return (plot1)
}

seats_timeseries = function(state_filter = "PA") {
  years = seq(2012, 2020, by = 2)
  temp_data <- data %>% filter(state == state_filter, year %in% years)
  district_num = max(temp_data$district)
  seats = temp_data %>%
    pivot_wider(values_from = "votes", names_from = "party") %>%
    mutate(won = democrat > republican) %>% group_by(year) %>%
    summarise(democrat = sum(won) / district_num) %>%
    mutate(republican = 1 - democrat) %>% pivot_longer(!year,
                                                        names_to = "party",
                                                        values_to = "seats")
  tot_votes = temp_data %>% group_by(year) %>% summarise(votes = sum(votes))
  
  democrat_votes = temp_data %>% filter(party == "democrat") %>% group_by(year) %>%
    summarise(votes = sum(votes)) %>% mutate(votes = votes / tot_votes$votes)
  
  seats$party <- factor(seats$party,
                        levels = c("republican", "democrat"))
  
  plot1 <- ggplot() +
    geom_area(position = "stack", data = seats, mapping = aes(x = year, y = seats, fill = party)) +
    geom_line(data = democrat_votes, mapping = aes(x = year, y = votes, lty = "Democratic Votes"), color = "black") +
    labs(y = "proportion",
         linetype = "",
         title = "Voteshares over time")
  return (plot1)
}

income_ts = function(state_filter = "PA") {
  years = seq(2012, 2018, by = 2)
  temp_data <- data %>% filter(state == state_filter, year %in% years)
  income = temp_data %>%
    pivot_wider(values_from = "votes", names_from = "party") %>%
    mutate(party = ifelse(democrat > republican, "democrat", "republican")) %>% group_by(year, party) %>%
    summarise(income = mean(median_income_12mo))
  income$party <- factor(income$party,
                        levels = c("republican", "democrat"))

  plot1 <- ggplot(income, aes(x = year, y = income, color = party)) +
    geom_line() +
    geom_point() +
    labs(title = "Mean median income of districts represented by party over time")+
    expand_limits(y=0)
  return (plot1);
}

age_ts = function(state_filter = "PA") {
  years = seq(2012, 2018, by = 2)
  temp_data <- data %>% filter(state == state_filter, year %in% years)
  income = temp_data %>%
    pivot_wider(values_from = "votes", names_from = "party") %>%
    mutate(party = ifelse(democrat > republican, "democrat", "republican")) %>% group_by(year, party) %>%
    summarise(age = mean(median_age))
  income$party <- factor(income$party,
                         levels = c("republican", "democrat"))
  plot1 <- ggplot(income, aes(x = year, y = age, color = party)) +
    geom_line() +
    geom_point() +
    labs(title = "Mean median age of districts represented by party over time") +
    expand_limits(y=0)
  return (plot1);
}

black_ts = function(state_filter = "PA") {
  years = seq(2012, 2018, by = 2)
  temp_data <- data %>% filter(state == state_filter, year %in% years)
  income = temp_data %>%
    pivot_wider(values_from = "votes", names_from = "party") %>%
    mutate(party = ifelse(democrat > republican, "democrat", "republican")) %>% group_by(year, party) %>%
    summarise(black_pop = mean(black_pop))
  income$party <- factor(income$party,
                         levels = c("republican", "democrat"))
  plot1 <- ggplot(income, aes(x = year, y = black_pop, color = party)) +
    geom_line() +
    geom_point() +
    labs(title = "Mean total of black (only or multiracial) population of districts represented by party over time")+
    expand_limits(y=0)
  return (plot1);
}

white_ts = function(state_filter = "PA") {
  years = seq(2012, 2018, by = 2)
  temp_data <- data %>% filter(state == state_filter, year %in% years)
  income = temp_data %>%
    pivot_wider(values_from = "votes", names_from = "party") %>%
    mutate(party = ifelse(democrat > republican, "democrat", "republican")) %>% group_by(year, party) %>%
    summarise(white_pop = mean(white_pop))
  income$party <- factor(income$party,
                         levels = c("republican", "democrat"))
  plot1 <- ggplot(income, aes(x = year, y = white_pop, color = party)) +
    geom_line() +
    geom_point() +
    labs(title = "Mean total of white (only or multiracial) population of districts represented by party over time")+
    expand_limits(y=0)
  return (plot1);
}

get_voteshare <- function(party, votes, party_filter, years) {
  party_vote = votes[party == party_filter]
  total_votes = sum(votes)
  return (party_vote / total_votes)
}

population_change <- function(state_filter, party_filter, start, end) {
  temp_data <- data %>% filter(state == state_filter, year %in% c(start, end))

  temp_data %>% group_by(district, year) %>%
    arrange(year) %>%
    summarise(vote_share = get_voteshare(party, votes, party_filter),
              population = mean(population)) %>%
    group_by(district) %>%
    arrange(year) %>%
    summarise(perc_vote_change = (vote_share[2] - vote_share[1]) / vote_share[1],
              perc_pop_change = (population[2] - population[1]) / population[1]) %>%
    ggplot(., aes(x = perc_pop_change, y = perc_vote_change)) +
    geom_abline(slope=1, intercept=0, lty="dashed") +
    geom_point() +
    ggtitle(paste("", party_filter, "% vote share change vs. ",
                  "% population change in ", state_filter,
                  " from ", start, " to ", end, sep = ""))
}

ui <- dashboardPage(
  dashboardHeader(title = "House Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Voteshare", tabName = "voteshare"),
      menuItem("Time Series", tabName="ts"))),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "voteshare",
              bootstrapPage(
                selectInput(inputId = "year",
                            label = "Choose a year",
                            choices = seq(2012, 2020,by=2),
                            selected = 2012),
                
                selectInput(inputId = "state_vs",
                              label = strong("Choose a state"),
                              choices = state.abb,
                              selected = "PA"),
                
                plotOutput(outputId = "voteshare", height = "400px"),
                plotOutput(outputId = "populations", height = "400px"),
                plotOutput(outputId = "proportions", height = "300px")
                )
              ),
      tabItem(tabName = "ts",
              bootstrapPage(
                selectInput(inputId = "state_ts",
                            label = strong("Choose a state"),
                            choices = state.abb,
                            selected = "PA"),
                "Total Districts: ",
                textOutput("district_num"),
                
                plotOutput(outputId = "vote_ts", height = "400px"),
                plotOutput(outputId = "age_ts", height = "400px"),
                plotOutput(outputId = "income_ts", height = "400px"),
                plotOutput(outputId = "black_ts", height = "400px"),
                plotOutput(outputId = "white_ts", height = "400px"),
                
                selectInput(inputId = "party_ts",
                            label = strong("Choose a party"),
                            choices = c("democrat", "republican"),
                            selected = "democrat"),
                plotOutput(outputId = "pop_change", height = "400px")
                )
              )
      )
    )
  )

server <- function(input, output) {
  output$voteshare <- renderPlot({
    plot = voteshare(input$state_vs, input$year)
    plot
  })
  
  output$proportions <- renderPlot({
    plot = voteshare_proportions(input$state_vs, input$year)
    plot
  })
  
  output$populations <- renderPlot({
    plot = population(input$state_vs, input$year)
    plot
  })
  
  
  output$vote_ts <- renderPlot({
    plot = seats_timeseries(input$state_ts)
    plot
  })
  
  output$income_ts <- renderPlot({
    plot = income_ts(input$state_ts)
    plot
  })
  
  output$age_ts <- renderPlot({
    plot = age_ts(input$state_ts)
    plot
  })
  
  output$black_ts <- renderPlot({
    plot = black_ts(input$state_ts)
    plot
  })
  
  output$white_ts <- renderPlot({
    plot = white_ts(input$state_ts)
    plot
  })
  
  output$district_num <- renderText({
    df = data %>% filter(state == input$state_ts, year == 2012)
    num = max(1, max(df$district))
    num
  })
  
  output$pop_change <- renderPlot({
    plot = population_change(input$state_ts, input$party_ts, 2012, 2018)
    plot
  })
}


shinyApp(ui, server)

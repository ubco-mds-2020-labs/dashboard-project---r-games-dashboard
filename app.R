library(ggplot2)
library(plotly)
library(dplyr)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(reshape2)
library(tidyverse)

#Create App
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

#Read in data/wrangle
game <- readr::read_csv(here::here('data', 'vgsales.csv'))
game_melt <- melt(data=game,id.vars = c("Rank","Name","Platform","Year","Genre","Publisher"),measure.vars=c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"))
game_melt$Year <- as.integer(game_melt$Year)
colnames(game_melt)[7] <- "Region"
colnames(game_melt)[8] <- "Copies Sold"
#game_melt <- tidyr::gather(game, key = "Region", value = "Sales", NA_Sales, EU_Sales, Global_Sales, JP_Sales, Other_Sales)
#genre_sales <- aggregate(Global_Sales ~ Genre, game, sum)
#sorted_genre_totalsales <- genre_sales[order(-genre_sales$Global_Sales),]$Genre

#Data wrangling
#sales_data <- game_melt[!(game_melt$Region=="Global_Sales"),]
#sales_data_platform <- aggregate(Sales ~ Platform+Year+Genre+Region, game_melt, sum)
#sales_data_publisher <- aggregate(Sales ~ Publisher+Year+Genre+Region, game_melt, sum)

#Nested Lists for Filters
platform_filter <- unique(game$Platform) %>%
    purrr::map(function(col) list(label = col, value = col))
platform_filter <- append(platform_filter,list(list(label="All",value="all")))

genre_filter <- unique(game$Genre) %>%
    purrr::map(function(col) list(label = col, value = col))
genre_filter <- append(genre_filter,list(list(label="All",value="all")))

publisher_filter <- unique(game$Publisher) %>%
    purrr::map(function(col) list(label = col, value = col))
publisher_filter <- append(publisher_filter,list(list(label="All",value="all")))

app$layout(htmlDiv(list(
    dccTabs(id="tabs", children=list(
        dccTab(label='Number of copies released', children=list(
            htmlDiv(list(
                htmlH1('Dashr heroky deployment'),
                htmlLabel("Plot 1: Copies Sold vs Time"),
                dccGraph(id='plot-area'),
                htmlBr(),
                htmlLabel("Plot 2: Number of Games Released vs Time"),
                dccGraph(id='plot-area2'),
                htmlBr(),
                htmlLabel("Select your region of interest:"),
                dccDropdown(
                    id='region_selector',
                    options = list(list(label="North America",value="NA_Sales"),
                                   list(label="Europe",value="EU_Sales"),
                                   list(label="Japan",value="JP_Sales"),
                                   list(label="Other",value="Other_Sales"),
                                   list(label="Global", value = "Global_Sales")),
                    value='Global_Sales',
                    multi=TRUE),
                htmlBr(),
                htmlLabel("Select your Platform of interest:"),
                dccDropdown(
                    id='platform_selector',
                    options = platform_filter,
                    value="all",
                    multi=TRUE),
                htmlBr(),
                htmlLabel("Select your Genre of interest:"),
                dccDropdown(
                    id='genre_selector',
                    options = genre_filter,
                    value="all",
                    multi=TRUE),
                htmlBr(),
                htmlLabel("Select your Publisher of interest:"),
                dccDropdown(
                    id='publisher_selector',
                    options = publisher_filter,
                    value="all",
                    multi=TRUE),
                htmlBr(),
                htmlLabel('Slider'),
                dccRangeSlider(
                    id = "year_selector",
                    min = 1980,
                    max = 2017,
                    marks = list("1980" = "1980",
                                 "1985" = "1985",
                                 "1990" = "1990",
                                 "1995" = "1995",
                                 "2000" = "2000",
                                 "2005" = "2005",
                                 "2010" = "2010",
                                 "2015" = "2015"),
                    value = list(1980,2017))
            ))
        )),
        dccTab(label='Number of copies sold', children=list(
            dccGraph(
                id='example-graph-1',
                figure=list(
                    'data'= list(
                        list('x'= c(1, 2, 3), 'y'= c(1, 4, 1),
                             'type'= 'bar', 'name'= 'SF'),
                        list('x'= c(1, 2, 3), 'y'= c(1, 2, 3),
                             'type'= 'bar', 'name'= 'Montréal')
                    )
                )
            )
        )),
        dccTab(label='Top Game titles, Platforms and Publishers across Genres', children=list(
            dccGraph(
                id='example-graph-2',
                figure=list(
                    'data'= list(
                        list('x'= c(1, 2, 3), 'y'= c(1, 4, 1),
                             'type'= 'bar', 'name'= 'SF'),
                        list('x'= c(1, 2, 3), 'y'= c(1, 2, 3),
                             'type'= 'bar', 'name'= 'Montréal')
                    )
                )
            )
        ))
    ))
)))

#Callback for Plot1
app$callback(
    output('plot-area', 'figure'),
    list(input('region_selector', 'value'),
         input('platform_selector', 'value'),
         input('genre_selector', 'value'),
         input('publisher_selector', 'value'),
         input('year_selector', 'value')),
    function(reg,plat,gen,pub,years) {
        # Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
        # Output: Graph
        #
        # Create subset based on filters 
        # Pass to graph
        # Output graph
        if ("Global_Sales" %in% reg){
            filter_region = list("Global_Sales")
        } else {
            filter_region = reg
        }
        if ("all" %in% plat){
            filter_plat = unique(game_melt$Platform)
        } else {
            filter_plat = plat
        }
        if ("all" %in% gen){
            filter_gen = unique(game_melt$Genre)
        } else {
            filter_gen = gen
        }
        if ("all" %in% pub){
            filter_pub = unique(game_melt$Publisher)
        } else {
            filter_pub = pub
        }
        min_year = years[1]
        max_year = years[2]
        
        graph1 <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Year,Genre) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Copies Sold`,
                fill = Genre) + 
            geom_bar(stat="identity")+
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) +
            ylab("Number of Copies Sold (in millions)")+
            xlab("Year")
        
        return (ggplotly(graph1))
    }
)

#Callback for Plot2
app$callback(
    output('plot-area2', 'figure'),
    list(input('region_selector', 'value'),
         input('platform_selector', 'value'),
         input('genre_selector', 'value'),
         input('publisher_selector', 'value'),
         input('year_selector', 'value')),
    function(reg,plat,gen,pub,years) {
        # Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
        # Output: Graph
        #
        # Create subset based on filters 
        # Pass to graph
        # Output graph
        if ("Global_Sales" %in% reg){
            filter_region = list("Global_Sales")
        } else {
            filter_region = reg
        }
        if ("all" %in% plat){
            filter_plat = unique(game_melt$Platform)
        } else {
            filter_plat = plat
        }
        if ("all" %in% gen){
            filter_gen = unique(game_melt$Genre)
        } else {
            filter_gen = gen
        }
        if ("all" %in% pub){
            filter_pub = unique(game_melt$Publisher)
        } else {
            filter_pub = pub
        }
        min_year = years[1]
        max_year = years[2]
        
        graph2 <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Year,Genre) %>%
            count(Year,Genre) %>%
            rename(`Number of Releases`="n") %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Number of Releases`,
                fill = Genre) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            ylab("Number of Games Released")+
            xlab("Year")
        
        return (ggplotly(graph2))
    }
)

app$run_server(host = '127.0.0.1')

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)

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
sales_data <- game_melt[!(game_melt$Region=="Global_Sales"),]
#sales_data_platform <- aggregate(Sales ~ Platform+Year+Genre+Region, game_melt, sum)
#sales_data_publisher <- aggregate(Sales ~ Publisher+Year+Genre+Region, game_melt, sum)
top_game_init <- game_melt %>% #Initialize Top Game Card (Tab3)
    group_by(Name) %>%
    summarise("Copies Sold" = sum(`Copies Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_genre_init <- game_melt %>% #Initialize Top Genre Card (Tab3)
    group_by(Genre) %>%
    summarise("Copies Sold" = sum(`Copies Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_platform_init <- game_melt %>% #Initialize Top Platform Card (Tab3)
    group_by(Platform) %>%
    summarise("Copies Sold" = sum(`Copies Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_publisher_init <- game_melt %>% #Initialize Top Publisher Card (Tab3)
    group_by(Publisher) %>%
    summarise("Copies Sold" = sum(`Copies Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

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
                htmlH1('Game Dashboard R deployment'),
                htmlBr(),
                dbcButton("Reset Filters",
                          id="reset_button"),
                htmlBr(),
                htmlLabel("Plot 1: Copies Sold vs Time"),
                dccGraph(id='plot-area'),
                htmlBr(),
                htmlLabel("Plot 2: Number of Games Released vs Time"),
                dccGraph(id='plot-area2'),
                htmlBr(),
                htmlLabel("Plot 3: Number of Platforms, Genres and Publishers with games selling over 100,000 copies."),
                dccGraph(id='plot-area3'),
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
            htmlDiv(list(
                # htmlH1('AAMIR DATA'),
                htmlLabel("Plot 4: Copies Sold vs Genre"),
                dccGraph(id='plot-area4'),
                htmlBr(),
                htmlLabel("Select your region of interest:"),
                dccDropdown(
                    id='region_selector2',
                    options = list(list(label="North America",value="NA_Sales"),
                                   list(label="Europe",value="EU_Sales"),
                                   list(label="Japan",value="JP_Sales"),
                                   list(label="Other",value="Other_Sales"),
                                   list(label="Global", value = "Global_Sales")),
                    value='Global_Sales',
                    multi=FALSE)
            ))
        )),
        dccTab(label='Top Game titles, Platforms and Publishers across Genres', children=list(
            htmlDiv(list(
                dbcContainer(list(
                    dbcRow(list(
                        dbcCol(list(
                            dbcCard(list(
                                dbcCardHeader("Game with most copies sold:"),
                                dbcCardBody(list(
                                    htmlH5(id="top_game",
                                           top_game_init$Name),
                                    htmlP(id="top_game_sales",
                                          sprintf("%.2f million copies sold",top_game_init$`Copies Sold`))
                                ))
                                ),
                                color="primary",
                                inverse=TRUE
                            ),
                            dbcCard(list(
                                dbcCardHeader("Genre with Most Copies Sold"),
                                dbcCardBody(list(
                                    htmlH5(id="top_genre",
                                           top_genre_init$Genre),
                                    htmlP(id="top_genre_sales",
                                          sprintf("%.2f million copies sold",top_genre_init$`Copies Sold`))
                                ))
                            ),
                                color="secondary",
                                inverse=TRUE
                            ),
                            dbcCard(list(
                                dbcCardHeader("Platform with Most Copies Sold"),
                                dbcCardBody(list(
                                    htmlH5(id="top_platform",
                                           top_platform_init$Platform),
                                    htmlP(id="top_platform_sales",
                                          sprintf("%.2f million copies sold",top_platform_init$`Copies Sold`))
                                ))
                            ),
                                color="info",
                                inverse=TRUE
                            ),
                            dbcCard(list(
                                dbcCardHeader("Publisher with Most Copies Sold"),
                                dbcCardBody(list(
                                    htmlH5(id="top_publisher",
                                           top_publisher_init$Publisher),
                                    htmlP(id="top_publisher_sales",
                                          sprintf("%.2f million copies sold",top_publisher_init$`Copies Sold`))
                                ))
                            ),
                                color="success",
                                inverse=TRUE
                            )
                        ),width=3),
                        dbcCol(list(
                            dbcCard(
                                dbcCardBody(list(
                                    htmlH1("Top Game is:"),
                                    htmlP("Answer"),
                                    htmlP("# of Sales")
                                ))
                            )    
                        ))
                    ))
                ))
            ))
        ))
    ))
)))

#Callback for Button
app$callback(
    list(output('region_selector', 'value'),
         output('platform_selector', 'value'),
         output('genre_selector', 'value'),
         output('publisher_selector', 'value'),
         output('year_selector', 'value')),
    list(input('reset_button','n_clicks')),
    function(n_clicks){
        #Input: if button is clicked
        #Output: Default values for all filters
        #
        #If clicked - return default values to filters
        return (list("Global_Sales","all","all","all",list(1980,2017)))
    }
)


#Callback for Cards (Tab 3)
app$callback(
    list(output('top_game', 'children'),
         output("top_game_sales","children"),
         output('top_genre','children'),
         output('top_genre_sales','children'),
         output('top_platform','children'),
         output('top_platform_sales','children'),
         output('top_publisher','children'),
         output('top_publisher_sales','children')),
    list(input('region_selector', 'value'),
         input('platform_selector', 'value'),
         input('genre_selector', 'value'),
         input('publisher_selector', 'value'),
         input('year_selector', 'value')),
    function(reg,plat,gen,pub,years) {
        #Input: List of Regions, Platforms, Genres, Publishers, Min and Max Year
        #Output: Name & Sales of Top Game
        #
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
        
        top_game <- game_melt %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Name) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_genre <- game_melt %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Genre) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_publisher <- game_melt %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Publisher) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_platform <- game_melt %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year) %>%
            group_by(Platform) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        return (list(top_game$Name,sprintf("%.2f million copies sold",top_game$`Copies Sold`),
                     top_genre$Genre,sprintf("%.2f million copies sold",top_genre$`Copies Sold`),
                     top_platform$Platform,sprintf("%.2f million copies sold",top_platform$`Copies Sold`),
                     top_publisher$Publisher,sprintf("%.2f million copies sold",top_publisher$`Copies Sold`)))
    }
)

#Callback for all plots
app$callback(
    list(output('plot-area', 'figure'),
         output('plot-area2', 'figure'),
         output('plot-area3', 'figure')),
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
        
        filtered_game_melt <- game_melt[,3:8] %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year)
        
        graph1 <-  filtered_game_melt %>%
            group_by(Year,Genre) %>%
            summarise("Copies Sold" = sum(`Copies Sold`)) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Copies Sold`,
                fill = Genre,
                text = paste("Year: ",as.factor(Year),
                             "<br>Copies Sold: ",`Copies Sold`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity")+
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) +
            ylab("Number of Copies Sold (in millions)")+
            xlab("Year")
        graph1 <- ggplotly(graph1,tooltip="text")
        
        graph2 <- filtered_game_melt %>%
            group_by(Year,Genre) %>%
            count(Year,Genre) %>%
            rename(`Number of Releases`="n") %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Number of Releases`,
                fill = Genre,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Games Released: ",`Number of Releases`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            ylab("Number of Games Released")+
            xlab("Year")
        graph2<-ggplotly(graph2,tooltip="text")
        
        graph3 <- filtered_game_melt %>%
            group_by(Year)%>%
            melt(id.vars=c("Year"),measure.vars=c("Genre","Platform","Publisher")) %>%
            rename(Category='variable') %>% 
            group_by(Year,Category) %>%
            unique() %>%
            count(Year,Category) %>%
            rename(`Counts of Genres, Publishers and Platforms`= n) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Counts of Genres, Publishers and Platforms`,
                fill = Category,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Succesful Gen, Publ, Plat: ",`Counts of Genres, Publishers and Platforms`,
                             "<br>Category: ", Category)) + 
            geom_bar(stat="identity")+
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            ylab("Counts of Sucessful Genres, Publishers and Platforms")+
            xlab("Year")
        graph3<-ggplotly(graph3,tooltip="text")
        
        return(list(graph1,graph2,graph3))
    }
)

#Callback for Plot4
app$callback(
    output('plot-area4', 'figure'),
    list(input('region_selector2', 'value')),
    function(reg) {
        # Input: List of Regions
        # Output: Graph
        #
        if ("Global_Sales" %in% reg){
            region_filter = list("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
        } else {
            region_filter = reg
        }
        graph3 <- sales_data[,3:8] %>% 
            subset(Region %in% region_filter) %>%
            group_by(Genre) %>%
            summarize(genre_sales = sum(`Copies Sold`)) %>%
            ggplot() +
            aes(x=reorder(Genre,-genre_sales),
                y=genre_sales) + 
            geom_bar(stat="identity", fill='darkblue') +
            theme(axis.text.x = element_text(angle=45, hjust=0.9, vjust=0.9))+
            ylab("Number of Copies Sold (in millions)") +
            xlab("Genre")
        
        return (ggplotly(graph3))
    }
)

app$run_server(host = '127.0.0.1')
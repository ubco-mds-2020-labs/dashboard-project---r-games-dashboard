library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(data.table)

#Create App
app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

#Read in data/wrangle
#game <- readr::read_csv(here::here('data', 'vgsales.csv'))

game <- read_csv('data/vgsales.csv')
game_melt <- melt(data=game,id.vars = c("Rank","Name","Platform","Year","Genre","Publisher"),measure.vars=c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales"))
game_melt$Year <- as.integer(game_melt$Year)
colnames(game_melt)[7] <- "Region"
colnames(game_melt)[8] <- "Copies_Sold"

#game_melt <- tidyr::gather(game, key = "Region", value = "Sales", NA_Sales, EU_Sales, Global_Sales, JP_Sales, Other_Sales)
#genre_sales <- aggregate(Global_Sales ~ Genre, game, sum)
#sorted_genre_totalsales <- genre_sales[order(-genre_sales$Global_Sales),]$Genre

#Data wrangling
sales_data <- game_melt[!(game_melt$Region=="Global_Sales"),]
#sales_data_platform <- aggregate(Sales ~ Platform+Year+Genre+Region, game_melt, sum)
#sales_data_publisher <- aggregate(Sales ~ Publisher+Year+Genre+Region, game_melt, sum)
top_game_init <- game_melt %>% #Initialize Top Game Card (Tab3)
    group_by(Name) %>%
    summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_genre_init <- game_melt %>% #Initialize Top Genre Card (Tab3)
    group_by(Genre) %>%
    summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_platform_init <- game_melt %>% #Initialize Top Platform Card (Tab3)
    group_by(Platform) %>%
    summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
    subset(`Copies Sold`== max(`Copies Sold`))

top_publisher_init <- game_melt %>% #Initialize Top Publisher Card (Tab3)
    group_by(Publisher) %>%
    summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
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

## Dropdown modules

dropdown_region = dccDropdown(id='region_selector',
                              options = list(list(label="North America",value="NA_Sales"),
                                             list(label="Europe",value="EU_Sales"),
                                             list(label="Japan",value="JP_Sales"),
                                             list(label="Other",value="Other_Sales"),
                                             list(label="Global", value = "Global_Sales")),
                              value='Global_Sales',
                              multi=TRUE)

dropdown_region_2 = dccDropdown(
    id='region_selector2',
    options = list(list(label="North America",value="NA_Sales"),
                   list(label="Europe",value="EU_Sales"),
                   list(label="Japan",value="JP_Sales"),
                   list(label="Other",value="Other_Sales"),
                   list(label="Global", value = "Global_Sales")),
    value='Global_Sales',
    multi=FALSE)

dropdown_platform = dccDropdown(id='platform_selector',options = platform_filter,value="all",multi=TRUE)

dropdown_genre = dccDropdown(id='genre_selector',options = genre_filter,value="all",multi=TRUE)

dropdown_publisher = dccDropdown(id='publisher_selector',options = publisher_filter,value="all",multi=TRUE)

dropdown_gamechoice = dccDropdown(id='popular_selector',
                                  options = list(list(label="Game Title",value="Game_Title"),
                                                 list(label="Publisher",value="Publisher"),
                                                 list(label="Platform",value="Platform")),
                                  value='Game_Title',
                                  multi=FALSE)

# Range slider modules

range_slider_timeseries = dccRangeSlider(id = "year_selector",
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


clearing_filters_button = dbcButton("Reset Filters",id="reset_button")

#  Collapse buttons
tab1_htu <- htmlDiv(list(
    dbcButton("How to use:",
              id="tab1-button",
              className="mb-3",
              color="primary"),
    dbcCollapse(
        id = "tab1-collapse",
        is_open = TRUE,
        dbcCard(list(
            dbcCardBody(list(
                htmlP("- Change the filters on sidebar to desired fields."),
                htmlP("- Use the `Reset Filters` button to return to default values."),
                htmlP("- Plots will change with the filters on the sidebar."),
                htmlP("- You can also filter the plots by clicking on their legends."),
                htmlP("- If you put your mouse over any point in the plots, you will see a tooltip with further details."),
                htmlP("- Click the legend to remove/isolate variables of interest.")
            ))
        ))
    )
))

tab2_htu <- htmlDiv(list(
    dbcButton("How to use:",
              id="tab2-button",
              className="mb-3",
              color="primary"),
    dbcCollapse(
        id = "tab2-collapse",
        is_open = TRUE,
        dbcCard(list(
            dbcCardBody(list(
                htmlP("- Change the filters on sidebar to desired fields."),
                htmlP("- Use the `Reset Filters` button to return to default values."),
                htmlP("- Plots will change with the filters on the sidebar."),
                htmlP("- You can also filter the plots by clicking on their legends."),
                htmlP("- If you put your mouse over any point in the plots, you will see a tooltip with further details."),
                htmlP("- Click the legend to remove/isolate variables of interest.")
            ))
        ))
    )
))

tab3_htu <- htmlDiv(list(
    dbcButton("How to use:",
              id="tab3-button",
              className="mb-3",
              color="primary"),
    dbcCollapse(
        id = "tab3-collapse",
        is_open = TRUE,
        dbcCard(list(
            dbcCardBody(list(
                htmlP("- Change the filters on sidebar to desired fields."),
                htmlP("- Both cards and plots will change with the filters on the sidebar."),
                htmlP("- Use the `Reset Filters` button to return to default values."),
                htmlP("- Use the dropdown to change the category of interest for the plot."),
                htmlP("- If you put your mouse over any point in the plots, you will see a tooltip with further details."),
                htmlP("- Click the legend to remove/isolate variables of interest.")
            ))
        ))
    )
))


# Tab modules
tab1_components = 
    htmlDiv(list(
        htmlBr(),
        htmlH3("Number of Games Released Over Time"),
        dccGraph(id='plot-area2'),
        htmlBr(),
        htmlH3("Global Number of Genres, Platforms and Publishers with Games Selling Over 100,000 Copies"),
        dccGraph(id='plot-area3')
    ))

first_tab_sidebar_Card = dbcCard(
    dbcCardBody(htmlDiv(
        list(htmlH4("Dashboard for Video Games Statistics")
        )
    ))
)

first_tab_sidebar_Card_2 = dbcCard(
    dbcCardBody(htmlDiv(
        list(clearing_filters_button,htmlBr(),htmlBr(),
             htmlLabel("Select your Region of Interest:"),
             dropdown_region,
             htmlBr(),
             htmlLabel("Select your Platform of Interest:"),
             dropdown_platform,
             htmlBr(),
             htmlLabel("Select your Genre of Interest:"),
             dropdown_genre,
             htmlBr(),
             htmlLabel("Select your Publisher of Interest:"),
             dropdown_publisher,
             htmlBr(),
             htmlLabel('Time Range'),
             range_slider_timeseries
             
        )
    ))
)




first_tab_figures_card = dbcCard(
    dbcCardBody(htmlDiv(list(tab1_components))))


row_tab1 = dbcRow(list(
    dbcCol(first_tab_sidebar_Card, width = 3),
    dbcCol(first_tab_figures_card), width = 9))


tab_1 = dccTab(label='Number of Games Released',children=list(
    dbcCard(list(
        dbcCardBody(list(
            htmlP("This tab contains information regarding the trend of game releases across Genres, Platforms and Publishers."),
            htmlP("NOTE: Every game was released in every Region. Therefore, the Region filter will not change these values."),
            tab1_htu
        ))
    )),
    htmlBr(),
    first_tab_figures_card))



tab_2 = dccTab(label='Number of Copies Sold', children=list(
    htmlDiv(list(
        dbcCard(list(
            dbcCardBody(list(
                htmlP("This tab contains information regarding the number of copies sold across Genres, Platforms and Publishers."),
                tab2_htu
            ))
        )),
        htmlBr(),
        dbcCard(list(
            dbcCardBody(list(
                htmlH3("Number of Copies Sold Over Time"),
                dccGraph(id='plot-area'),
                htmlBr(),
                htmlH3("Total Number of Copies Sold by Genre"),
                dccGraph(id='plot-area4')
            ))
        ))
    ))
))

tab_3 = dccTab(label='Top Copies Sold', children=list(
    #Information at the Top
    htmlDiv(list(
        dbcCard(list(
            dbcCardBody(list(
                htmlP("This tab contains information regarding top Games, Genres, Platforms and Publishers in terms of copies sold."),
                tab3_htu
            ))
        )),
        htmlBr(),
        # Top Score Cards
        dbcContainer(list(
            dbcRow(list(
                dbcCol(list(
                    dbcCard(list(
                        dbcCardHeader("Game with Most Copies Sold:"),
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
                    htmlBr(),
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
                    htmlBr(),
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
                    htmlBr(),
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
                ),width = 4),
                #Graph
                dbcCol(list(
                    dbcCard(
                        dbcCardBody(list(
                            htmlH3(id="graph_5_title"),
                            htmlBr(),
                            htmlLabel("Select your choice of interest:"),
                            dropdown_gamechoice,
                            dccGraph(id='plot-area5'),
                            htmlBr()
                        ))
                    )    
                ))
            ))
        ))
    ))
))

app$layout(dbcRow(list(
    dbcCol(list(
        htmlDiv(list(first_tab_sidebar_Card, htmlBr(), first_tab_sidebar_Card_2, htmlBr()))),width = 3),
    dbcCol(dbcContainer(
        (htmlDiv(htmlDiv(list(
            dccTabs(id="tabs", children=list(
                tab_1,tab_2,tab_3
            ))
        ))
        ))), width = 9)
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

#Callback for HTU button - Tab 1
app$callback(
    list(output("tab1-collapse","is_open")),
    list(input("tab1-button","n_clicks"),
         state("tab1-collapse","is_open")),
    function (n_clicks,is_open){
        #Input: Button and State of Collapse
        #Output: Opposite of Current State of Collapse
        return (list(!is_open))
    }
)

#Callback for HTU button - Tab 2
app$callback(
    list(output("tab2-collapse","is_open")),
    list(input("tab2-button","n_clicks"),
         state("tab2-collapse","is_open")),
    function (n_clicks,is_open){
        #Input: Button and State of Collapse
        #Output: Opposite of Current State of Collapse
        return (list(!is_open))
    }
)


#Callback for HTU button - Tab 3
app$callback(
    list(output("tab3-collapse","is_open")),
    list(input("tab3-button","n_clicks"),
         state("tab3-collapse","is_open")),
    function (n_clicks,is_open){
        #Input: Button and State of Collapse
        #Output: Opposite of Current State of Collapse
        return (list(!is_open))
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
        
        filtered_subset <- game_melt %>% 
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year)
        
        top_game <-  filtered_subset %>%
            group_by(Name) %>%
            summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_genre <- filtered_subset %>%
            group_by(Genre) %>%
            summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_publisher <- filtered_subset %>%
            group_by(Publisher) %>%
            summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
            subset(`Copies Sold`== max(`Copies Sold`))
        
        top_platform <- filtered_subset %>%
            group_by(Platform) %>%
            summarise("Copies Sold" = sum(`Copies_Sold`)) %>%
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
            summarise("Copies Sold" = sum(`Copies_Sold`)) %>% 
            ggplot() +
            aes(x=as.factor(Year),
                y=`Copies Sold`,
                fill = Genre,
                group = 1,
                text = paste("Year: ",as.factor(Year),
                             "<br>Copies Sold: ",`Copies Sold`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity")+ #geom_line
            theme_bw() +
            theme(legend.title=element_blank()) +
            theme(panel.grid.major.x = element_blank()) + 
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2)) +
            #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
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
                group = 1,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Games Released: ",`Number of Releases`,
                             "<br>Genre: ", Genre)) + 
            geom_bar(stat="identity") + #geom_line
            theme_bw() +
            theme(legend.title=element_blank()) +
            theme(panel.grid.major.x = element_blank()) + 
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
            ylab("Number of Games Released")+
            xlab("Year")
        graph2<-ggplotly(graph2,tooltip="text")
        
        graph3 <- filtered_game_melt %>%
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
                group = 1,
                text = paste("Year: ",as.factor(Year),
                             "<br>No. of Major Genre, Publishers, Platforms: ",`Counts of Genres, Publishers and Platforms`,
                             "<br>Category: ", Category)) + 
            geom_area(stat="identity")+
            theme_bw() +
            theme(legend.title=element_blank()) +
            theme(panel.grid.major.x = element_blank()) + 
            theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2))+
            #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
            ylab("Number of Major Genres, Publishers and Platforms")+
            xlab("Year")
        graph3<-ggplotly(graph3,tooltip="text")
        
        return(list(graph1,graph2,graph3))
    }
)

#Callback for linking time sliders



#Callback for Tab2-Plot2
app$callback(
    output('plot-area4', 'figure'),
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
        filtered_subset <- game_melt %>%
            subset(Region %in% filter_region & Platform %in% filter_plat & Genre %in% filter_gen & Publisher %in% filter_pub & Year >= min_year & Year <= max_year)
        graph3 <- filtered_subset %>%
            group_by(Genre) %>%
            summarize(genre_sales = sum(Copies_Sold)) %>%
            ggplot() +
            aes(x=reorder(Genre,-genre_sales),
                y=genre_sales,
                fill=Genre,
                text=paste("Genre: ", Genre,
                           "<br>Copies Sold: ", genre_sales)) +
            geom_bar(stat="identity") +
            theme_bw() +
            theme(legend.position = "none") + 
            #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
            ylab("Number of Copies Sold (in millions)") +
            xlab("Genre")
        return (ggplotly(graph3,tooltip="text"))
    }
)

#Callback for Tab3-Plot1
app$callback(
    output('plot-area5', 'figure'),
    list(input('popular_selector', 'value')),
    function(plot_type) {
        # Input: List of Regions
        # Output: Graph
        sales_sub <- data.table(sales_data, key="Genre")[, head(.SD, 30), by=Genre]
        if (plot_type == "Game_Title"){
            graph4 <- sales_sub %>%
                ggplot() +
                aes(x=reorder(Genre,-Copies_Sold), 
                    y=Copies_Sold, 
                    fill = Genre, 
                    color = Genre,
                    text=paste("Genre: ", Genre,
                               "<br>Copies Sold: ", Copies_Sold)) +
                geom_point() + 
                geom_text(aes(label=ifelse(Copies_Sold>15,as.character(Name),'')),hjust=-0.1, vjust=0) +
                theme(axis.text.x = element_text(angle=45, hjust=0.9, vjust=0.9))+
                #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                ylab("Number of Copies Sold (in millions)") +
                xlab("Genre")
        } else if (plot_type == "Publisher") {
            graph4 <- sales_sub %>%
                group_by(Genre, Publisher) %>%
                summarise(net_sales = sum(Copies_Sold), `.groups` = 'keep') %>%
                ggplot() +
                aes(x=reorder(Genre,-net_sales), 
                    y=net_sales, 
                    fill = Genre, 
                    color = Genre,
                    text=paste("Genre: ", Genre,
                               "<br>Copies Sold: ", net_sales)) +
                geom_point() +
                geom_text(aes(label=ifelse(net_sales>50,as.character(Publisher),'')),hjust=-0.1, vjust=0) +
                theme(axis.text.x = element_text(angle=45, hjust=0.9, vjust=0.9)) +
                #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                ylab("Number of Copies Sold (in millions)") +
                xlab("Genre")
        } else if (plot_type == "Platform") {
            graph4 <- sales_sub %>%
                group_by(Genre, Platform) %>%
                summarise(net_sales = sum(Copies_Sold), `.groups` = 'keep') %>%
                ggplot() +
                aes(x=reorder(Genre,-net_sales), 
                    y=net_sales, 
                    fill = Genre, 
                    color = Genre,
                    text=paste("Genre: ", Genre,
                               "<br>Copies Sold: ", net_sales)) +
                geom_point() +
                geom_text(aes(label=ifelse(net_sales>30,as.character(Platform),'')),hjust=-0.1, vjust=0) +
                #scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                theme(axis.text.x = element_text(angle=45, hjust=0.9, vjust=0.9)) +
                ylab("Number of Copies Sold (in millions)") +
                xlab("Genre")
        }
        return (ggplotly(graph4,tooltip="text"))
    }
)

#app$run_server(host = '0.0.0.0')
app$run_server(debug=T)
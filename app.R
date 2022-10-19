library(shinydashboard)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
theme_set(theme_classic())

data <- read.csv("acled_west_inter.csv")[,-1]
data.b <- read.csv("acled.csv")[,-1]

geo_data <- st_read("wca_admbnda_adm2_ocha/wca_admbnda_adm2_ocha.shp")

#Garder Burkina
geo_data.b <- geo_data %>% filter(admin0Pcod == "BF") %>% select(admin2Name,13:15)

my.data.frame1_geo <- data.b %>% 
    group_by(admin2, year) %>% 
    summarise(n = n())

my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Komandjari", 
                                    "Komonjdjari", my.data.frame1_geo$admin2)
my.data.frame1_geo$admin2 <- ifelse(my.data.frame1_geo$admin2 == "Kourittenga", 
                                    "Kouritenga", my.data.frame1_geo$admin2)

my.data.frame1_geo[146,] <-list("Leraba", 2019, 0)
my.data.frame1_geo[147,] <-list("Leraba", 2020, 0)
my.data.frame1_geo[148,] <-list("Leraba", 2021, 0)
my.data.frame1_geo[149,] <-list("Leraba", 2022, 0)

#Nombre d'éléments par provinces
my.data.frame1_geo <- my.data.frame1_geo %>% 
    pivot_wider(names_from = year, values_from = n, values_fill = 0) %>% 
    pivot_longer(2:5, names_to = "year", values_to = "n") %>% 
    left_join(geo_data.b, by = c("admin2" = "admin2Name")) %>% 
    st_as_sf()

#Données de description
desc <- read.delim("description.txt")
desc_text <- paste(desc$My.text, collapse = "<br/>")


ui <- dashboardPage(
    dashboardHeader(title = "Security dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("MAP", tabName = "map", icon = icon("map")),
            selectInput("country", "Choisir le pays", choices = unique(data$country), 
                        selected = "Burkina Faso"),
            # selectInput("admin1", "select Variable", choices = unique(data$admin1)),
            # selectInput("admin2", "select Variable", choices = unique(data$admin2)),
            selectInput("event_type", "Choose event type", choices = unique(data$event_type), 
                        multiple = T, selected = unique(data$event_type)),
            # selectInput("interaction", "select Variable", choices = unique(data$interaction))
            menuItem("About", tabName = "about", icon = icon("info"))
            
    )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                # Boxes need to be put in a row (or column)
                fluidRow(
                    infoBoxOutput("region", width = 3),
                    infoBoxOutput("province", width = 3),
                    infoBoxOutput("event", width = 3),
                    infoBoxOutput("interaction", width = 3)
                ),
                fluidRow(
                    box("Evolution", plotOutput("evol", height = 200), width = 6),
                    box("Event type", plotOutput("plot3", height = 200), width = 6)
                ),
                fluidRow(
                    box("Regions", plotOutput("plot1", height = 200), width = 4),
                    box("Provinces", plotOutput("plot2", height = 200), width = 4),
                    box("Interaction", plotOutput("plot4", height = 200), width = 4)
                ),
                fluidRow(
                    box("Country", plotOutput("country_plot", height = 200), width = 12))),
    tabItem(tabName = "map", 
            box("Events by provinces", plotOutput("plot5", height = 500), width = 12)),
    tabItem(tabName = "about", 
            fluidRow(
                box(title = "Description", htmlOutput("text"), 
                    width = 12)))
)))


server <- function(input, output) {
    
    output$text <- renderUI({
        HTML(desc_text)
    })
    
    region_max <- reactive({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(admin1) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1) %>% pull(admin1)
    })
    
    province_max <- reactive({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(admin2) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1) %>% pull(admin2)
    })
    
    inter_max <- reactive({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(inter) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1) %>% pull(inter)
    })
    
    event_max <- reactive({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(event_type) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1) %>% pull(event_type)
    })
    
    output$region <- renderInfoBox({
        infoBox("Region", region_max(), icon = icon("map-pin"))
    })
    
    output$province <- renderInfoBox({
        infoBox("Province", province_max(), icon = icon("map-pin"))
    })
    
    output$interaction <- renderInfoBox({
        infoBox("Interaction",
                h1(strong( inter_max()),
                style = "font-size:11px;"), icon = icon("people-arrows"))
    })
    
    output$event <- renderInfoBox({
        infoBox("Event", h1(strong(event_max()), 
                            style = "font-size:11px;"),
                icon = icon("meteor"))
    })
    
    output$country_plot <- renderPlot({
        data  %>% filter(country %in% input$country) %>% 
            filter(event_type %in% input$event_type) %>%
            group_by(country) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1:10) %>% 
            ggplot() + geom_col(aes(reorder(country, n), n)) + 
            theme(axis.text.x = element_text(angle = 10)) +
            scale_x_discrete(name = NULL) +
            scale_y_continuous(NULL)
    })
    
    output$plot1 <- renderPlot({
        data  %>% filter(country %in% input$country) %>% 
            filter(event_type %in% input$event_type) %>%
            group_by(admin1) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1:10) %>% 
            ggplot() + geom_col(aes(reorder(admin1, n), n)) + 
            theme(axis.text.x = element_text(angle = 40)) +
            scale_x_discrete(name = NULL)+
        scale_y_continuous(NULL)
    })
    
    output$plot2 <- renderPlot({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(admin2) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1:10) %>% 
            ggplot() + geom_col(aes(reorder(admin2, n), n)) + 
            theme(axis.text.x = element_text(angle = 40)) +
            scale_x_discrete(name = NULL)+
            scale_y_continuous(NULL)
    })
        
    output$plot3 <- renderPlot({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(event_type) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1:10) %>% 
            ggplot() + geom_col(aes(reorder(event_type, n), n)) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 10)) +
            scale_x_discrete(name = NULL)+
            scale_y_continuous(NULL)
    })       
    
    output$plot4 <- renderPlot({
        data  %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            group_by(interaction) %>% count %>% ungroup %>% 
            arrange(desc(n)) %>%   
            slice(1:10) %>% 
            ggplot() + geom_col(aes(reorder(interaction, n), n)) + 
            theme(axis.text.x = element_text(angle = 0)) +
            scale_x_discrete(name = input$var)+
            scale_y_continuous(NULL)
    })
        
    output$plot5 <- renderPlot({
        my.data.frame1_geo %>% 
            group_by(admin2) %>% 
            summarise(n = sum(n)) %>%  ggplot +
            geom_sf(aes(fill = n)) + 
            geom_sf_label(aes(label = admin2), size = 3,
                          label.padding =unit(.5, "mm")) +
            theme_classic() + 
            theme(axis.text = element_blank(), 
                  axis.line = element_blank(), 
                  axis.ticks = element_blank()) +
            xlab(NULL) + ylab(NULL) +
            scale_fill_gradient2(
                low = "grey", 
                mid = "white", 
                high = "red") +
            annotation_scale(
                location = "br",
                width_hint = 1,
                pad_x = unit(0, "cm"),
                pad_y = unit(.04, "cm")) +
            annotation_north_arrow(
                location = "br", width = unit(0.5, "in"), height = unit(0.5, "in"),
                pad_x = unit(0, "in"),
                pad_y = unit(0.6, "in"),
                style = north_arrow_nautical
            )
    
    })
    
    output$evol <- renderPlot({
        data %>% filter(country %in% input$country) %>%
            filter(event_type %in% input$event_type) %>%
            mutate(event_date = as.Date(event_date), 
                   data_m = format(event_date, "%y_%m")) %>% 
            group_by(data_m) %>% summarise(n = n()) %>% 
            ggplot()+ 
            # geom_line(aes(x = data_m, y = n, group = 1))+ 
            geom_col(aes(x = data_m, y = n), alpha = .5) + 
            theme(axis.text.x = element_text(angle = 90)) +
            xlab("Evolution sur la période (Septembre 2019 - Septembre 2022)")
        
    })
}

shinyApp(ui, server)


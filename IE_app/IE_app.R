# required libraries are loaded
library(shiny)
require(leaflet)
require(ggplot2)
require(data.table)
library(rgdal)
library(dplyr)
require(plotly)
require(shinythemes)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)



#for Obesity : 
us_data <- read.csv("orderdata.csv")

#removing National
us_data = filter(us_data, States!= "National")

#only obesity data from all the data
obese_overweight<- us_data[us_data$Class=="Obesity / Weight Status",]

#average obesity for each state for map
obese_overweight <- obese_overweight %>%
    group_by(States) %>% summarise(mean = mean(Percentage))








#Reading USA shape file
usa_states = readOGR("us_states/cb_2019_us_state_5m/cb_2019_us_state_5m.shp")
#plot(usa_states)

# to check if the state names in the shape file matches with my data
is.element(obese_overweight$States,usa_states$NAME)

#checking if opposite is true
is.element(usa_states$NAME,obese_overweight$States)



#making sure both the data sets have matching state names
usa_states <- subset(usa_states, is.element(usa_states$NAME, obese_overweight$States))



#align the data across both the datasets
obese_overweight <- obese_overweight[order(match(obese_overweight$States,usa_states$NAME)),]

#creating bins for different shades of colors:
#color scheme for choropleth

palette <- colorBin(c('#fff7ec',  
                      '#fee8c8',
                      '#fdd49e',
                      '#fdbb84',
                      '#fc8d59',
                      '#ef6548',
                      '#d7301f',
                      '#b30000',
                      '#7f0000'), 
                    bins = c(27,28,29,30,31,32,33,34,35,36))

#popups

#for obesity
popup1 <- paste0("<span style='color: #7f0000'><strong>Obesity levels</strong></span>",
                 "<br><span style='color: #ef6548;'><strong>State:</strong></span>", 
                 obese_overweight$States, 
                 "<br><span style='color: #ef6548;'><strong>Percentage</strong></span>", 
                 round(obese_overweight$mean,2))


#for physical activity

physical<- us_data[us_data$Class=='Physical Activity'&us_data$Question!='Percent of adults who engage in no leisure-time physical activity',]

#average activity for each state for map
physical <- physical %>%
    group_by(States) %>% summarise(mean = mean(Percentage))

# to check if the state names in the shape file matches with my data
is.element(physical$States,usa_states$NAME)

#checking if opposite is true
is.element(usa_states$NAME,physical$States)

#making sure both the data sets have matching state names (new variable for physical)
usa_states1 <- subset(usa_states, is.element(usa_states$NAME, physical$States))

#align the data across both the datasets
physical <- physical[order(match(physical$States,usa_states1$NAME)),]


#creating bins for different shades of colors:
#color scheme for choropleth

palette1 <- colorBin(c('#f7fcf0',  
                       '#e0f3db',
                       '#ccebc5',
                       '#a8ddb5',
                       '#7bccc4',
                       '#4eb3d3',
                       '#2b8cbe',
                       '#0868ac',
                       '#084081'), 
                     bins = c(15,18,21,24,27,30,33,36,39.5))

#popup

#for physical activity
popup2 <- paste0("<span style='color: #084081'><strong>Physical Activity levels</strong></span>",
                 "<br><span style='color: #4eb3d3;'><strong>State:</strong></span>", 
                 physical$States, 
                 "<br><span style='color: #4eb3d3;'><strong>Percentage</strong></span>", 
                 round(physical$mean,2))



#socio economic factors:

#age
age<- subset(us_data, StratificationCategory1 == "Age (years)")

#age obesity
age_obese<- subset(age, Class == "Obesity / Weight Status")

age_obese<- age_obese %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#age physical
age_physical<- subset(age, Class == "Physical Activity"&
                          Question!='Percent of adults who engage in no leisure-time physical activity')

age_physical<- age_physical %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#Education

education <- subset(us_data, StratificationCategory1 =="Education")

#education obesity

education_obese<- subset(education, Class == "Obesity / Weight Status")

education_obese<- education_obese %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#education physical activity

education_physical<- subset(education, Class == "Physical Activity"&
                                Question!='Percent of adults who engage in no leisure-time physical activity')

education_physical<- education_physical %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))


#Gender

gender <- subset(us_data, StratificationCategory1 =="Gender")

#gender obesity

gender_obese<- subset(gender, Class == "Obesity / Weight Status")

gender_obese<- gender_obese %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#gender physical activity

gender_physical<- subset(gender, Class == "Physical Activity"&
                             Question!='Percent of adults who engage in no leisure-time physical activity')

gender_physical<- gender_physical %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))


#Income

income <- subset(us_data, StratificationCategory1 =="Income")

#income obesity

income_obese<- subset(income, Class == "Obesity / Weight Status")

income_obese<- income_obese %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#income physical activity

income_physical<- subset(income, Class == "Physical Activity"&
                             Question!='Percent of adults who engage in no leisure-time physical activity')

income_physical<- income_physical %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#Race/Ethnicity

race<- subset(us_data, StratificationCategory1 =="Race/Ethnicity")

#Race obesity

race_obese<- subset(race, Class == "Obesity / Weight Status")

race_obese<- race_obese %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))

#race physical activity

race_physical<- subset(race, Class == "Physical Activity"&
                           Question!='Percent of adults who engage in no leisure-time physical activity')

race_physical<- race_physical %>%
    group_by(Stratification1) %>% summarise(mean = mean(Percentage))


#for gender obese and gender physical for all years:

gender_obese_bar<- subset(gender, Class == "Obesity / Weight Status")

gender_obese_bar<- gender_obese_bar %>%
    group_by(Stratification1,Year) %>% summarise(mean = mean(Percentage))

gender_obese_bar<- gender_obese_bar %>% 
    rename(
        Gender = Stratification1
    )

gender_obese_bar$Year <- as.factor(gender_obese_bar$Year)



gender_physical_bar<- subset(gender, Class == "Physical Activity"&
                                 Question!='Percent of adults who engage in no leisure-time physical activity')

gender_physical_bar<- gender_physical_bar %>%
    group_by(Stratification1,Year) %>% summarise(mean = mean(Percentage))


gender_physical_bar<- gender_physical_bar %>% 
    rename(
        Gender = Stratification1
    )

gender_physical_bar$Year <- as.factor(gender_physical_bar$Year)


#correlation
unhealthy<- us_data[us_data$Class=='Fruits and Vegetables',]

#Relation of obesity and unhealthy diet

obeseagg<- aggregate(Percentage~States, us_data[us_data$Class=="Obesity / Weight Status",],mean)
unhealthyagg <- aggregate(Percentage ~ States, unhealthy, mean)



obese.unhealthy <- merge(obeseagg, unhealthyagg, by="States", all = T)

obese.unhealthy<- obese.unhealthy[obese.unhealthy$States!='Virgin Islands',]
obese.unhealthy<- obese.unhealthy[obese.unhealthy$States!='Puerto Rico',]

obese.unhealthy<- obese.unhealthy %>% 
    rename(
        obesity = Percentage.x,
        unhealthy_food_habits = Percentage.y
    )



n <- 55
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))





















# Define UI 
shinyUI <- (fluidPage( 
    
    navbarPage(theme = shinythemes::shinytheme("cyborg"),""),
    # Application title
    headerPanel("Campgrounds and facilities in National parks"),
    
    # Sidebar with 2 selection inputs 
    
    sidebarLayout(
        
        sidebarPanel(
            
            selectInput("variable", "Socio-Economic Factors:", 
                        choices = c("Age" = "age", 
                                    "Education" = "education",
                                    "Gender" = "gender",
                                    "Income" = "income",
                                    "Race/Ethnicity" = "race"),selected = "race"
            ),
            selectInput("variable2", "Obesity or Physical activity",
                        c("obesity"="obesity","physical activity"="physical activity")
                        
                        
            ),
            h3(textOutput("caption")),
            plotOutput("plot1"),
            h6(tags$span(style="color: #FFFFFF","Females are increasingly becoming obese compared to males, West Virginia and Mississippi are the most obese states in USA, Colorado as state is more into physical activity, income and education are good indicators of unhealthy diet. 
Race and age are good indicators of obesity whereas income and education is not. 
")),
            h6(tags$span(style="color: #FFFFFF","Socio-economic conditions include age, gender, race, education and income. Race, age and gender are influential factors in obesity levels. Although income and education doesn’t seem to affect obesity levels as much. Income and education are not reliable indicators of obesity.

In the case of unhealthy food habits, it might not be an individual’s choice that dictates if they are able acquire healthy foods for consumption (low income and insufficient education may lead to poor decisions or consumption of low-quality foods). On the other hand, obesity seem to be a rather preventable disease in most cases (small percentage of people are obese due to pre-existing health conditions.) which means it is mostly dictated by an individual’s choices. Thus, their level of education and income has little effect on decisions that lead to obesity."))
            
            
            
            
            
            
        ),
        
        
        # Show the caption and plot of the bar charts, bubble charts and also plot the leaflet map
        mainPanel(
            h3(tags$span(style="color: #FFFFFF","How does obesity relate to an unhealthy lifestyle?")),
            plotlyOutput("plot4"),
            h6(tags$span(style="color: #FFFFFF","Here unhealthy lifestyle constitutes of low levels of physical activity and not consuming healthy foods. It is clear that, as unhealthy lifestyle percentages increases obesity levels also increase.")),
            h3(tags$span(style="color: #FFFFFF","Obesity/Physical activity by US states Choropleth")),
            leafletOutput("mymap"),
            h6(tags$span(style="color: #FFFFFF","It’s quite interesting to note that Colorado has the lowest level of obesity amongst the US states. Mississippi and Arkansas have low levels of physical activity and consequentially are on the high spectrum in obesity levels. We can see this trend for each US state (I only highlighted the extreme cases). Through analysis of this trend it is safe to say that physical activity and obesity level are inversely correlated (states with higher levels of physical activity have low levels of obesity).")),
            h3(tags$span(style="color: #FFFFFF","Why are Females becoming obese faster?")),
            splitLayout(
                plotlyOutput("plot2"),
                plotlyOutput("plot3")),
            h6(tags$span(style="color: #FFFFFF","Physical activity levels are different amongst males and females. Males are taking part in muscular training and leisure time physical activity more than females. This might be a factor that helps to answer the question. My hypothesis: obesity rate amongst females is increasing faster than males due to the fact that males are on average more engaged in physical activity than females."))
            
            
            
            
            
        )
    )
))

# server.R

# Define server logic required 
shinyServer <- (function(input, output) {
    
    # Return the formula text for printing as a caption
    output$caption <- reactiveText(function() {
        paste("Variations based on:", input$variable)
    })
    
    # Generate a plot of variations by socio-economic factors 
    
    # ggplot version
    
    output$plot1 <- reactivePlot(function() {
        # check for the input variable
        # for each socio-economic factor takings subsets of data (created earlier)
        if (input$variable == "age"&input$variable2=="obesity") {
            
            ggdata<- age_obese
        }
        
        else if (input$variable == "age"&input$variable2=="physical activity") {
            
            ggdata<- age_physical
        }
        
        else if (input$variable == "education"&input$variable2=="obesity") {
            
            ggdata<-education_obese
        }
        
        else if (input$variable == "education"&input$variable2=="physical activity") {
            
            ggdata<-education_physical
        }
        
        else if (input$variable == "gender"&input$variable2=="obesity"){
            
            ggdata<-gender_obese
            
        }
        
        else if (input$variable == "gender"&input$variable2=="physical activity"){
            
            ggdata<-gender_physical
            
        }
        
        else if (input$variable == "income"&input$variable2=="obesity"){
            
            ggdata<-income_obese
            
        }
        
        else if (input$variable == "income"&input$variable2=="physical activity"){
            
            ggdata<-income_physical
            
        }
        
        else if (input$variable == "race"&input$variable2=="obesity"){
            
            ggdata<-race_obese
            
        }
        
        else if (input$variable == "race"&input$variable2=="physical activity"){
            
            ggdata<-race_physical
            
        }
        
        
        
        #ggplot2 viz
        ggplot(ggdata,aes(x=Stratification1,y=mean,fill=Stratification1))+
            geom_bar(stat = "identity")+
            
            
            scale_fill_brewer(palette="Set3")+
            labs (y= "percentage (%)\n", 
                  x = "\nCategories", 
                  fill = "groups", 
                  title = "Socio-Economic Factors\n")+
            theme_dark()+
            geom_text(aes(label=round(mean,2)), vjust=-0.2)+
            theme(axis.text.x = element_text(angle = 90))
        
        
        
        
    })
    
    
    # interactive leaflet map 
    output$mymap <- renderLeaflet({ # create leaflet map
        k <- leaflet() %>%
            addProviderTiles(providers$Stamen.Toner)%>%
            setView(lng = -94.676392, lat = 39.106667, zoom = 3.5)%>% #centre on kansas
            addPolygons(data = usa_states,
                        fillOpacity = 0.75,
                        fillColor = palette(obese_overweight$mean),
                        highlightOptions = highlightOptions(color = "red", weight = 5,
                                                            bringToFront = TRUE),
                        color="white",
                        weight = 1,
                        popup = popup1,
                        group="<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>")%>%
            addPolygons(data = usa_states1,fillOpacity = 0.75,
                        fillColor = palette1(physical$mean),
                        highlightOptions = highlightOptions(color = "blue", weight = 5,
                                                            bringToFront = TRUE),
                        color="white",
                        weight = 1,
                        popup = popup2,
                        group="Physical Activity Map")%>%
            
            addLayersControl(
                baseGroups = c("<span style='color: #7f0000; font-size: 10pt'><strong>Obesity Map</strong></span>",
                               "<span style='color: #084081; font-size: 10pt'><strong>Physical Activity Map</strong></span>"),
                options = layersControlOptions(collapsed = FALSE))
        
        print(k)
    })
    
    output$plot2 <- renderPlotly({
        #for obesity
        
        #ggplot2 viz
        ggplotly(ggplot(gender_obese_bar, aes(fill = Gender, y = mean, x = Year)) + 
                     geom_bar(position = "dodge", stat = "identity") +
                     theme_classic() +
                     scale_fill_manual(values=c("#CC79A7", "#0072B2")) +
                     theme(axis.text = element_text(size = 8),
                           axis.title = element_text(size = 10),
                           plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                           legend.title = element_text(size = 10)) +
                     labs (y= "Obese percentage\n", 
                           x = "\nYears", 
                           fill = "Gender", 
                           title = "Levels of obesity in USA by gender\n") 
        )
        
    })
    
    output$plot3 <- renderPlotly({
        
        #for physical activity
        
        #ggplot2 viz
        ggplotly(ggplot(gender_physical_bar, aes(fill = Gender, y = mean, x = Year)) + 
                     geom_bar(position = "dodge", stat = "identity") +
                     theme_classic() +
                     scale_fill_manual(values=c("#CC79A7", "#0072B2")) +
                     theme(axis.text = element_text(size = 8),
                           axis.title = element_text(size = 10),
                           plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
                           legend.title = element_text(size = 10)) +
                     labs (y= "Physical activity percentage\n", 
                           x = "\nYears", 
                           fill = "Gender", 
                           title = "Levels of physical activity in USA by gender\n"))
        
        
    })
    
    output$plot4 <- renderPlotly({
        
        #bubblechart plotly
        
        fig <- plot_ly(obese.unhealthy, x = ~unhealthy_food_habits, y = ~obesity, type = 'scatter', mode = 'markers', size = ~(abs(obesity-unhealthy_food_habits)), color = ~States, colors = col_vector,
                       #Choosing the range of the bubbles' sizes:
                       sizes = c(10, 50),
                       marker = list(symbol = 'circle',
                                     line = list(width = 2, color = '#FFFFFF'),
                                     opacity = 0.8, sizemode = 'diameter'),
                       hoverinfo = 'text',
                       text = ~paste('State:', States, '<br>% Disparity:', (round(abs(obesity-unhealthy_food_habits),2))))
        fig <- fig %>% layout(paper_bgcolor='#696969',plot_bgcolor='#696969',
                              title = 'Relation of Obesity and Unhealthy Lifestyle habits by States',
                              xaxis = list(showgrid = T,color = '#ffffff',title='Unhealthy Lifestyle'),
                              yaxis = list(showgrid = T,color = '#ffffff',title='Obesity'),
                              showlegend = FALSE)
        
        fig
        
    })
    
    
    
    
})



# Run the interactive shiny application 
shinyApp(ui = shinyUI, server = shinyServer)





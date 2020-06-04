library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

###Defenders DATA####
###
require(broom)
require(gdata)
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 
#data.all <- separate(data = data.all, col = TurretType, into = c("Turret", "Upgrade"), sep = "LV")
data.all <- filter(data.all, Level > 0)
data.all$Level <- as.factor(data.all$Level)
data.all$Round <- as.factor(data.all$Wave)
data.all$Location <- as.factor(data.all$Location)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)


#data.all <- filter(data.all, Medicine == "red" | Medicine == "blue")
#data.all <- filter(data.all, Shot > 0)
data.all <- mutate(data.all, PercDestroyed = Destroyed/Shot)
data.all["None"] = NA
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}

ui <- fluidPage(
    # App title ----
    titlePanel("Defenders Data Visualizations"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "groupID",
                        label = "Group ID:", 
                        choices =  c("all", all_groups),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = "all"),
            
            selectInput(inputId = "playerID",
                        label = "Player ID:",
                        choices =  c("all", all_players),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = "all"),
            
            selectInput("levels", "Level",
                        choices = c(1, 2, 3, 4, 5),
                        multiple = TRUE,
                        selected = "1"),
            
            selectInput(inputId = "xvar",
                        label = "X Axis:",
                        #columns of the dataset
                        choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                        selected = "Medicine",
                        multiple = FALSE),
            
            selectInput(inputId = "yvar",
                        label = "Y Axis:",
                        #columns of the dataset
                        choices = c("Shot", "Destroyed", "PercDestroyed"),
                        selected = "Shot",
                        multiple = FALSE),
            
            checkboxInput('bplot',"Add boxplot",FALSE),
            
            selectInput(inputId = "color",
                        label = "Color by",
                        choices = c("None", "Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus" ),
                        selected = "None",
                        multiple = FALSE),
            
            selectInput(inputId = "facets",
                        label = "Facet by",
                        choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus", "None"),
                        selected = "None",
                        multiple = FALSE),
            
            
            selectInput(inputId = "tests",
                        label = "Statistic Tests",
                        choices = c("None", "two-sample t-test", "ANOVA"),
                        selected = "None",
                        multiple = FALSE),
            ##"paired T-test"
            downloadButton('downloadData', label = "Defenders Data"),
            
            p(h4("For more information, hit the link below:"), align = "center"),
            a(h4("More Information"),
              href="https://stat2labs.sites.grinnell.edu/defenders.html", 
              align="center")
            
            
            
        ),
        
        mainPanel(
            
            plotOutput(outputId = "Plot"),
            verbatimTextOutput("help"),
            verbatimTextOutput("why"),
            tableOutput("tbl1"),
            h3(textOutput("caption"))
        )
    )
)

server <- function(input, output,session) {
    
    # Updates PlayerID based upon GroupID
    observe({
        # req() requires a selection from GroupID before any output
        # or reactivity occurs (keeps the app from crashing)
        req(input$groupID)   
        
        if ("all" %in% input$groupID) {gamedata <- data.all}
        else{gamedata <- filter(data.all, GroupID == input$groupID)}
        
        updateSelectInput(session, 
                          "playerID",
                          choices = c("all", sort(unique(gamedata$PlayerID))),
                          selected = "all")
    })
    
    # Updates Levels based upon GroupID
    observe({
        req(input$groupID)   
        
        if ("all" %in% input$groupID) {gamedata <- data.all}
        else{gamedata <- filter(data.all, GroupID == input$groupID)}
        updateSelectInput(session, 
                          "levels",
                          choices = sort(unique(gamedata$Level)),
                          selected = sort(unique(gamedata$Level))[c(1)])
    })
    
    
    output$Plot <- renderPlot({
        req(input$groupID)
        data.all <- data.all[data.all$Level %in% input$levels, ]
        
        if ("all" %in% input$groupID) {plotData <- data.all}
        else{
            if("all" %in% input$playerID) {plotData <- data.all[data.all$GroupID %in% input$groupID, ]}
            else{
                plotData <- data.all[data.all$GroupID %in% input$groupID, ]
                plotData = plotData[plotData$PlayerID %in% input$playerID, ]
            }
        }
        
        
        if (input$bplot == "TRUE"){
          if(input$color != "None"){
            myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
                geom_boxplot() +
              geom_point(position = position_jitterdodge()) + 
                scale_color_manual(values=c("blue", "red", "orange", "purple", "lightblue", "pink", "green", "gray", "black", "lightsalmon", "lightskyblue", "maroon"))+
                theme(axis.text.x = element_text(size=20))+
                xlab(input$xvar) + ylab(input$yvar)+
            labs(title = paste("Plot of ",input$yvar, "by",input$xvar, "and colored by", input$color)) + 
                theme(axis.text.x = element_text(size = 10))
          }
          else{
            myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, fill = input$color)) +
              geom_boxplot() +
            geom_point(position = position_jitterdodge()) + 
              theme(axis.text.x = element_text(size=20))+
              xlab(input$xvar) + ylab(input$yvar) + 
              scale_fill_manual(values = rep(NA, 2)) +
              labs(title = paste("Plot of ",input$yvar, "by",input$xvar)) + 
                theme(axis.text.x = element_text(size = 10))
          }
        } else {
          if(input$color != "None"){
            myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
               geom_point(position = position_jitterdodge()) + 
                scale_color_manual(values=c("blue", "red", "orange", "purple", "lightblue", "pink", "green", "gray", "black", "lightsalmon", "lightskyblue", "maroon"))+
                theme(axis.text.x = element_text(size=20))+
                xlab(input$xvar) + ylab(input$yvar) +
              labs(title = paste("Plot of ",input$yvar, "by",input$xvar, "and colored by", input$color)) +
                theme(axis.text.x = element_text(size = 10))
          }
          
          else{
            myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, fill = input$color)) +
             geom_point(position = position_jitterdodge()) + 
              theme(axis.text.x = element_text(size=20))+
              xlab(input$xvar) + ylab(input$yvar) + 
              scale_fill_manual(values = rep(NA, 2)) + 
              labs(title = paste("Plot of ",input$yvar, "by",input$xvar)) +
                theme(axis.text.x = element_text(size = 10))
          }
        }
        
        if (input$facets != "None") {
            myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
                labs(title = paste("Plot of ",input$yvar, "by",input$xvar, "and Faceted by type of", input$facets)) +
                theme(axis.text.x = element_text(size = 10))
        }
        
        ###
        
          output$tbl1 = renderTable({
              
          YVariable = plotData %>% pull(input$yvar)
          XVariable = plotData %>% pull(input$xvar)
          ColorVariable = plotData %>% pull(input$color)
          FacetVariable = plotData %>% pull(input$facets)
          FacetVariable = drop.levels(FacetVariable)
          ColorVariable = drop.levels(ColorVariable)
          XVariable = drop.levels(XVariable)
          FacetVariable = drop.levels(FacetVariable)
          ColorVariable = drop.levels(ColorVariable)
          
          
          if (input$tests == "ANOVA") {
              
              
              if(input$facets == "None" & input$color != "None" & nlevels(XVariable) == 1)
              {
                  anovatest = aov(YVariable ~ ColorVariable)
              }
              ##????
              
              
              if(input$facets == "None" & input$color == "Round" & nlevels(XVariable) == 1)
              {
                  anovatest = aov(YVariable ~ ColorVariable)
              }
              
            if(input$facets != "None"){ #there are facets
              if(input$color == "None"){ ##input color none
                  if(nlevels(FacetVariable) > 1){ ##facet level 1
                      if(nlevels(XVariable) > 1){
                anovatest = aov(YVariable ~ XVariable + FacetVariable + XVariable*FacetVariable)
                      }
                      else{
                          anovatest = aov(YVariable ~ XVariable + FacetVariable)
                      }
                  } ##facet level 1

                  else{
                      anovatest = aov(YVariable ~ XVariable)
                  } ##facet level 1
              } ##input color none

              else{ #input color not != none
                  if(nlevels(ColorVariable) > 1){
                      if(nlevels(FacetVariable) > 1){
                          if(nlevels(XVariable) > 1){
               anovatest = aov(YVariable ~ XVariable + FacetVariable + ColorVariable +
                                  XVariable*FacetVariable + XVariable*ColorVariable + FacetVariable*ColorVariable)
                          }
                          else{
                              anovatest = aov(YVariable ~ XVariable + FacetVariable + ColorVariable +
                                                  FacetVariable*ColorVariable)
                          }
                      }
                      else{
                          if(nlevels(Xvariable) > 1){
                          anovatest = aov(YVariable ~ XVariable  + ColorVariable +
                                               XVariable*ColorVariable)
                          }
                          else {anovatest = aov(YVariable ~ XVariable  + ColorVariable)

                          }
                      }

                  }

                  else{
                      if(nlevels(FacetVariable) > 1){
                          if(nlevels(XVariable) > 1){
                      anovatest = aov(YVariable ~ XVariable + FacetVariable +
                                          XVariable*FacetVariable)
                          }
                          else{
                              anovatest = aov(YVariable ~ XVariable + FacetVariable)
                          }
                      }

                      else{
                          anovatest = aov(YVariable ~ XVariable)
                      }

                  }
              } #input color not != none

            } ##input boolean facet true

            else{ ##input boolean facet = false
              if(input$color == "None"){
                  if(nlevels(XVariable) == 1){
                anovatest = 0
                  }
                  else {
                      anovatest = aov(YVariable ~ XVariable)
                  }
              }

              else{
                  if(nlevels(ColorVariable) > 1){
                      if(nlevels(XVariable) > 1){
                anovatest = aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable)
                      }
                      else{
                          anovatest = aov(YVariable ~ XVariable + ColorVariable)
                      }
                  }

                  else{
                      anovatest = aov(YVariable ~ XVariable)

                  }
              }
            } ##input boolean facet = false

              check2 = tidy(anovatest)
              ##check2$sumsq = round(check2$sumsq, digits = 2)
             ## check2$meansq = round(check2$meansq, digits = 2)
             ## check2$statistic = round(check2$statistic, digits = 2)
              sum_df = sum(check2$df)
              sum_ss = sum(check2$'sumsq')
              sum_df
              sum_ss
              check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
              ## check2[is.na(check2)] = ""
              for(i in 1:(length(check2$p.value) - 2)){
                  if(check2$p.value[i] >= 0.001){
                      check2$p.value[i] = round(check2$p.value[i], digits = 3)
                  } else{
                      check2$p.value[i] = round(check2$p.value[i], digits = 3)
                      check2$p.value[i] = "<0.001"
                  }
              }
              
              check2
          }
          })
          
          ## fails if color is same as xvar and facets and so on
          output$why = renderPrint({
            YVariable = plotData %>% pull(input$yvar)
            XVariable = plotData %>% pull(input$xvar)
            ColorVariable = plotData %>% pull(input$color)
            FacetVariable = plotData %>% pull(input$facets)
            FacetVariable = drop.levels(FacetVariable)
            ColorVariable = drop.levels(ColorVariable)
            colorlevel = nlevels(ColorVariable)
            facetlevel = nlevels(FacetVariable)
          if (input$tests == "two-sample t-test"){
            if(input$facets == "None" & input$color == "None"){
              dropped = drop.levels(XVariable)
              
              if(nlevels(dropped) == 2) {
               t1 = t.test(YVariable ~ XVariable)
                  tab <- map_df(list(t1), tidy)
                  tab
              }
              else{
                  "Can only have 2 means"
              }
            }
              
           else if(input$xvar == input$color & input$facets == "None") {
               dropped = drop.levels(XVariable)
               
               if(nlevels(dropped) == 2) {
                   t1 = t.test(YVariable ~ XVariable)
                   tab <- map_df(list(t1), tidy)
                   tab
               }
               else{
                   "Can only have 2 means"
               }
               
           }   
              
              else if(input$xvar == input$facets & input$facets != "None" & input$color == "None") {
                  dropped = drop.levels(XVariable)
                  
                  if(nlevels(dropped) == 2) {
                      t1 = t.test(YVariable ~ XVariable)
                      tab <- map_df(list(t1), tidy)
                      tab
                  }
                  else{
                      "Can only have 2 means"
                  }
                  
              }
              
              else if(input$xvar == input$facets & input$xvar == input$color & input$booleanfacet != "None") {
                  dropped = drop.levels(XVariable)
                  
                  if(nlevels(dropped) == 2) {
                     t1 = t.test(YVariable ~ XVariable)
                     tab <- map_df(list(t1), tidy)
                     tab
                  }
                  else{
                      "Can only have 2 means"
                  }
                  
              }
              
              else if(colorlevel < 2 | facetlevel < 2) {
                  dropped = drop.levels(XVariable)
                  
                  if(nlevels(dropped) == 2) {
                      t1 = t.test(YVariable ~ XVariable)
                      tab <- map_df(list(t1), tidy)
                      tab
                  }
                  else{
                      "Can only have 2 means"
                  }
                  
              }
              
              else if(colorlevel < 2 & facetlevel < 2) {
                  dropped = drop.levels(XVariable)
                  
                  if(nlevels(dropped) == 2) {
                     t1 = t.test(YVariable ~ XVariable)
                     tab <- map_df(list(t1), tidy)
                     tab
                  }
                  else{
                      "Can only have 2 means"
                  }
                  
              }
              
            else{
                "Can only have 2 means"
            }
            
          }
          
          })
          
          myplot
          
           
    })
    
    plotData <- reactive({
        if ("all" %in% input$groupID) {filter(data.all, Level %in% input$levels)}
        else{
            if("all" %in% input$playerID)
            {filter(data.all, GroupID %in% input$groupID, Level %in% input$levels)}
            else{filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID, Level %in% input$levels)}
        }
    })     
    
       
   
    
 
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('Data-', Sys.Date(), '.csv', sep="")
        },
        content = function(con) {
            write.csv(plotData(), con)
            
        })
    
    
    
}

shinyApp(ui = ui, server = server)


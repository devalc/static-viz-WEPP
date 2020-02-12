# @ Chinmay Deval
# This is a Shiny web application. You can run the application by clicking

#

library(shiny)
library(tidyverse)
library(shinythemes)
# library(gganimate)
library(plotly)

# Data Preparation Steps

hill <- read.csv("lt2020_2_hill_summary.csv")
data_chn <- read.csv("lt2020_2_chn_summary.csv")
data_wshed <- read.csv("lt2020_2_out_summary.csv")


unique_watsheds <- as.character(unique(hill$Watershed))
unique_scenario <- as.character(unique(hill$Scenario))


ui <- navbarPage("viz-WEPP",
    
    
    ## set the theme
    
    theme = shinytheme(theme = "flatly"),
    
    tabPanel("Hillslope",
             sidebarPanel(
                 # Input: Slider for the number of bins ----
                 selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                             selected = unique_watsheds[1],multiple = F),
                 
                 selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
                             selected = unique_scenario[1],multiple = T),
                 
                 selectInput(inputId="var1",label="Choose Variable",choices =  as.character(unique(colnames(hill)))[8:25],
                             selected = as.character(unique(colnames(hill)))[10],multiple = F)
                 
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
                 
                 fluidRow(
                     column(6, plotOutput("Plot_vs_CumArea")),
                     column(6, plotOutput("Plot_vs_CumArea_abs"))
                 ),
                 fluidRow(
                     column(6, plotOutput("Plot_vs_CumLen")),
                     column(6, plotOutput("Plot_vs_CumLen_abs"))
                 )
             )),
    
    tabPanel("Channel",
             sidebarPanel(
                 # Input: Slider for the number of bins ----
                 selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                             selected = unique_watsheds[1],multiple = F),
                 
                 selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
                             selected = unique_scenario[1],multiple = T),
                 
                 # radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                 
                 selectInput(inputId="chan_var",label="Choose Variable",choices = as.character(unique(colnames(data_chn)))[10:17],
                             selected = as.character(unique(colnames(data_chn)))[10], multiple = F)
                 
                 
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
                 
                 fluidRow(
                     column(6, plotOutput("Plot1")),
                     column(6, plotOutput("Plot2"))
                 ),
                 fluidRow(
                     column(6, plotOutput("Plot3")),
                     column(6, plotOutput("Plot4"))
                 )
             )),
    
    tabPanel("Watershed",
             sidebarPanel(
                 # Input: Slider for the number of bins ----
                 selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                             selected = unique_watsheds[1],multiple = F),
                 
                 # selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
                 #             selected = unique_scenario[1],multiple = T),
                 
                 radioButtons(inputId = "ScenVvar",label = "Select heatmap or specific variable",
                              choices = c("Heatmap"="Heatmap","Pie Chart"="Pie Chart"), selected = "Heatmap")
                 
                 # selectInput(inputId="wshed_var",label="Choose Variable",choices = as.character(unique(colnames(data_wshed)))[4:19],
                 #             selected = as.character(unique(colnames(data_wshed)))[8], multiple = F)
                 
                 
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
                 
                 fluidPage(
                 plotlyOutput("Plot5" ,height = "800px", width ="1200px")
                 # plotOutput("Plot5",height = "800px", width ="800px" )
                     
                 ))
             ),
    
    tabPanel("Spatial-Viz",
             sidebarPanel(
                 # Input: Slider for the number of bins ----
                 selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                             selected = unique_watsheds[1],multiple = F),
                 
                 selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
                             selected = unique_scenario[1],multiple = T),
                 
                 # radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
                 
                 selectInput(inputId="var1",label="Choose Variable",choices = NULL,
                             selected = character(0), multiple = F)
                 
                 
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
                 
                 fluidRow(
                     column(6, plotOutput("Plot9")),
                     column(6, plotOutput("Plot10"))
                 ),
                 fluidRow(
                     column(6, plotOutput("Plot11")),
                     column(6, plotOutput("Plot12"))
                 )
             ))
    
    
    )
                                  
     
           

# Define server logic required to draw a histogram ----
server <- function(input, output){
    

    hill_subset <- reactive({
        dplyr::filter(hill, Watershed %in% input$Watershed) 
    })
    
    chn_subset <- reactive({
        dplyr::filter(data_chn, Watershed %in% input$Watershed) 
    })
    
    # wshed_subet <- reactive({
    #     dplyr::filter(data_wshed, Watershed %in% input$Watershed) 
    # })
    
#################   HILLSLOPE DF #################      
    hill_arr_by_var <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(input$var1, desc) %>%
                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                       cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100,
                       cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                       cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100,
                       cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.)/sum(Soil.Loss..kg.ha.)*100,
                       cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.)/sum(Sediment.Deposition..kg.ha.)*100,
                       cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.)/sum(Sediment.Yield..kg.ha.)*100,
                       cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.)/sum(Solub..React..P..kg.ha.3.)*100,
                       cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.)/sum(Particulate.P..kg.ha.3.)*100,
                       cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.)/sum(Total.P..kg.ha.3.)*100,
                       cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction)/sum(Particle.Class.1.Fraction)*100,
                       cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction)/sum(Particle.Class.2.Fraction)*100,
                       cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction)/sum(Particle.Class.3.Fraction)*100,
                       cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction)/sum(Particle.Class.4.Fraction)*100,
                       cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction)/sum(Particle.Class.5.Fraction)*100,
                       cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm)/sum(Particle.Fraction.Under.0.016.mm)*100,
                       cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)/sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)*100
                ) %>%
                ungroup()})
    
#################   CHANNEL DF #################  
    
    chn_arr_by_var <- reactive({
        chn_subset() %>% group_by(Scenario) %>% arrange_at(input$chan_var, desc) %>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100,
                   cumDischarge.mm = cumsum(Discharge..mm.)/sum(Discharge..mm.)*100,
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.)/sum(Sediment.Yield..tonne.)*100,
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.)/sum(Channel.Erosion..tonne.)*100,
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.)/sum(Upland.Charge..mm.)*100,
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.)/sum(Solub..React..P..kg.ha.)*100,
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.)/sum(Particulate.P..kg.ha.)*100,
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)/sum(Total.P..kg.ha.)*100) %>%
            ungroup()})

#################   CHANNEL PLOTS #################     

    output$Plot5 <- renderPlotly({
        
        
        wshed_subet <- dplyr::filter(data_wshed, Watershed %in% input$Watershed)
        
        if (input$ScenVvar == "Heatmap") {
        d <-  wshed_subet[,c(2,7:20)] %>% dplyr::mutate_if(is.numeric, scale)
        
        d.m <- reshape2::melt(d)
        
        
        a<-ggplot(d.m, aes(Scenario, variable,  fill= value)) + 
            geom_tile(inherit.aes = TRUE)  +
            scale_fill_distiller(palette = "Spectral") + 
            theme(
                axis.text.x = element_text(angle = 90,colour = "Black", size = 12, face = "bold"),
                axis.text.y = element_text(colour = "Black", size = 12, face = 'bold'),
                axis.title = element_blank()
                
            )
        a}else
            if (input$ScenVvar == "Pie Chart") {
                
                d <-  wshed_subet[,c(2,7:20)] 
                
                d.m <- reshape2::melt(d)
                
                d.m <- d.m %>%
                    group_by(variable) %>%
                    mutate(total = sum(value),
                           share = (value/total)*100) %>%
                    ungroup() 
                
                ggplot(d.m) +
                    facet_wrap(~variable) +
                    geom_bar(aes(y = share, x = "", fill = Scenario), stat = "identity") +
                    theme(
                        axis.title = element_blank() 
                    ) +
                    coord_polar("y", start = 0) 
                    
                
                }
        # plotly::ggplotly(a)
    })
        
        
#################   CHANNEL PLOTS #################      
    output$Plot1 <- renderPlot({
        p1 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercContriChanArea))
        if(input$chan_var ==  "Discharge..mm."){
            p1 <- p1 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=1)}else
                if(input$chan_var ==  "Sediment.Yield..tonne."){
                    p1 <- p1 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=1)}else
                        if(input$chan_var ==  "Channel.Erosion..tonne."){
                            p1 <- p1 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=1)}else
                                if(input$chan_var ==  "Upland.Charge..mm."){
                                    p1 <- p1 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=1)}else
                                        if(input$chan_var ==  "Lateral.Flow..mm."){
                                            p1 <- p1 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=1)}else
                                                if(input$chan_var ==  "Solub..React..P..kg.ha."){
                                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=1)}else
                                                        if(input$chan_var ==  "Particulate.P..kg.ha."){
                                                            p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=1)}else
                                                                if(input$chan_var ==  "Total.P..kg.ha."){
                                                                    p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=1)}
                
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total contributing channel area",y=paste("Percent of total ", input$chan_var, sep = " "), title="",colour="Scenario")
        
        
        
        p1

        })
    
    output$Plot2 <- renderPlot({
        p1 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercChanArea))
        if(input$chan_var ==  "Discharge..mm."){
            p1 <- p1 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=1)}else
                if(input$chan_var ==  "Sediment.Yield..tonne."){
                    p1 <- p1 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=1)}else
                        if(input$chan_var ==  "Channel.Erosion..tonne."){
                            p1 <- p1 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=1)}else
                                if(input$chan_var ==  "Upland.Charge..mm."){
                                    p1 <- p1 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=1)}else
                                        if(input$chan_var ==  "Lateral.Flow..mm."){
                                            p1 <- p1 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=1)}else
                                                if(input$chan_var ==  "Solub..React..P..kg.ha."){
                                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=1)}else
                                                        if(input$chan_var ==  "Particulate.P..kg.ha."){
                                                            p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=1)}else
                                                                if(input$chan_var ==  "Total.P..kg.ha."){
                                                                    p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=1)}
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel area",y=paste("Percent of total ", input$chan_var, sep = " "), title="",colour="Scenario")
        
        
        
        p1
        
    })
    
    
    output$Plot3 <- renderPlot({
        p1 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercLen))
        if(input$chan_var ==  "Discharge..mm."){
            p1 <- p1 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=1)}else
                if(input$chan_var ==  "Sediment.Yield..tonne."){
                    p1 <- p1 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=1)}else
                        if(input$chan_var ==  "Channel.Erosion..tonne."){
                            p1 <- p1 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=1)}else
                                if(input$chan_var ==  "Upland.Charge..mm."){
                                    p1 <- p1 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=1)}else
                                        if(input$chan_var ==  "Lateral.Flow..mm."){
                                            p1 <- p1 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=1)}else
                                                if(input$chan_var ==  "Solub..React..P..kg.ha."){
                                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=1)}else
                                                        if(input$chan_var ==  "Particulate.P..kg.ha."){
                                                            p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=1)}else
                                                                if(input$chan_var ==  "Total.P..kg.ha."){
                                                                    p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=1)}
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total length",y=paste("Percent of total ", input$chan_var, sep = " "), title="",colour="Scenario")
        
        
        
        p1
        
    })
    
#################   HILLSLOPE PLOTS #################       
    output$Plot_vs_CumArea <- renderPlot({

        p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercArea))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm  , color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=1)
                                                                    }


        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"),
                  legend.position = "bottom")+
            labs(x="Percent Area",y=paste("Percent of total", input$var1, sep = " "), title="",colour="Scenario")



        p1

    })

   
    output$Plot_vs_CumArea_abs <- renderPlot({
        
        p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercArea))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumsum(Runoff..mm.) , color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumsum(Lateral.Flow..mm.), color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumsum(Baseflow..mm.), color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumsum(Soil.Loss..kg.ha.), color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Deposition..kg.ha.), color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Yield..kg.ha.), color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumsum(Solub..React..P..kg.ha.3.), color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumsum(Particulate.P..kg.ha.3.), color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumsum(Total.P..kg.ha.3.), color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.1.Fraction), color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.2.Fraction), color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.3.Fraction), color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.4.Fraction), color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.5.Fraction), color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumsum(Particle.Fraction.Under.0.016.mm), color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) , color= Scenario),size=1)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"))+
            labs(x="Percent of total hillslope area",y=input$var1,title="",colour="Scenario") 
        
        p1
        
    })
    
    
    output$Plot_vs_CumLen <- renderPlot({

        p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercLen ))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm, color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha., color= Scenario),size=1)
                                                                    }


        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="BLACK",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel length",y= paste("Percent of total",input$var1, sep = " "),title="",colour="Scenario")

        p1

    })
    
    output$Plot_vs_CumLen_abs <- renderPlot({
        
        p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercLen))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumsum(Runoff..mm.) , color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumsum(Lateral.Flow..mm.), color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumsum(Baseflow..mm.), color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumsum(Soil.Loss..kg.ha.), color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Deposition..kg.ha.), color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Yield..kg.ha.), color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumsum(Solub..React..P..kg.ha.3.), color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumsum(Particulate.P..kg.ha.3.), color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumsum(Total.P..kg.ha.3.), color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.1.Fraction), color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.2.Fraction), color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.3.Fraction), color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.4.Fraction), color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumsum(Particle.Class.5.Fraction), color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumsum(Particle.Fraction.Under.0.016.mm), color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) , color= Scenario),size=1)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"),
                  legend.title = element_text(size=14,color="BLACK",face="bold"),
                  legend.text = element_text(size=14,color="BLACK"))+
            labs(x="Percent of total channel length",y=input$var1,title="",colour="Scenario") 
        
        p1
        
    })
    
    
    # output$Plot2 <- renderImage({
    # 
    #     outfile <- tempfile(fileext='.gif')
    # 
    #     p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercLen )) +  theme_bw()+
    #         theme(axis.title = element_text(size=14,color="BLACK",face="bold"),
    #               axis.text = element_text(size=14,color="BLACK",face="bold"),
    #               legend.title = element_text(size=14,color="BLACK",face="bold"),
    #               legend.text = element_text(size=14,color="BLACK"))+
    #         labs(x="Percent Channel Length",y=input$var1,title="",colour="Scenario")
    #     if(input$var1 == "Runoff..mm."){
    #         p1 <- p1 + geom_line(aes(y=cumRunoff.mm, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #     }else
    #         if(input$var1 == "Lateral.Flow..mm."){
    #             p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #         }else
    #             if(input$var1 == "Baseflow..mm."){
    #                 p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #             }else
    #                 if(input$var1 == "Soil.Loss..kg.ha."){
    #                     p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                 }else
    #                     if(input$var1 == "Sediment.Deposition..kg.ha."){
    #                         p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                     }else
    #                         if(input$var1 == "Sediment.Yield..kg.ha."){
    #                             p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                         }else
    #                             if(input$var1 == "Solub..React..P..kg.ha.3."){
    #                                 p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                             }else
    #                                 if(input$var1 == "Particulate.P..kg.ha.3."){
    #                                     p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                 }else
    #                                     if(input$var1 == "Total.P..kg.ha.3."){
    #                                         p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                     }else
    #                                         if(input$var1 == "Particle.Class.1.Fraction"){
    #                                             p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                         }else
    #                                             if(input$var1 == "Particle.Class.2.Fraction"){
    #                                                 p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                             }else
    #                                                 if(input$var1 == "Particle.Class.3.Fraction"){
    #                                                     p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                                 }else
    #                                                     if(input$var1 == "Particle.Class.4.Fraction"){
    #                                                         p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                                     }else
    #                                                         if(input$var1 == "Particle.Class.5.Fraction"){
    #                                                             p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                                         }else
    #                                                             if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
    #                                                                 p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                                             }else
    #                                                                 if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
    #                                                                     p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha., color= Scenario),size=1)+ transition_reveal(cumPercLen)
    #                                                                 }
    #     anim_save("outfile.gif", animate(p1)) # New
    # 
    #     # Return a list containing the filename
    #     list(src = "outfile.gif",
    #          contentType = 'image/gif',
    #          width = 1000,
    #          height = 1000
    #          # alt = "This is alternate text"
    #     )}, deleteFile = TRUE)
    
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
#rm(list=ls())

library(tidyverse)
library(ggrepel)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)
library(cowplot)
library(htmlwidgets)

# Upload everything to global environment 
# setwd("C:/Users/efons/gis_plots")
data <- read.csv("http://scvurppp.org/gsi/scc_gsi.csv") %>% 
  filter(include=="Yes") %>% 
  mutate(fy = factor(fy, levels=c("FY02-03", "FY03-04", "FY04-05", "FY05-06", "FY06-07", "FY07-08", "FY08-09",
                                   "FY09-10", "FY10-11", "FY11-12", "FY12-13", "FY13-14", "FY14-15", "FY15-16", "FY16-17", "FY17-18",
                                  "FY18-19", "FY19-20", "FY20-21", "FY21-22"))) %>%  # can be automated
  filter(!is.na(fy))

# acre type in rows, not columns
# maybe there's an easier way, but i dont see how to use it in ggplot otherwise
data_acr <- rbind(data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Industrial",
                             ac =  data$oi_acres,
                             trmnt_cat = data$trmnt_cat),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Urban Residential",
                             ac =  data$our_acres,
                             trmnt_cat = data$trmnt_cat),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Urban Commercial",
                             ac =  data$ouc_acres,
                             trmnt_cat = data$trmnt_cat),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "New Urban",
                             ac =  data$nu_acres,
                             trmnt_cat = data$trmnt_cat),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Other",
                             ac =  data$otheracres,
                             trmnt_cat = data$trmnt_cat),
                  data.frame(project_id = data$project_id,  
                             permittee= data$permittee,
                             ac_type = "Open Space",
                             ac =  data$os_acres,
                             trmnt_cat = data$trmnt_cat)
                  
)

# Parameters used in ui: 
vars_permittees <- as.character(sort(unique(data$permittee)))
vars_trmnt_cat <- as.character(sort(unique(data$trmnt_cat)))

colors_projType <- c("lightblue", "salmon", "purple")
names(colors_projType) <- levels(data$projtype)

colors_acrType <- brewer_pal(pal="Set3")(6)[c(4,2,5,3,6,1)]
names(colors_acrType) <- levels(data_acr$ac_type)

# The App 
shinyApp(
  
  
  
  # user interface 
  ui <- fluidPage(
    # a simple user interface - can also use shinydashboard 
    

          
    # choose permittee
    fluidRow(
      column(3,pickerInput(inputId="permittee", label= "Choose Permittee",
                      choices= vars_permittees,
                      selected= vars_permittees,
                      multiple=T,
                      options = pickerOptions(actionsBox=T,liveSearch = F)
    )), 
    
    column(3,pickerInput(inputId="trmnt_cat", label= "Treatment Categories",
                choices= vars_trmnt_cat,
                selected= vars_trmnt_cat,
                multiple=T,
                options = pickerOptions(actionsBox=T,liveSearch = F)
    ))), 
    
    
    # Bar plot total acres vs. FY 
  
    plotOutput("bar_totAcr"),
    
    br(),
    br(),
    
    
    # The 2 pie charts 
  
    fluidRow(column(6,plotOutput("pie_projType")),
    column(6,plotOutput("pie_acrType"))),
    actionLink(inputId="cat_definitions", label="Definitions", icon=icon("question-circle"),
               style="color: #007bff")
    ),
  
  
  
  
  # Server function 
  server <- function(input,output){
    
    
    # reactive subsetting when user changes inputs 
    data_sub_1 <- reactive({
      data %>% 
        filter(permittee %in% input$permittee,
               trmnt_cat %in% input$trmnt_cat)
    })
    
    data_sub_2 <- reactive({
      data_acr %>% 
        filter(permittee %in% input$permittee, 
               trmnt_cat %in% input$trmnt_cat)
    })
    
    
    
    # render plots - plots can be improved. 
    # Need to add a condition when zero permittee selected 
    
    # bar plot
    output$bar_totAcr <- renderPlot({
      data_sub <- data_sub_1()
      if (nrow(data_sub)>0){
        
        
      # make stacked barplot : Constructed / Under Construction   
      p <- ggplot(data=data_sub, aes(x=fy,y=tot_acres, fill=status)) + theme_bw()+
          geom_bar(stat="identity", width=0.70)+ 
          scale_x_discrete("Fiscal Year", drop=F) + 
          scale_fill_manual(values=c("Constructed"="#007bff", "Under Construction"="grey")) + 
         ylab("Total Acres") + 
        theme(text = element_text(size=14),
              axis.text.x = element_text(angle=90),
              plot.title = element_text(face = 'bold', size= 16),
              axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
              legend.position = "none")  + 
        ggtitle("Green Stormwater Infrastructure projects by year")
        
      
      if (max((data_sub%>% 
               dplyr::filter(status=="Constructed") %>% 
               dplyr::group_by(fy)%>% 
               dplyr::summarise(sum=sum(tot_acres)))$sum)>0) {
        p<- p + annotate("text", x=length(levels(data$fy))-1, y=max((data_sub%>% 
                                                                dplyr::filter(status=="Constructed") %>% 
                                                                dplyr::group_by(fy)%>% 
                                                                dplyr::summarise(sum=sum(tot_acres)))$sum)*1.3, label="Under\nConstruction",
                         col="grey40", fontface=2)
      }
      
          
      return(p)
          
      }
    })
    
    
    
    
   
    # pie charts
    output$pie_projType <- renderPlot({
      
      data_sub <- data_sub_1() 

     # condition to avoid error message
        # data grouping makes it easier to print labels on pie chart. Maybe there's another way.
        data_pie_proj <- data_sub %>% 
        group_by(projtype) %>% 
        summarise(n_projects=n(),
                  ac_projtype = sum(tot_acres)) 
      
      if(nrow(data_pie_proj)>0){
         ggplot(data=data_pie_proj, aes(x='', y=ac_projtype, fill=projtype)) + geom_bar(stat="identity") + theme_bw()+
        coord_polar("y", start=0) + # make the bar chart a pie chart 
        theme(text= element_text(size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          legend.position = "right",
          legend.title = element_text(face="bold"),
          plot.title = element_text(face = 'bold', size= 16)
        ) + 
        geom_text_repel(aes(label=signif(ac_projtype,2)), position=position_stack(vjust=0.5), direction="x") + # add labels
        guides(fill=guide_legend(title="Project Type (Acres):")) + 
        scale_fill_manual(values=colors_projType) +  # colors remain constnt when inputs change
        ggtitle("Project Type")  
      }
       
      
      
    })
    
    
    output$pie_acrType <- renderPlot({
      data_sub_2 <- data_sub_2()
      
      
      
      data_acr_pie <- data_sub_2 %>% 
        group_by(ac_type) %>% 
        summarise(sum_ac=sum(ac)) %>% 
        filter(sum_ac >0)
      
      if (nrow(data_acr_pie)>0){
        
        ggplot(data=data_acr_pie, aes(x='', y=sum_ac, fill=ac_type)) + geom_bar(stat="identity") + theme_bw()+
          coord_polar("y", start=0) + 
          theme(text= element_text(size=14),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(), 
                legend.position = "right",
                legend.title = element_text(face="bold"),
                plot.title = element_text(face = 'bold', size= 16)
          ) + 
          geom_text_repel(aes(label=signif(sum_ac,2)), position=position_stack(vjust=0.5), direction="x") + 
          guides(fill=guide_legend(title="Land Use (Acres):")) + 
          scale_fill_manual(values=colors_acrType) +
          ggtitle("Previous Land Use Category")
        

        
      }
      
    })
    
    
    observeEvent(input$cat_definitions,{
      showModal(modalDialog(
        title = "Definitions",
        HTML("Definition of project types: 
             <ul><li> Green Street: Green Stormwater Infrastructure located in the public right-of-way. </li>
             <li> Parcel-based: Green Stormwater Infrastructure on a public or private parcel. </li>
             <li> Regional Facility: A public or private stormwater treatment or hydromodification control facility that
collects runoff from a large area or from multiple development projects. </li></ul>
            
             <br/> 
             <br/>
             
             Definition of land use categories: 
             <ul><li> Old Industrial: Developed as an industrial land use before 1980. </li>
             <li> Old Urban Residential: Developed as a residential or parks land use before 1980. </li>
             <li> Old Urban Commercial: Developed before 1980 as any land use other than industrial, residential, or parks. </li>
             <li> New Urban: Developed or redeveloped after 1979. </li>
             <li> Open Space: Area that is not developed or mostly pervious including agriculture, vacant lots, large urban parks, stream channels, golf courses, and cemeteries. </li></ul>
"
        )
        ,
        easyClose = TRUE, footer=modalButton("Got it!")
        ))
    })

    
  }
  )
  


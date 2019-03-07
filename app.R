#hi:()

library(tidyverse)
library(ggrepel)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)

# Upload everything to global environment 
# setwd("C:/Users/efons/Desktop/GSI website")
data <- read.csv("http://eoainc.org/scc_gsi.csv") %>% 
  filter(include=="Yes", 
         !fy=="FY01-02")

# acre type in rows, not columns
# maybe there's an easier way, but i dont see how to use it in ggplot otherwise
data_acr <- rbind(data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Industrial",
                             ac =  data$oi_acres),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Urban Residential",
                             ac =  data$our_acres),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Old Urban Commercial",
                             ac =  data$ouc_acres),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "New Urban",
                             ac =  data$nu_acres),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Other",
                             ac =  data$otheracres),
                  data.frame(project_id = data$project_id, 
                             permittee= data$permittee,
                             ac_type = "Open Space",
                             ac =  data$os_acres)
                  
)

# Parameters used in ui: 
vars_permittees <- as.character(sort(unique(data$permittee)))

colors_projType <- c("lightblue", "salmon", "purple")
names(colors_projType) <- levels(data$projtype)

colors_acrType <- brewer_pal(pal="Set3")(6)[c(4,2,5,3,1,6)]
names(colors_acrType) <- levels(data_acr$ac_type)

# The App 
shinyApp(
  
  
  
  # user interface 
  ui <- fluidPage(
    # a simple user interface - can also use shinydashboard 
    title="gsi",
          
    # choose permittee
    pickerInput(inputId="permittee", label= "Choose Permittee",
                      choices= vars_permittees,
                      selected= vars_permittees,
                      multiple=T,
                      options = pickerOptions(actionsBox=T,liveSearch = T)
    ), 
    
    # Bar plot total acres vs. FY 
    plotOutput("bar_totAcr"),
    
    # The 2 pie charts 
    column(6,plotOutput("pie_projType")),
    column(6,plotOutput("pie_acrType"))

    ),
  
  
  
  
  # Server function 
  server <- function(input,output){
    
    
    # reactive subsetting when user changes inputs 
    data_sub_1 <- reactive({
      data %>% 
        filter(permittee %in% input$permittee)
    })
    
    data_sub_2 <- reactive({
      data_acr %>% 
        filter(permittee %in% input$permittee)
    })
    
    
    
    # render plots - plots can be improved. 
    # Need to add a condition when zero permittee selected 
    
    # bar plot
    output$bar_totAcr <- renderPlot({
      data_sub <- data_sub_1()
      if (nrow(data_sub)>0){
        
      ggplot(data=data_sub, aes(x=fy,y=tot_acres)) + geom_bar(stat="identity", fill="#007bff") + 
        theme(axis.text.x = element_text(angle=90),
              plot.title = element_text(face = 'bold', size= "14")) + 
        xlab("Fiscal Year") + ylab("Total Acres") + 
          ggtitle("Green Stormwater Infrastructure projects by year") 

      }
    })
    
    # pie 1 
    output$pie_projType <- renderPlot({
      data_sub <- data_sub_1() 
      if (nrow(data_sub)>0){ # condition to avoid error message
        # data grouping makes it easier to print labels on pie chart. Maybe there's another way.
        data_pie_proj <- data_sub %>% 
        group_by(projtype) %>% 
        summarise(n_projects=n(),
                  ac_projtype = sum(tot_acres)) 
      
      ggplot(data=data_pie_proj, aes(x='', y=ac_projtype, fill=projtype)) + geom_bar(stat="identity") + 
        coord_polar("y", start=0) + # make the bar chart a pie chart 
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          legend.position = "right",
          legend.title = element_text(face="bold"),
          plot.title = element_text(face = 'bold', size= "14")
        ) + 
        geom_text_repel(aes(label=signif(ac_projtype,2)), position=position_stack(vjust=0.5), direction="x") + # add labels
        guides(fill=guide_legend(title="Project Type (in Acres):")) + 
        scale_fill_manual(values=colors_projType) + # colors remain constnt when inputs change
        ggtitle("Project Type")
      
        }
      
      
    })
    
    # pie 2 
    output$pie_acrType <- renderPlot({
      data_sub <- data_sub_2()
      if (nrow(data_sub)>0){
        
      data_acr_pie <- data_sub %>% 
        group_by(ac_type) %>% 
        summarise(sum_ac=sum(ac)) %>% 
        filter(sum_ac >0)
    
      if (nrow(data_acr_pie)>0){
     
      ggplot(data=data_acr_pie, aes(x='', y=sum_ac, fill=ac_type)) + geom_bar(stat="identity") + 
        coord_polar("y", start=0) + 
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          legend.position = "right",
          legend.title = element_text(face="bold"),
          plot.title = element_text(face = 'bold', size= "14")
        ) + 
        geom_text_repel(aes(label=signif(sum_ac,2)), position=position_stack(vjust=0.5), direction="x") + 
        guides(fill=guide_legend(title="Land Use Category (in Acres):")) + 
        scale_fill_manual(values=colors_acrType) + 
        ggtitle("Previous land use category") 

      }}
    })
    
  }
  )
  


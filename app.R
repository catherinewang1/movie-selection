#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tuneR)
library(beepr)
library(png)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Movie Selection!!!"),

    # Inputs 
    sidebarLayout(
        sidebarPanel(
            textInput("names_str",
                      "Names",
                      'Alice, Bob, Charles'),
            numericInput("seed",
                      "Random Seed",
                       1234567),
            actionButton('clear', 
                         'Clear',
                         style="color: #fff; background-color: #696969; border-color: #000000"),
            actionButton('initialize',
                         'Initialize',
                         style="color: #fff; background-color: #337ab7; border-color: #000000"
                         ),
            actionButton('eliminate',
                         'ELIMINATE',
                         style="color: #fff; background-color: #DC143C; border-color: #000000"
                        )
        ),
        
        # Show the eliminated names so far
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required 
server <- function(input, output) {
    mypng <- readPNG('www/download-confetti-free-png-transparent-image-and-clipart-confetti-falling-png-400_208.png')
    v = reactiveValues(df = NULL, p = NULL)
    delta = .05
    #clear the plot shown 
    observeEvent(input$clear, {
        v$p = NULL
    })
    
    #initialize the plot with current names and random seed
    observeEvent(input$initialize, {
            #insertUI(selector = "#eliminate",
            #         where = "afterEnd",
            #         ui = tags$audio(src = "Mario Kart Race Start - Gaming Sound Effect (HD).mp3", type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")
            #)
            #set random seed
            isolate(set.seed(input$seed))
            #parse names input
            names = isolate(unique(strsplit(gsub(" ", "", input$names_str, fixed = TRUE), ',')[[1]]))
            
            #create dataframe for plotting
            newdf = data.frame(names) #df with status of each name
            newdf$yloc = sample(1:nrow(newdf)) #randomized position on plot
            newdf$eliminated = 0 #no one eliminated yet
            v$df = newdf
            
            #visualize
            
            newp = ggplot(newdf) + xlim(0, 1) + ylim(0, nrow(newdf)) +
                geom_rect(aes(xmin = 0, xmax = 1, ymin = yloc - 1 + delta, ymax = yloc - delta, fill = names),
                          alpha = .5, color = 'black') +
                geom_text(aes(x = .5, y = yloc - .5, label = names),
                          size = 40 / nrow(newdf)) +
                scale_fill_brewer(palette="Set3") +
                theme_classic() +
                theme(axis.ticks = element_blank(),
                      legend.position = 'none',
                      axis.title = element_blank(),
                      axis.line = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank())
            v$p = newp
    })
    
    # eliminate a name
    observeEvent(input$eliminate, {
            
            df = v$df
            if(nrow(df) - sum(df$eliminated) == 0) {
                # no more possible
                return()
            } 
            
            #eliminate 1 more
            to_eliminate = which(df$eliminated == 0) %>% sample(1)
            df[to_eliminate, 'eliminated'] = 1
            newp = v$p
            
            #visualize
            newp = newp + geom_rect(aes(xmin = 0, xmax = 1,
                                  ymin = df[to_eliminate, 'yloc'] - 1 + delta,
                                  ymax = df[to_eliminate, 'yloc'] - delta), 
                              size = 8/nrow(df), color = '#DC143C', alpha = 0)
            v$p = newp
            newp = newp + geom_segment(aes(x = 0, y = df[to_eliminate, 'yloc'] - 1 + delta,
                                     xend = 1, yend = df[to_eliminate, 'yloc'] - delta),
                                 size = 8/nrow(df), color = '#DC143C') +
                geom_segment(aes(x = 0, y = df[to_eliminate, 'yloc'] - delta),
                             xend = 1, yend = df[to_eliminate, 'yloc'] - 1 + delta,
                             size = 8/nrow(df), color = '#DC143C')
            v$df = df
            v$p = newp
            
            if (nrow(df) - sum(df$eliminated) == 1) {
                # one more possible
                
                winner_idx = which(df$eliminated == 0)
                df[winner_idx, 'eliminated'] = 1
                
                #make a sound
                if(grepl('holly', tolower(df[winner_idx, 'names']), fixed = TRUE)) {
                    holly_rigged_sounds = c('ILLUMINATI CONFIRMED - MLG Sound Effects (HD).mp3' )
                    insertUI(selector = "#eliminate",
                             where = "afterEnd",
                             ui = tags$audio(src = holly_rigged_sounds[1], type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")  
                    )
                } else {
                    winner_sounds = c("smb_stage_clear.wav",
                                      'victory_fanfare_mono.wav')
                    insertUI(selector = "#eliminate",
                             where = "afterEnd",
                             ui = tags$audio(src = winner_sounds[1], type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")  
                    )
                }
                newp = v$p
                newp = newp + geom_rect(aes(xmin = 0, xmax = 1,
                                            ymin = df[winner_idx, 'yloc'] - 1 + delta,
                                            ymax = df[winner_idx, 'yloc'] - delta), 
                                        size = 8/nrow(df), color = '#00FA9A', alpha = 0) +
                        annotation_raster(mypng, 
                                      ymin = df[winner_idx, 'yloc'] - 1 + delta,
                                      ymax= df[winner_idx, 'yloc'] - delta,
                                      xmin = 0, xmax = 1)
                v$df = df
                v$p = newp
                return()
            } 
            eliminated_sound = c(#"Nope (Construction Worker TF2) - Gaming Sound Effect (HD).mp3",
                                 "wrong-answer-sound-effect_80.mp3",
                                 #'Denied - Gaming Sound Effect (HD).mp3',
                                 'Sad Trombone - Gaming Sound Effect (HD).mp3')
            insertUI(selector = "#eliminate",
                     where = "afterEnd",
                     ui = tags$audio(src = eliminated_sound[sample(length(eliminated_sound), 1)], type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
            )
    })
    
    #plot visualization of names left
    output$plot = renderPlot({v$p})

}

# Run the application 
shinyApp(ui = ui, server = server)

# Sound Effects
 
# eliminated: family feud, sad trombone
# wins: 
# Holly wins:

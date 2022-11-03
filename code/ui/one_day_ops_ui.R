# tabPanel("One Day"
# 
# ) # end of tab panel

tabPanel("Local Ops",
         fluidRow( # major row that contains whole body
           column( # major column that contains whole body
             width = 12,
             #
             # now add the content
             column(  # this is the 1st main column - with the graphs
               width = 6,
               fluidRow( # row with Potomac flow graph
                 box(
                   title = "Little Falls flows, observed and forecasted 
                   - daily data",
                   width = NULL,
                   plotOutput("one_day_ops_plot1", height = "300px")
                 )
               ),
               fluidRow( # row with optional other graphs
                 # h3(),
                 column(
                   width = 12,
                   box(
                     title = "Little Falls flows, observed and forecasted 
                     - hourly data",
                     width = NULL,
                     plotOutput("one_day_ops_plot2", height = "300px")
                   )
                 )
               ) # end of 2nd fluid row
               ), # end of 1st main column - with graphs
             column( # this is the 2nd main column - with values & triggers
               width = 6, 
               box(title = "Today's need from Patuxent load shift", 
                   width = NULL,
                   height=40),
               box(title = "  - based on PRRISM algorithm",
                   width = NULL,
                   height=40),
               valueBoxOutput("lfalls_0day_fc1", width = NULL),
               valueBoxOutput("lfalls_0day_deficit1", width = NULL),
               box(title = "  - based on LFFS",
                   width = NULL,
                   height=40),
               valueBoxOutput("lfalls_0day_fc2", width = NULL),
               valueBoxOutput("lfalls_0day_deficit2", width = NULL),
               br(),
               box(title = "Tomorrow's need from combined L Seneca release
                   and Occoquan load shift", 
                   width = NULL,
                   height=60),
               box(title = "  - based on PRRISM algorithm",
                   width = NULL,
                   height=40),
               valueBoxOutput("lfalls_1day_fc1", width = NULL),
               valueBoxOutput("lfalls_1day_deficit1", width = NULL),
               box(title = "  - based on LFFS",
                   width = NULL,
                   height=40),
               valueBoxOutput("lfalls_1day_fc2", width = NULL),
               valueBoxOutput("lfalls_1day_deficit2", width = NULL)
               ) # end of 2nd main column
           ) # end of major column that contains whole body
         ) # end of major row that contains whole body

) # end of tab panel
# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Displays graphs and values on 10-Day Ops tab
# *****************************************************************************
# INPUTS
# *****************************************************************************
# Content created in code/server/ten_day_ops/ten_day_ops_server.R
# *****************************************************************************

tabPanel("QA",
         fluidRow( # major row that contains whole body
           column( # major column that contains whole body
             width = 12,
             #
             # now add the content
             fluidRow( # row with graph on LHS and value boxes on RHS
               column(  # first column of first row - with the graphs
                 width = 8,
                 box(
                   title = "Little Falls flows - observed and forecasted",
                   width = NULL,
                   plotOutput("lffs_qa_plot", height = plot.height, 
                              width = plot.width)
                 # plotOutput("potomacFlows", height = "220px")
                 ) # end of box with graph
                 ), # end of column with graph
               column( # second column of first row - with some values
                 width = 4,
                 # some general information
                 valueBoxOutput("lffs_today", width = NULL) #,
                 ) # end of column with values
             ), # end of first row

               fluidRow( # full-length row with stats tables
                 h3("LFFS, baseflow-corrected - long-term stats"),
                   box(
                     title = "Forecast stats",
                     width = NULL,
                     tableOutput("lffs_bfc_stats")
                   ),
                   box(
                     title = "Low-flow forecast stats",
                     width = NULL,
                     tableOutput("lffs_bfc_lf_stats"),
                   ),
                 h3("LFFS - long-term stats"),
                 box(
                   title = "Forecast stats",
                   width = NULL,
                   tableOutput("lffs_stats")
                 ),
                 box(
                   title = "Low-flow forecast stats",
                   width = NULL,
                   tableOutput("lffs_lf_stats"),
                 )
                 ) # end of second fluid row
           ) # end of major column that contains whole body
         ) # end of major row that contains whole body

) # end of tab panel
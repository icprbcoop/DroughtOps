tabPanel("Situational Awareness",
  fluidRow( # r1: major row that contains whole body
    column( # c1: major column that contains whole body
      width = 12,
      # now add the content
      column(  # c2: this is the 1st main column - with the graphs
        width = 6,
        fluidRow( # row with Potomac flow graph
          box(
            title = NULL,
            width = NULL,
            plotOutput("sit_aware_flows_plot", height = "400px")
          )
        ),
        fluidRow( # row with 2 reservoir graphs
          h3("Reservoir storage (billion gallons)"),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("sit_aware_jrr_stor", height = "190px")
            )
          ),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("sit_aware_sen_stor", height = "190px")
            )
          )
        ), # end row with jrr and sen graphs
        fluidRow(
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("sit_aware_occ_stor", height = "190px")
            )
          ),
          column(
            width = 6,
            box(
              title = NULL,
              width = NULL,
              plotOutput("sit_aware_pat_stor", height = "190px")
            )
          )
        ) # end row with sen and pat graphs 
        #                ) # end of row with all 4 reservoir graphs
      ), # end of c2 - 1st main column - with graphs
      
      # Value boxes that appear on the right ----------------------------------
      column( # c3: this is the 2nd main column - with values & triggers
        width = 6,
        h3("Potomac River flow"),
        valueBoxOutput("lfalls_flow_yesterday_text", width = NULL),
        valueBoxOutput("lfalls_flow_today_text", width = NULL),
        valueBoxOutput("por_flow_yesterday_text", width = NULL),
        valueBoxOutput("por_flow_today_text", width = NULL),
        valueBoxOutput("lfalls_adj_yesterday_text", width = NULL),
        h3("Status and Stages"),
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "coop_ops")
        ),
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "lfaa_alert")
        ),
        box(
          title=NULL,
          width=NULL,
          height=50,
          htmlOutput(outputId = "mwcog_stage")
        ) ,

        #these two boxes are for outputting the Maryland drought map
        #and Virginia drought squares

        #----------------------beginning of Maryland section-----------------------------------
        box(
          title = NULL,#"MARYLAND DROUGHT STATUS",
          width = NULL,#6,
          height = 260,#220,
          htmlOutput(outputId="MD_title"),
          box(
            tags$img(alt="Drought Status Map:2019-05-31",
                       src= md_drought_map,
                       style="width:250px;height:150px;border:0;")
            #leafletOutput("mymap", height =140, width =300)
            )
          ), #END OF MARYLAND DROUGHT UI
        #tags$p("Western region: Drought Watch; Central region: Drought Warning")),
        #-------------------------end of Maryland section---------------------------------------


        #------------------------beginning of Virginia section----------------------------------
        box(
          title = NULL,#"VIRGINIA DROUGHT STATUS",
          width = NULL,#6,
          height = 260,#220
          htmlOutput(outputId="VA_title"),

          box(
            tags$img(alt= "virginia map placeholder",#"test_VA_droughtmap_temp.png",#"Drought Status Map:2019-05-31",
                     src= va_drought_map,
                     # used for shinyapp.io published version #src = "publishable_VA_droughtmap_temp.png",#
                     style="width:200px;height:150px;border:0;")
          )

          # htmlOutput(outputId = "boxes2")
        ) #END OF VIRGINIA DROUGHT UI
        
        #"NoVa: Drought Watch; Shenandoah: Drought Emergency")
      ) # end of c3 - 2nd main column
    ) # end of c1 - major column that contains whole body
  ), # end of major row that contains whole body
  fluidRow( # Temporary row to display some output for QAing
    valueBoxOutput("QA_out", width = NULL)
  ) # end fluidRow for QAing purposes
) # end of tab panel

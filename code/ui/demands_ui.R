# *****************************************************************************
# DESCRIPTION
# *****************************************************************************
# Displays graphs and values on Demand tab
# *****************************************************************************
# INPUTS
# *****************************************************************************
# Content created in code/server/demands/demands_server.R
# *****************************************************************************

tabPanel("Demands",
fluidRow( # major row that contains whole body
  column( # major column that contains whole body
    width = 12,
    #
    # Graphs-------------------------------------------------------------------
    column(  # this is the 1st main column - with the graphs
      width = 6,
      fluidRow( # row with Potomac withdrawal graph
        box(
          title = "Potomac withdrawals - observed and forecasted",
          width = NULL,
          # plotOutput("pot_withdrawals", height = plot.height, width = plot.width)
          plotOutput("pot_withdrawals", height = "260px", width = plot.width)
        )
      ),
      fluidRow( # row with Potomac withdrawal graph
        box(
          title = "WMA production",
          width = NULL,
          plotOutput("wma_production", height = "260px", width = plot.width)
        )
      ) # end of 2nd fluid row
    ), # end of 1st main column - with graphs
    
    # Value boxes---------------------------------------------------------------
    column( # this is the 2nd main column - with values & triggers
      width = 6,
      box(title = "Yesterday's daily average withdrawals",
          width = NULL,
          height=60),
      valueBoxOutput("w_fw_pot_yesterday", width = NULL),
      valueBoxOutput("w_wssc_pot_yesterday", width = NULL),
      valueBoxOutput("w_wa_gf_yesterday", width = NULL),
      valueBoxOutput("w_wa_lf_yesterday", width = NULL),
      valueBoxOutput("w_lw_pot_yesterday", width = NULL),
      valueBoxOutput("w_fw_occ_yesterday", width = NULL),
      valueBoxOutput("w_wssc_pat_yesterday", width = NULL),
      valueBoxOutput("w_wma_pot_yesterday", width = NULL)

      # br(),
      # box(title = "Jennings Randolph water supply release based on LFalls 9-day forecast from LFFS",
      #     width = NULL,
      #     height=60),
      # valueBoxOutput("lfalls_lffs_9day_fc", width = NULL),
      # valueBoxOutput("lffs_9day_deficit", width = NULL),
      # valueBoxOutput("luke_target2", width = NULL)
      # # valueBoxOutput("lfalls_obs", width = NULL)
    ) # end of 2nd main column
  ) # end of major column that contains whole body
) # end of major row that contains whole body

) # end of tab panel

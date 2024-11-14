#ui anomalia congênita (07-11-2024)
 
  tabler_tab_item(
         tabName = "anomalia",
          page_heading(title = 'Anomalias Congênitas', pretitle = 'Painel de informações Sinasc',
          tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'anomalia_dropdown',
                                uiOutput('anomalia_dropopcoes'), left = T))
                                ),
       tags$div(class = 'page-body',
        tags$div(class = 'container-xl',
        hr(),
        
        fluidRow(class = 'row-deck row-cards',
                mod_summary_card_ui('anomalia_total', div_class = 'col-md-4'),
                mod_summary_card_ui('anomalia_prioritaria', div_class = 'col-md-4'),
                mod_summary_card_ui('anomalia_letal', div_class = 'col-md-4'),
                mod_summary_card_ui('anomalia_mapa', div_class = 'col-md-12'),
                mod_summary_card_ui('anomalia_tabela_acp', div_class = 'col-md-12'),
                mod_summary_card_ui('anomalia_tabela_letal', div_class = 'col-md-12')

        )
       ) #end div container
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item
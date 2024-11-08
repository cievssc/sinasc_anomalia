#ui hospital (21-out-24, 16:53h)
 
  tabler_tab_item(
         tabName = "hospital",
          page_heading(title = 'Hospitais', pretitle = 'Painel de informações Sinasc',
          tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'hospital_dropdown',
                                uiOutput('hospital_dropopcoes'), left = T))
                                ),
       tags$div(class = 'page-body',
        tags$div(class = 'container-xl',
        hr(),
        
        fluidRow(class = 'row-deck row-cards',
                mod_summary_card_ui('hospital_total', div_class = 'col-md-4'),
                mod_summary_card_ui('hospital_total_pub', div_class = 'col-md-4'),
                mod_summary_card_ui('hospital_total_priv', div_class = 'col-md-4'),
                mod_summary_card_ui('hospital_mapa', div_class = 'col-md-12'),
                mod_summary_card_ui('hospital_tabela', div_class = 'col-md-12')

        )
       ) #end div container
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item
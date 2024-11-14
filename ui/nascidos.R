 #ui nascidos vivos (14-out-24, 16:57h)
 
  tabler_tab_item(
         tabName = "nascidos_vivos",
          page_heading(title = 'Síntese dos registros de Nascidos Vivos', pretitle = 'Painel de informações Sinasc',
          tags$div( class ="col-auto ms-auto d-print-none",
                          shinyWidgets::dropdown(style = "unite", icon = icon("gear"),  inputId = 'nascido_dropdown',
                                uiOutput('nascido_dropopcoes'), left = T))
                                ),
       tags$div(class = 'page-body',
        tags$div(class = 'container-xl',
        hr(),
        fluidRow(class = 'row-deck row-cards',
                 mod_summary_card_ui('nascido_total', div_class = 'col-md-4'),
                  mod_summary_card_ui('nascido_total_prenatal', div_class = 'col-md-4'),
                  mod_summary_card_ui('nascido_total_parto', div_class = 'col-md-4'),

                  mod_summary_card_ui('nascido_timeserie', div_class = 'col-md-5'),
                  mod_summary_card_ui('nascido_mapa', div_class = 'col-md-7'),
                  mod_summary_card_ui('nascido_peso', div_class = 'col-md-4'),
                  mod_summary_card_ui('nascido_gestacao', div_class = 'col-md-4'),
                  mod_summary_card_ui('nascido_consulta', div_class = 'col-md-4'),
                  mod_summary_card_ui('nascido_tabela', div_class = 'col-md-12'),
                  mod_summary_card_ui('nascido_tabela_gar', div_class = 'col-md-12')
                  ) #end row
        
        
       ) #end div container
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item
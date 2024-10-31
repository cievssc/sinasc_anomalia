 #ui home  (14-jul-24, 21:16h)
 
  tabler_tab_item(
         tabName = "home",
          page_heading(title = 'Notificações de casos e surtos', pretitle = 'Painel geral Vigpass'),
       tags$div(class = 'page-body',
        tags$div(class = 'container-xl',
        # br(),
          hr(),
        fluidRow(class = 'row-deck row-cards',
             column(4,
               fluidRow(class = 'row row-deck row-cards',
                  mod_summary_card_ui('home_total', div_class = "col-md-12"),
                  mod_summary_card_ui('home_total_caso', div_class = "col-md-12"), #total de casos
                  mod_summary_card_ui('home_total_surto', div_class = "col-md-12") #total de surtos
             )), #end column
                  mod_summary_card_ui('home_mapa', div_class = "col-md-8"),
               #br(),
                  mod_summary_card_ui('home_serie', div_class = "col-md-5"),
                  mod_summary_card_ui('home_regbar', div_class = 'col-md-4'),
                  mod_summary_card_ui('home_origem', div_class = 'col-md-3')
                  ) #end row
       ) #end div container
       #verbatimTextOutput('visual')
       ) #end div page-body
       ) #end tabler_tab_item
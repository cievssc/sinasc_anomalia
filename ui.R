 #app Painel Sinasc (14-out-24, 14:07h)

 ui <-  tabler_page(dark = F,
   header_tabler(logo = img(src = "./images/cievs_nacional.png",style="width:150px;height:auto;",class="navbar-brand-image"),
               #HTML('<img src="./images/renaveh.png" width="2200" height="640" alt="Tabler" class="navbar-brand-image">'),
              tags$link(rel = "stylesheet",type = 'text/css', href = './css/legenda_leaflet.css'),
               tags$div( class =  "navbar-nav flex-row order-md-last", 
                    
               #tags$div(class = "nav-item d-none d-md-flex me-3",
                #shinyWidgets::dropdown(label = 'Filtros', style = "unite",right = T, icon = icon("gear"),  inputId = 'home_dropdown',
                 #               uiOutput('home_dropopcoes'), left = T)),
                  tags$div(class = "nav-item d-none d-md-flex me-3",
                     #uiOutput('home_dateinput') #códigtos em server_home
                     sliderInput('head_daterange',
                         'Período das notificações:',
                         min = min(dados[,'ano'], na.rm = T),
                         max = max(dados[,'ano'], na.rm = T), sep= '', step = 1,
                         value = c(max(dados[,'ano'], na.rm = T)-3,max(dados[,'ano'], na.rm = T)))
                      ), #end dib      
                      tags$div(class = "nav-item d-none d-md-flex me-3",
                      actionButton("head_atualizar", label = "Atualizar")
                          ) #end div
                          )
       
                          ), #endheader
   tabler_navbar(
     #brand_url = "https://preview-dev.tabler.io",
     #brand_image = "https://preview-dev.tabler.io/static/logo.svg",
     nav_menu = tabler_navbar_menu(
      inputId = 'current_tab',
       tabler_navbar_menu_item(
         text = "Nascidos Vivos",
         icon = icon_home(),
         tabName = "nascidos_vivos",
         selected = TRUE
       ),
       tabler_navbar_menu_item(
         text = "Hospital",
         icon = icon_hosp(),
         tabName = "hospital",
         selected = FALSE
       ),
       tabler_navbar_menu_item(
         text = "Anomalia Congênita",
         icon = icon_graph(),
         tabName = "anomalia",
         selected = FALSE
       )
     )#,
     #tags$button("update", "Change tab", icon = icon("exchange-alt"))
   ),
   tabler_body(classe = 'page-wrapper',
     tabler_tab_items(
                      
      source('./ui/nascidos.R',  local = T, encoding = 'UTF-8')$value
      ,
      source('./ui/hospital.R', local = T, encoding = 'UTF-8')$value 
      ,
      source('./ui/anomalia.R', local = T, encoding = 'UTF-8')$value 
    
       
     ),
    tabler_footer(
       left = "CIEVS/DIVE",
       right = a(href = "https://www.google.com")
     )
   )
  )
                        
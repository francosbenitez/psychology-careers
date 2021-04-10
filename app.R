# Carga de librerias necesarias
library("readxl")                      # Para cargar el dataset
library("ggplot2")                     # Para generar los plots
library("shiny")
library("dplyr")                       # Para transformar los datos y piping
library("tibble")                      # Para ver el dataset
library("ggiraph")                     # Para la interactividad del grafico
library("ggthemes")
library("shinydashboard")
library("gcookbook")

# Cargo el dataset
antiguedad <- read_excel("data/1. Antiguedad.xlsx")

antiguedad <- antiguedad %>%
  rename(universidad = Universidad,
         media_publicacion = `Media publicación`,
         media_publicacion_sin_ac = `Media publicacion (sin a.C.)`,
         de = `Desv. Est.`
  ) %>% 
  select(universidad, media_publicacion, media_publicacion_sin_ac, de) 

view(antiguedad)

# Interfaz de usuario 
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Datos", icon = icon("th"), tabName = "widgets",
             badgeColor = "green"
    ),
    menuItem("Gráficos", icon = icon("bar-chart-o"),
             menuSubItem("Antiguedad", tabName = "subitem1"),
             menuSubItem("Autores predominantes", tabName = "subitem2")
    ),
    menuItem("Código de fuente", icon = icon("file-code-o"),
             href = "https://github.com/francosbenitez/shiny-apps/blob/main/psychology-careers/app.R"
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Shiny Dashboard",
    titleWidth = 250
  ),
  sidebar,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem("dashboard",
              fluidRow(                                                             # Primer fila
                box(
                  title = "Antigüedad media de los textos de las carreras de psicología según Universidad",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 7,                                                   # Amplitud de la primera columna
                       h4("Seleccioná universidades"),                              # Titulo de la primera fila
                       actionButton("reset", label = "Reiniciar selección"),        # Boton de reinicio de las selecciones
                       HTML("<br><br>"),
                       ggiraph::girafeOutput("por_universidad")                     # Grafico interactivo por universidad
                ),
                box(
                  status = "warning",
                  width = 5,                                                   # Amplitud de la segunda columna
                       h4("Universidades apuntadas"),                               # Primer titulo de la segunda columna
                       verbatimTextOutput("console"),                               # Universidades apuntadas
                       h4("Universidades seleccionadas"),                           # Segundo titulo de la segunda columna
                       tableOutput("datatab")                                       # Aca aparece la tabla, cuyo nombre es "datatab" 
                ),
                box(
                    title = "Variables",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 3,
                    HTML("<ul>
                    <li><i>universidad</i> = Universidades de Argentina.</li>
                    <li><i>media_publicacion</i> = Media de publicación de los textos de la carrera.</li>
                    <li><i>media_publicacion_sin_ac</i> = Media de publicación de los textos de la carrera sin considerar los textos anteriores al siglo X.</li>
                    <li><i>de</i> = Desviación estándar.</li>
                         </ul>")
                ),
                box(
                    title = "Notas",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 4,
                    HTML("<ul>
                    <li>En <strong>promedio</strong>, la bibliografía de las <strong>asignaturas obligatorias</strong> de las carreras tiene <strong>44</strong> años de antiguedad cuando consideramos todos los textos.</li>
                    <li>En <strong>ninguna</strong> carrera de psicología el promedio de publicación de la bibliografía es <strong>posterior</strong> al año <strong>1982</strong>.</li>
                    <li>En la mayoría de las carreras, la bibliografía de las asignaturas se publicó en promedio entre la <strong>década</strong> de <strong>1970</strong> y <strong>1980</strong>.</li>
                         </ul>")
          )
        )
      ),
      tabItem(tabName = "widgets",
              h2("Datos")
      )
    )
  )
)


# Codigo del servidor 
gg_antiguedad <- antiguedad %>% 
  arrange(media_publicacion) %>% 
  mutate(universidad = factor(universidad, levels = universidad)) %>% 
  ggplot(aes(y = as.factor(universidad), x = media_publicacion, label = media_publicacion)) +
  geom_col_interactive(aes(x = media_publicacion_sin_ac, data_id = universidad, tooltip = universidad, fill = "black")) +
  geom_col_interactive(aes(data_id = universidad, tooltip = universidad, fill = "grey")) +
  labs(x = "
       Antiguedad media", y = "
       Universidad") +
  geom_vline(xintercept = mean(antiguedad$media_publicacion),
             linetype= "dashed",
             size = 1) +
  coord_cartesian(xlim=c(1950,2000)) +
  geom_label_interactive(aes(
    tooltip = media_publicacion,
    colour = "grey",
    fontface = "bold"), 
    size = 3.5) +
  guides(fill=FALSE, color=FALSE) +
  theme_light()

server <- function(input, output) { 
  
  universidad_seleccionada <- reactive({
    input$por_universidad_selected                                       # Del paquete
  })
  output$console <- renderPrint({                                        # Consola
    input$por_universidad_hovered                                        # Del paquete
  })
  
  output$por_universidad <- renderGirafe({                               # Traigo el grafico
    x <- girafe(code = print(gg_antiguedad),
                width_svg = 6, height_svg = 5,
                options = list(
                  opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;", reactive = TRUE),
                  opts_selection(
                    type = "multiple")
                ))
    x
  })
  
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'por_universidad_set', message = character(0))
  })
  
  output$datatab <- renderTable({
    out <- antiguedad[antiguedad$universidad %in% universidad_seleccionada(), ]
    if(nrow(out) < 1) return(NULL)
    row.names(out) <- NULL
    out
  })
  
}

shinyApp(ui, server)

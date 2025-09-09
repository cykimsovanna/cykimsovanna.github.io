<<<<<<< HEAD
library(shiny)
library(DBI)
library(dplyr)
library(RSQLite)
library(DT)
library(shiny)
library(leaflet)
library(sf)
library(RPostgres)
library(ggplot2)
library(base64enc)
library(stringr)
library(forcats)
library(ggrepel)
library(rsconnect)

db_conn <- function() {
  dbConnect(
    RSQLite::SQLite(),
    "khm_fdi.sqlite"
  )
}

# UI
ui <- fluidPage(
  tags$head(
    tags$title("Cambodia FDI & Trade")
  ),
  titlePanel(
    div(style = "font-size: 28px; font-weight: bold;",
        "FOREIGN DIRECT INVESTMENTS & TRADE IN CAMBODIA")
  ),
  tabsetPanel(id = "tabs",
              tabPanel("ALL FDI PROJECTS", value = "ALL FDI PROJECTS",
                       fluidRow(
                         column(
                           width = 3,
                           wellPanel(
                             selectInput("country_filter", "Investor Country:", choices = c("All"), selected = "All"),
                             selectInput("sector_filter", "Economic Sector:", choices = c("All"), selected = "All"),
                             selectInput("province_filter", "Province:", choices = c("All"), selected = "All"),
                             actionButton("apply_filters", "Apply Filters")
                           ),
                           tabsetPanel(
                             tabPanel("By Country", dataTableOutput("country_projects_table")),
                             tabPanel("By Sector", dataTableOutput("sector_projects_table")),
                             tabPanel("By Province", dataTableOutput("province_projects_table"))
                           )
                         ),
                         column(width = 9, dataTableOutput("fdi_table"))
                       )
              ),
              tabPanel("TRADE OVERVIEW", dataTableOutput("trade_overview_table")),
              tabPanel("IMPORTS", value = "Imports",
                       fluidRow(
                         column(
                           width = 6,
                           fluidRow(
                             column(6,
                                    selectInput("import_country", "Select Country:",
                                                choices = c("China", "France", "Hong Kong", "Japan", "Malaysia", 
                                                            "Singapore", "South Korea", "Thailand", "USA", "Vietnam")
                                    )
                             ),
                             column(6,
                                    div(style = "padding-top: 30px;", uiOutput("import_heading"))
                             )
                           ),
                           div(style = "overflow-x: auto;", dataTableOutput("imports_table"))
                         ),
                         column(
                           width = 6,
                           plotOutput("import_bar", height = "300px"),
                           plotOutput("import_pie", height = "300px")
                         )
                       )
              ),
              tabPanel("EXPORTS", value = "Exports",
                       fluidRow(
                         column(
                           width = 6,
                           fluidRow(
                             column(6,
                                    selectInput("export_country", "Select Country:",
                                                choices = c("China", "France", "Hong Kong", "Japan", "Malaysia", 
                                                            "Singapore", "South Korea", "Thailand", "USA", "Vietnam")
                                    )
                             ),
                             column(6,
                                    div(style = "padding-top: 30px;", uiOutput("export_heading"))
                             )
                           ),
                           div(style = "overflow-x: auto;", dataTableOutput("exports_table"))
                         ),
                         column(
                           width = 6,
                           plotOutput("export_bar", height = "300px"),
                           plotOutput("export_pie", height = "300px")
                         )
                       )
              ),
              tabPanel("MAP OF FDI PROJECTS",
                       tags$head(tags$style(HTML(".leaflet-popup-content { width: 600px !important; }"))),
                       leafletOutput("cambodia_map", height = "800px")
              )
  )
)

# SERVER
server <- function(input, output, session) {
  con <- db_conn()
  
  observe({
    countries <- dbGetQuery(con, "SELECT DISTINCT country_name FROM investor_country ORDER BY country_name")
    updateSelectInput(session, "country_filter", choices = c("All", countries$country_name))
    
    sectors <- dbGetQuery(con, "SELECT DISTINCT sector FROM fdi_project ORDER BY sector")
    updateSelectInput(session, "sector_filter", choices = c("All", sectors$sector))
    
    provinces <- dbGetQuery(con, "SELECT DISTINCT province FROM province ORDER BY province")
    updateSelectInput(session, "province_filter", choices = c("All", provinces$province))
  })
  
  filtered_projects <- reactive({
    query <- "
      SELECT f.sector, f.project, f.investment, f.job_creation,
        GROUP_CONCAT(DISTINCT c.company) AS companies,
        GROUP_CONCAT(DISTINCT t.country_name) AS countries,
        GROUP_CONCAT(DISTINCT p.province) AS provinces
      FROM fdi_project f
      LEFT JOIN project_company pc ON f.fdi_id = pc.fdi_id
      LEFT JOIN company c ON pc.company_id = c.company_id
      LEFT JOIN project_country pt ON f.fdi_id = pt.fdi_id
      LEFT JOIN investor_country t ON pt.country_id = t.country_id
      LEFT JOIN project_province pp ON f.fdi_id = pp.fdi_id
      LEFT JOIN province p ON pp.province_id = p.province_id
      GROUP BY f.fdi_id, f.sector, f.project, f.investment, f.job_creation
    "
    df <- dbGetQuery(con, query)
    
    colnames(df) <- c(
      "Economic Sector",
      "Development Project",
      "Amount of Investment (in millions USD)",
      "Job Creation",
      "Companies",
      "Investor Country",
      "Location of Investment"
    )
    if (input$country_filter != "All") {
      df <- df %>% filter(grepl(input$country_filter, `Investor Country`))
    }
    if (input$sector_filter != "All") {
      df <- df %>% filter(`Economic Sector` == input$sector_filter)
    }
    if (input$province_filter != "All") {
      df <- df %>% filter(grepl(input$province_filter, `Location of Investment`))
    }
    
    df
  })
  
  # ALL FDI PROJECTS
  output$fdi_table <- renderDataTable({
    input$apply_filters
    isolate({
      df <- filtered_projects()
      colnames(df) <- c(
        "Economic Sector",
        "Development Project",
        "Amount of Investment (in millions USD)",
        "Job Creation",
        "Companies",
        "Investor Country",
        "Location of Investment"
      )
      datatable(df, options = list(scrollX = TRUE))
    })
  })
  
  # TRADE OVERVIEW
  output$trade_overview_table <- renderDataTable({
    df <- dbGetQuery(con, "
    SELECT 
      country_name,
      trade_balance,
      total_import,
      total_export,
      tariff_cambodia,
      tariff_partner
    FROM trade_overview
    ORDER BY country_name
  ")
    
    colnames(df) <- c(
      "Country",
      "Trade Balance (in millions USD)",
      "Total Import (in millions USD)",
      "Total Export (in millions USD)",
      "Avg Tariff Imposed by Cambodia (%)",
      "Avg Tariff Imposed by Partner Country (%)"
    )
    
    datatable(df, options = list(pageLength = nrow(df), scrollX = TRUE))
  })
  
  # IMPORTS
  output$imports_table <- renderDataTable({
    req(input$import_country)
    
    query <- "
    SELECT i.product_type, i.import_value, i.import_share
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
  "
    
    df <- dbGetQuery(con, query, params = list(input$import_country))
    
    colnames(df) <- c("Product Type", "Import Value (in thousands USD)", "Import Share (%)")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$import_heading <- renderUI({
    req(input$import_country)
    h4(strong(paste("Imports from", input$import_country)), style = "font-size: 20px;")
  })
  
  ## Bar chart
  output$import_bar <- renderPlot({
    req(input$import_country)
    df <- dbGetQuery(con, "
    SELECT i.product_type, i.import_value
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
    ORDER BY i.import_value DESC
  ", params = list(input$import_country))
    
    df$import_value <- df$import_value / 1000
    ggplot(df, aes(x = reorder(product_type, import_value), y = import_value)) +
      geom_col(fill = "#941111") +
      coord_flip() +
      labs(x = "Product", y = "Millions of USD", title = "Ranking Imports by Product") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
  
  ## Pie chart
  output$import_pie <- renderPlot({
    req(input$import_country)
    
    df <- dbGetQuery(con, "
    SELECT i.product_type, i.import_share
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
  ", params = list(input$import_country))
    
    df <- df %>%
      arrange(desc(import_share)) %>%
      mutate(
        label = paste0(product_type, " (", round(import_share, 1), "%)"),
        label = factor(label, levels = label)
      )
    
    pie_colors <- c(
      "Animal" = "#ff7d00",
      "Chemicals" = "#2e507d",
      "Food Products" = "#ea9780",
      "Footwear" = "#f464a9",
      "Fuels" = "#8ca66a",
      "Hides and Skins" = "#de3c23",
      "Machine and Electronics" = "#deda23",
      "Metals" = "#e8c577",
      "Minerals" = "#dd9ae4",
      "Miscellaneous" = "#a8a8a8",
      "Plastic or Rubber" = "#5767ac",
      "Stone and Glass" = "#5c8d85",
      "Textiles" = "#41b7ea",
      "Transportation" = "#63656c",
      "Vegetable" = "#77d548",
      "Wood" = "#7f5d3a"
    )
    
    color_map <- pie_colors[df$product_type]
    names(color_map) <- df$label
    
    ggplot(df, aes(x = "", y = import_share, fill = label)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = color_map) +
      theme_void() +
      labs(title = "Import Distribution by Product") +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  # EXPORTS
  output$exports_table <- renderDataTable({
    req(input$export_country)
    
    query <- "
    SELECT e.product_type, e.export_value, e.export_share
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
  "
    
    df <- dbGetQuery(con, query, params = list(input$export_country))
    
    colnames(df) <- c("Product Type", "Export Value (in thousands USD)", "Export Share (%)")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$export_heading <- renderUI({
    req(input$export_country)
    h4(strong(paste("Exports to", input$export_country)), style = "font-size: 20px;")
  })
  
  ## Bar chart
  output$export_bar <- renderPlot({
    req(input$export_country)
    df <- dbGetQuery(con, "
    SELECT e.product_type, e.export_value
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
    ORDER BY e.export_value DESC
  ", params = list(input$export_country))
    
    df$export_value <- df$export_value / 1000
    ggplot(df, aes(x = reorder(product_type, export_value), y = export_value)) +
      geom_col(fill = "#366047") +
      coord_flip() +
      labs(x = "Product", y = "Millions of USD", title = "Ranking Exports by Product") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
  
  ## Pie chart
  output$export_pie <- renderPlot({
    req(input$export_country)
    
    df <- dbGetQuery(con, "
    SELECT e.product_type, e.export_share
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
  ", params = list(input$export_country))
    
    df <- df %>%
      arrange(desc(export_share)) %>%
      mutate(
        label = paste0(product_type, " (", round(export_share, 1), "%)"),
        label = factor(label, levels = label)
      )
    
    pie_colors <- c(
      "Animal" = "#ff7d00",
      "Chemicals" = "#2e507d",
      "Food Products" = "#ea9780",
      "Footwear" = "#f464a9",
      "Fuels" = "#8ca66a",
      "Hides and Skins" = "#de3c23",
      "Machine and Electronics" = "#deda23",
      "Metals" = "#e8c577",
      "Minerals" = "#dd9ae4",
      "Miscellaneous" = "#a8a8a8",
      "Plastic or Rubber" = "#5767ac",
      "Stone and Glass" = "#5c8d85",
      "Textiles" = "#41b7ea",
      "Transportation" = "#63656c",
      "Vegetable" = "#77d548",
      "Wood" = "#7f5d3a"
    )
    
    color_map <- pie_colors[df$product_type]
    names(color_map) <- df$label
    
    ggplot(df, aes(x = "", y = export_share, fill = label)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = color_map) +
      theme_void() +
      labs(title = "Export Distribution by Product") +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  # PROJECTS BY COUNTRY
  output$country_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT t.country_name, COUNT(DISTINCT pco.fdi_id) AS num_projects
      FROM project_country pco
      LEFT JOIN investor_country t ON pco.country_id = t.country_id
      GROUP BY t.country_name
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Country of Origin", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # PROJECTS BY SECTOR
  output$sector_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT sector, COUNT(DISTINCT fdi_id) AS num_projects
      FROM fdi_project
      GROUP BY sector
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Economic Sector", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # PROJECTS BY PROVINCE
  output$province_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT p.province, COUNT(DISTINCT pp.fdi_id) AS num_projects
      FROM project_province pp
      LEFT JOIN province p ON pp.province_id = p.province_id
      GROUP BY p.province
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Province", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # FDI MAP
  provinces_geo <- st_read("geoBoundaries-KHM-ADM1_simplified.geojson", quiet = TRUE)
  provinces_geo$shapeName <- tolower(trimws(provinces_geo$shapeName))
  rename_map <- c(
    "ratanakiri province" = "Ratanak Kiri",
    "bantey meanchey"     = "Banteay Meanchey",
    "mondulkiri"          = "Mondul Kiri",
    "tbong khmum"         = "Tboung Khmum"
  )
  provinces_geo$shapeName <- recode(provinces_geo$shapeName, !!!rename_map)
  provinces_geo$shapeName <- str_to_title(provinces_geo$shapeName)
  
  project_counts <- dbGetQuery(con, "
    SELECT p.province, COUNT(DISTINCT pp.fdi_id) AS num_projects
    FROM project_province pp
    LEFT JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province
  ")
  
  sector_dist <- dbGetQuery(con, "
    SELECT p.province, f.sector, COUNT(*) AS count
    FROM fdi_project f
    JOIN project_province pp ON f.fdi_id = pp.fdi_id
    JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province, f.sector
  ")
  
  country_dist <- dbGetQuery(con, "
    SELECT p.province, t.country_name, COUNT(*) AS count
    FROM fdi_project f
    JOIN project_country pco ON f.fdi_id = pco.fdi_id
    JOIN investor_country t ON pco.country_id = t.country_id
    JOIN project_province pp ON f.fdi_id = pp.fdi_id
    JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province, t.country_name
  ")
  
  provinces_geo <- provinces_geo %>%
    left_join(project_counts, by = c("shapeName" = "province"))
  
  sector_colors <- c(
    "Agriculture" = "#7AFF41",
    "Commercial Real Estate" = "#a7bdd3",
    "Energy" = "#C6C238",
    "Financial" = "#72A97A",
    "Industrial Development" = "#B2AFA1",
    "Infrastructure" = "#8fbfcd",
    "Mining Industry" = "#bda67f",
    "Tourism" = "#FF7192"
  )
  
  country_colors <- c(
    "China" = "#f65959",
    "Thailand" = "#cf28d3",
    "USA" = "#506aff",
    "Vietnam" = "#fdc23c",
    "Hong Kong" = "#218f80",
    "Japan" = "#9df2eb",
    "Taiwan" = "#8c47e0",
    "Singapore" = "#ffee79",
    "South Korea" = "#c8c2ff",
    "Malaysia" = "#52855d",
    "France" = "#61bffd"
  )
  
  popup_content <- lapply(provinces_geo$shapeName, function(prov) {
    n_proj <- project_counts$num_projects[match(prov, project_counts$province)]
    if (is.na(n_proj)) n_proj <- 0
    
    ## Sector Charts
    sector_data <- sector_dist %>%
      filter(province == prov) %>%
      arrange(desc(count))
    
    label_size <- if (nrow(sector_data) > 6) 3 else 5
    
    p1 <- ggplot()
    
    if (nrow(sector_data) > 0) {
      sector_data$sector <- factor(sector_data$sector, levels = sector_data$sector)
      sector_data <- sector_data %>%
        mutate(percentage = round(100 * count / sum(count), 1),
               label = paste0(percentage, "%"))
      
      p1 <- ggplot(sector_data, aes(x = "", y = count, fill = sector)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_label_repel(aes(label = label),
                         position = position_stack(vjust = 0.5),
                         show.legend = FALSE,
                         size = label_size,
                         fontface = "bold",
                         box.padding = 0.3,
                         segment.color = "grey40") +
        scale_fill_manual(values = sector_colors) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold")
        )
    }
    
    ## Country Charts
    country_data <- country_dist %>%
      filter(province == prov) %>%
      arrange(desc(count))
    
    label_size <- if (nrow(sector_data) > 6) 3 else 5
    
    p2 <- ggplot()
    
    if (nrow(country_data) > 0) {
      country_data$country_name <- factor(country_data$country_name, levels = country_data$country_name)
      country_data <- country_data %>%
        mutate(percentage = round(100 * count / sum(count), 1),
               label = paste0(percentage, "%"))
      
      p2 <- ggplot(country_data, aes(x = "", y = count, fill = country_name)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_label_repel(aes(label = label),
                         position = position_stack(vjust = 0.5),
                         show.legend = FALSE,
                         size = label_size,
                         fontface = "bold",
                         box.padding = 0.3,
                         segment.color = "grey40") +
        scale_fill_manual(values = country_colors) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold")
        )
    }
    
    p1_file <- tempfile(fileext = ".png")
    ggsave(p1_file, plot = p1, width = 6.5, height = 5, dpi = 120)
    p1_base64 <- base64enc::dataURI(file = p1_file, mime = "image/png")
    
    p2_file <- tempfile(fileext = ".png")
    ggsave(p2_file, plot = p2, width = 6.5, height = 5, dpi = 120)
    p2_base64 <- base64enc::dataURI(file = p2_file, mime = "image/png")
    
    paste0(
      "<b>", prov, "</b><br>",
      "Number of Investments: ", n_proj, "<br>",
      "<div style='display:flex; gap:10px; justify-content:center'>",
      "<img src='", p1_base64, "' width='280'>",
      "<img src='", p2_base64, "' width='280'>",
      "</div>"
    )
  })
  
  output$cambodia_map <- renderLeaflet({
    leaflet(provinces_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~colorNumeric("YlGnBu", provinces_geo$num_projects)(num_projects),
        fillOpacity = 0.7,
        weight = 1,
        color = "#666",
        label = ~shapeName,
        popup = popup_content
      ) %>%
      addLegend(
        position = "topright",
        pal = colorNumeric("YlGnBu", provinces_geo$num_projects),
        values = provinces_geo$num_projects,
        title = "Number of Investments",
        labFormat = labelFormat(suffix = ""),
        opacity = 0.7
      )
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
}

=======
library(shiny)
library(DBI)
library(dplyr)
library(RSQLite)
library(DT)
library(shiny)
library(leaflet)
library(sf)
library(RPostgres)
library(ggplot2)
library(base64enc)
library(stringr)
library(forcats)
library(ggrepel)
library(rsconnect)

db_conn <- function() {
  dbConnect(
    RSQLite::SQLite(),
    "khm_fdi.sqlite"
  )
}

# UI
ui <- fluidPage(
  tags$head(
    tags$title("Cambodia FDI & Trade")
  ),
  titlePanel(
    div(style = "font-size: 28px; font-weight: bold;",
        "FOREIGN DIRECT INVESTMENTS & TRADE IN CAMBODIA")
  ),
  tabsetPanel(id = "tabs",
              tabPanel("ALL FDI PROJECTS", value = "ALL FDI PROJECTS",
                       fluidRow(
                         column(
                           width = 3,
                           wellPanel(
                             selectInput("country_filter", "Investor Country:", choices = c("All"), selected = "All"),
                             selectInput("sector_filter", "Economic Sector:", choices = c("All"), selected = "All"),
                             selectInput("province_filter", "Province:", choices = c("All"), selected = "All"),
                             actionButton("apply_filters", "Apply Filters")
                           ),
                           tabsetPanel(
                             tabPanel("By Country", dataTableOutput("country_projects_table")),
                             tabPanel("By Sector", dataTableOutput("sector_projects_table")),
                             tabPanel("By Province", dataTableOutput("province_projects_table"))
                           )
                         ),
                         column(width = 9, dataTableOutput("fdi_table"))
                       )
              ),
              tabPanel("TRADE OVERVIEW", dataTableOutput("trade_overview_table")),
              tabPanel("IMPORTS", value = "Imports",
                       fluidRow(
                         column(
                           width = 6,
                           fluidRow(
                             column(6,
                                    selectInput("import_country", "Select Country:",
                                                choices = c("China", "France", "Hong Kong", "Japan", "Malaysia", 
                                                            "Singapore", "South Korea", "Thailand", "USA", "Vietnam")
                                    )
                             ),
                             column(6,
                                    div(style = "padding-top: 30px;", uiOutput("import_heading"))
                             )
                           ),
                           div(style = "overflow-x: auto;", dataTableOutput("imports_table"))
                         ),
                         column(
                           width = 6,
                           plotOutput("import_bar", height = "300px"),
                           plotOutput("import_pie", height = "300px")
                         )
                       )
              ),
              tabPanel("EXPORTS", value = "Exports",
                       fluidRow(
                         column(
                           width = 6,
                           fluidRow(
                             column(6,
                                    selectInput("export_country", "Select Country:",
                                                choices = c("China", "France", "Hong Kong", "Japan", "Malaysia", 
                                                            "Singapore", "South Korea", "Thailand", "USA", "Vietnam")
                                    )
                             ),
                             column(6,
                                    div(style = "padding-top: 30px;", uiOutput("export_heading"))
                             )
                           ),
                           div(style = "overflow-x: auto;", dataTableOutput("exports_table"))
                         ),
                         column(
                           width = 6,
                           plotOutput("export_bar", height = "300px"),
                           plotOutput("export_pie", height = "300px")
                         )
                       )
              ),
              tabPanel("MAP OF FDI PROJECTS",
                       tags$head(tags$style(HTML(".leaflet-popup-content { width: 600px !important; }"))),
                       leafletOutput("cambodia_map", height = "800px")
              )
  )
)

# SERVER
server <- function(input, output, session) {
  con <- db_conn()
  
  observe({
    countries <- dbGetQuery(con, "SELECT DISTINCT country_name FROM investor_country ORDER BY country_name")
    updateSelectInput(session, "country_filter", choices = c("All", countries$country_name))
    
    sectors <- dbGetQuery(con, "SELECT DISTINCT sector FROM fdi_project ORDER BY sector")
    updateSelectInput(session, "sector_filter", choices = c("All", sectors$sector))
    
    provinces <- dbGetQuery(con, "SELECT DISTINCT province FROM province ORDER BY province")
    updateSelectInput(session, "province_filter", choices = c("All", provinces$province))
  })
  
  filtered_projects <- reactive({
    query <- "
      SELECT f.sector, f.project, f.investment, f.job_creation,
        GROUP_CONCAT(DISTINCT c.company) AS companies,
        GROUP_CONCAT(DISTINCT t.country_name) AS countries,
        GROUP_CONCAT(DISTINCT p.province) AS provinces
      FROM fdi_project f
      LEFT JOIN project_company pc ON f.fdi_id = pc.fdi_id
      LEFT JOIN company c ON pc.company_id = c.company_id
      LEFT JOIN project_country pt ON f.fdi_id = pt.fdi_id
      LEFT JOIN investor_country t ON pt.country_id = t.country_id
      LEFT JOIN project_province pp ON f.fdi_id = pp.fdi_id
      LEFT JOIN province p ON pp.province_id = p.province_id
      GROUP BY f.fdi_id, f.sector, f.project, f.investment, f.job_creation
    "
    df <- dbGetQuery(con, query)
    
    colnames(df) <- c(
      "Economic Sector",
      "Development Project",
      "Amount of Investment (in millions USD)",
      "Job Creation",
      "Companies",
      "Investor Country",
      "Location of Investment"
    )
    if (input$country_filter != "All") {
      df <- df %>% filter(grepl(input$country_filter, `Investor Country`))
    }
    if (input$sector_filter != "All") {
      df <- df %>% filter(`Economic Sector` == input$sector_filter)
    }
    if (input$province_filter != "All") {
      df <- df %>% filter(grepl(input$province_filter, `Location of Investment`))
    }
    
    df
  })
  
  # ALL FDI PROJECTS
  output$fdi_table <- renderDataTable({
    input$apply_filters
    isolate({
      df <- filtered_projects()
      colnames(df) <- c(
        "Economic Sector",
        "Development Project",
        "Amount of Investment (in millions USD)",
        "Job Creation",
        "Companies",
        "Investor Country",
        "Location of Investment"
      )
      datatable(df, options = list(scrollX = TRUE))
    })
  })
  
  # TRADE OVERVIEW
  output$trade_overview_table <- renderDataTable({
    df <- dbGetQuery(con, "
    SELECT 
      country_name,
      trade_balance,
      total_import,
      total_export,
      tariff_cambodia,
      tariff_partner
    FROM trade_overview
    ORDER BY country_name
  ")
    
    colnames(df) <- c(
      "Country",
      "Trade Balance (in millions USD)",
      "Total Import (in millions USD)",
      "Total Export (in millions USD)",
      "Avg Tariff Imposed by Cambodia (%)",
      "Avg Tariff Imposed by Partner Country (%)"
    )
    
    datatable(df, options = list(pageLength = nrow(df), scrollX = TRUE))
  })
  
  # IMPORTS
  output$imports_table <- renderDataTable({
    req(input$import_country)
    
    query <- "
    SELECT i.product_type, i.import_value, i.import_share
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
  "
    
    df <- dbGetQuery(con, query, params = list(input$import_country))
    
    colnames(df) <- c("Product Type", "Import Value (in thousands USD)", "Import Share (%)")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$import_heading <- renderUI({
    req(input$import_country)
    h4(strong(paste("Imports from", input$import_country)), style = "font-size: 20px;")
  })
  
  ## Bar chart
  output$import_bar <- renderPlot({
    req(input$import_country)
    df <- dbGetQuery(con, "
    SELECT i.product_type, i.import_value
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
    ORDER BY i.import_value DESC
  ", params = list(input$import_country))
    
    df$import_value <- df$import_value / 1000
    ggplot(df, aes(x = reorder(product_type, import_value), y = import_value)) +
      geom_col(fill = "#941111") +
      coord_flip() +
      labs(x = "Product", y = "Millions of USD", title = "Ranking Imports by Product") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
  
  ## Pie chart
  output$import_pie <- renderPlot({
    req(input$import_country)
    
    df <- dbGetQuery(con, "
    SELECT i.product_type, i.import_share
    FROM import i
    JOIN investor_country t ON i.country_id = t.country_id
    WHERE t.country_name = ?
  ", params = list(input$import_country))
    
    df <- df %>%
      arrange(desc(import_share)) %>%
      mutate(
        label = paste0(product_type, " (", round(import_share, 1), "%)"),
        label = factor(label, levels = label)
      )
    
    pie_colors <- c(
      "Animal" = "#ff7d00",
      "Chemicals" = "#2e507d",
      "Food Products" = "#ea9780",
      "Footwear" = "#f464a9",
      "Fuels" = "#8ca66a",
      "Hides and Skins" = "#de3c23",
      "Machine and Electronics" = "#deda23",
      "Metals" = "#e8c577",
      "Minerals" = "#dd9ae4",
      "Miscellaneous" = "#a8a8a8",
      "Plastic or Rubber" = "#5767ac",
      "Stone and Glass" = "#5c8d85",
      "Textiles" = "#41b7ea",
      "Transportation" = "#63656c",
      "Vegetable" = "#77d548",
      "Wood" = "#7f5d3a"
    )
    
    color_map <- pie_colors[df$product_type]
    names(color_map) <- df$label
    
    ggplot(df, aes(x = "", y = import_share, fill = label)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = color_map) +
      theme_void() +
      labs(title = "Import Distribution by Product") +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  # EXPORTS
  output$exports_table <- renderDataTable({
    req(input$export_country)
    
    query <- "
    SELECT e.product_type, e.export_value, e.export_share
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
  "
    
    df <- dbGetQuery(con, query, params = list(input$export_country))
    
    colnames(df) <- c("Product Type", "Export Value (in thousands USD)", "Export Share (%)")
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$export_heading <- renderUI({
    req(input$export_country)
    h4(strong(paste("Exports to", input$export_country)), style = "font-size: 20px;")
  })
  
  ## Bar chart
  output$export_bar <- renderPlot({
    req(input$export_country)
    df <- dbGetQuery(con, "
    SELECT e.product_type, e.export_value
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
    ORDER BY e.export_value DESC
  ", params = list(input$export_country))
    
    df$export_value <- df$export_value / 1000
    ggplot(df, aes(x = reorder(product_type, export_value), y = export_value)) +
      geom_col(fill = "#366047") +
      coord_flip() +
      labs(x = "Product", y = "Millions of USD", title = "Ranking Exports by Product") +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 11)
      )
  })
  
  ## Pie chart
  output$export_pie <- renderPlot({
    req(input$export_country)
    
    df <- dbGetQuery(con, "
    SELECT e.product_type, e.export_share
    FROM export e
    JOIN investor_country t ON e.country_id = t.country_id
    WHERE t.country_name = ?
  ", params = list(input$export_country))
    
    df <- df %>%
      arrange(desc(export_share)) %>%
      mutate(
        label = paste0(product_type, " (", round(export_share, 1), "%)"),
        label = factor(label, levels = label)
      )
    
    pie_colors <- c(
      "Animal" = "#ff7d00",
      "Chemicals" = "#2e507d",
      "Food Products" = "#ea9780",
      "Footwear" = "#f464a9",
      "Fuels" = "#8ca66a",
      "Hides and Skins" = "#de3c23",
      "Machine and Electronics" = "#deda23",
      "Metals" = "#e8c577",
      "Minerals" = "#dd9ae4",
      "Miscellaneous" = "#a8a8a8",
      "Plastic or Rubber" = "#5767ac",
      "Stone and Glass" = "#5c8d85",
      "Textiles" = "#41b7ea",
      "Transportation" = "#63656c",
      "Vegetable" = "#77d548",
      "Wood" = "#7f5d3a"
    )
    
    color_map <- pie_colors[df$product_type]
    names(color_map) <- df$label
    
    ggplot(df, aes(x = "", y = export_share, fill = label)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = color_map) +
      theme_void() +
      labs(title = "Export Distribution by Product") +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  # PROJECTS BY COUNTRY
  output$country_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT t.country_name, COUNT(DISTINCT pco.fdi_id) AS num_projects
      FROM project_country pco
      LEFT JOIN investor_country t ON pco.country_id = t.country_id
      GROUP BY t.country_name
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Country of Origin", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # PROJECTS BY SECTOR
  output$sector_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT sector, COUNT(DISTINCT fdi_id) AS num_projects
      FROM fdi_project
      GROUP BY sector
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Economic Sector", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # PROJECTS BY PROVINCE
  output$province_projects_table <- renderDataTable({
    df <- dbGetQuery(con, "
      SELECT p.province, COUNT(DISTINCT pp.fdi_id) AS num_projects
      FROM project_province pp
      LEFT JOIN province p ON pp.province_id = p.province_id
      GROUP BY p.province
      ORDER BY num_projects DESC
    ")
    colnames(df) <- c("Province", "Number of Projects")
    datatable(df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # FDI MAP
  provinces_geo <- st_read("geoBoundaries-KHM-ADM1_simplified.geojson", quiet = TRUE)
  provinces_geo$shapeName <- tolower(trimws(provinces_geo$shapeName))
  rename_map <- c(
    "ratanakiri province" = "Ratanak Kiri",
    "bantey meanchey"     = "Banteay Meanchey",
    "mondulkiri"          = "Mondul Kiri",
    "tbong khmum"         = "Tboung Khmum"
  )
  provinces_geo$shapeName <- recode(provinces_geo$shapeName, !!!rename_map)
  provinces_geo$shapeName <- str_to_title(provinces_geo$shapeName)
  
  project_counts <- dbGetQuery(con, "
    SELECT p.province, COUNT(DISTINCT pp.fdi_id) AS num_projects
    FROM project_province pp
    LEFT JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province
  ")
  
  sector_dist <- dbGetQuery(con, "
    SELECT p.province, f.sector, COUNT(*) AS count
    FROM fdi_project f
    JOIN project_province pp ON f.fdi_id = pp.fdi_id
    JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province, f.sector
  ")
  
  country_dist <- dbGetQuery(con, "
    SELECT p.province, t.country_name, COUNT(*) AS count
    FROM fdi_project f
    JOIN project_country pco ON f.fdi_id = pco.fdi_id
    JOIN investor_country t ON pco.country_id = t.country_id
    JOIN project_province pp ON f.fdi_id = pp.fdi_id
    JOIN province p ON pp.province_id = p.province_id
    GROUP BY p.province, t.country_name
  ")
  
  provinces_geo <- provinces_geo %>%
    left_join(project_counts, by = c("shapeName" = "province"))
  
  sector_colors <- c(
    "Agriculture" = "#7AFF41",
    "Commercial Real Estate" = "#a7bdd3",
    "Energy" = "#C6C238",
    "Financial" = "#72A97A",
    "Industrial Development" = "#B2AFA1",
    "Infrastructure" = "#8fbfcd",
    "Mining Industry" = "#bda67f",
    "Tourism" = "#FF7192"
  )
  
  country_colors <- c(
    "China" = "#f65959",
    "Thailand" = "#cf28d3",
    "USA" = "#506aff",
    "Vietnam" = "#fdc23c",
    "Hong Kong" = "#218f80",
    "Japan" = "#9df2eb",
    "Taiwan" = "#8c47e0",
    "Singapore" = "#ffee79",
    "South Korea" = "#c8c2ff",
    "Malaysia" = "#52855d",
    "France" = "#61bffd"
  )
  
  popup_content <- lapply(provinces_geo$shapeName, function(prov) {
    n_proj <- project_counts$num_projects[match(prov, project_counts$province)]
    if (is.na(n_proj)) n_proj <- 0
    
    ## Sector Charts
    sector_data <- sector_dist %>%
      filter(province == prov) %>%
      arrange(desc(count))
    
    label_size <- if (nrow(sector_data) > 6) 3 else 5
    
    p1 <- ggplot()
    
    if (nrow(sector_data) > 0) {
      sector_data$sector <- factor(sector_data$sector, levels = sector_data$sector)
      sector_data <- sector_data %>%
        mutate(percentage = round(100 * count / sum(count), 1),
               label = paste0(percentage, "%"))
      
      p1 <- ggplot(sector_data, aes(x = "", y = count, fill = sector)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_label_repel(aes(label = label),
                         position = position_stack(vjust = 0.5),
                         show.legend = FALSE,
                         size = label_size,
                         fontface = "bold",
                         box.padding = 0.3,
                         segment.color = "grey40") +
        scale_fill_manual(values = sector_colors) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold")
        )
    }
    
    ## Country Charts
    country_data <- country_dist %>%
      filter(province == prov) %>%
      arrange(desc(count))
    
    label_size <- if (nrow(sector_data) > 6) 3 else 5
    
    p2 <- ggplot()
    
    if (nrow(country_data) > 0) {
      country_data$country_name <- factor(country_data$country_name, levels = country_data$country_name)
      country_data <- country_data %>%
        mutate(percentage = round(100 * count / sum(count), 1),
               label = paste0(percentage, "%"))
      
      p2 <- ggplot(country_data, aes(x = "", y = count, fill = country_name)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_label_repel(aes(label = label),
                         position = position_stack(vjust = 0.5),
                         show.legend = FALSE,
                         size = label_size,
                         fontface = "bold",
                         box.padding = 0.3,
                         segment.color = "grey40") +
        scale_fill_manual(values = country_colors) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold")
        )
    }
    
    p1_file <- tempfile(fileext = ".png")
    ggsave(p1_file, plot = p1, width = 6.5, height = 5, dpi = 120)
    p1_base64 <- base64enc::dataURI(file = p1_file, mime = "image/png")
    
    p2_file <- tempfile(fileext = ".png")
    ggsave(p2_file, plot = p2, width = 6.5, height = 5, dpi = 120)
    p2_base64 <- base64enc::dataURI(file = p2_file, mime = "image/png")
    
    paste0(
      "<b>", prov, "</b><br>",
      "Number of Investments: ", n_proj, "<br>",
      "<div style='display:flex; gap:10px; justify-content:center'>",
      "<img src='", p1_base64, "' width='280'>",
      "<img src='", p2_base64, "' width='280'>",
      "</div>"
    )
  })
  
  output$cambodia_map <- renderLeaflet({
    leaflet(provinces_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~colorNumeric("YlGnBu", provinces_geo$num_projects)(num_projects),
        fillOpacity = 0.7,
        weight = 1,
        color = "#666",
        label = ~shapeName,
        popup = popup_content
      ) %>%
      addLegend(
        position = "topright",
        pal = colorNumeric("YlGnBu", provinces_geo$num_projects),
        values = provinces_geo$num_projects,
        title = "Number of Investments",
        labFormat = labelFormat(suffix = ""),
        opacity = 0.7
      )
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
}

>>>>>>> 8f79c9b00b2cfed878881fa5eb14d308d8627c32
shinyApp(ui = ui, server = server)
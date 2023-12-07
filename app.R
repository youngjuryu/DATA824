library(shiny)
library(FactoMineR)
library(factoextra)
library(cluster)

# Load data
data <- read.csv("/Users/youngju/Library/CloudStorage/OneDrive-UniversityofKansas/Fall 2023/DATA 824/final project/shiny/cluster/data_cluster.csv")
data <- scale(data)
data <- na.omit(data)

# UI part
ui <- fluidPage(
    titlePanel("Clustering Shiny App"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("clusters", "Number of clusters:", min = 1, max = 8, value = 2),
            actionButton("runAnalysis", "Run Analysis")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("K-means Clustering", plotOutput("kmeansPlot")),
                tabPanel("PAM Clustering", plotOutput("pamPlot"))
            )
        )
    )
)

# Server part
server <- function(input, output, session) {
    clustered_data <- reactive({
        # k-means clustering
        km_attrib <- kmeans(data, centers = input$clusters)
        km_clusters <- km_attrib$cluster
        
        # PAM clustering
        pam_attrib <- pam(data, k = input$clusters)
        pam_clusters <- pam_attrib$clustering
        
        data_with_clusters <- cbind(data, km_clusters, pam_clusters)
        
        return(data_with_clusters)
    })
    
    # k-means clustering plot
    output$kmeansPlot <- renderPlot({
        clustered_data_df <- clustered_data()
        fviz_cluster(kmeans(clustered_data_df[, c(1:8)], centers = input$clusters),
                     data = clustered_data_df, palette = "jco")
    })
    
    # PAM clustering plot
    output$pamPlot <- renderPlot({
        clustered_data_df <- clustered_data()
        pam_attrib <- pam(clustered_data_df[, c(1:8)], k = input$clusters)
        fviz_cluster(pam_attrib, data = clustered_data_df, palette = "jco")
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

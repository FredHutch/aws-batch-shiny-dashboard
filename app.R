library(shinydashboard)
library(jsonlite)
library(DT)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "AWS Batch"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Spot", tabName = "spot-test", icon = icon("microchip")),
      menuItem("On Demand", tabName = "refdbs_on_demand", icon = icon("microchip"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "spot-test",
              h4("Queue: spot-test"),
              fluidRow(
                valueBoxOutput("runningBox"),
                valueBoxOutput("runnableBox"),
                valueBoxOutput("succeededBox"),
                valueBoxOutput("startingBox"),
                valueBoxOutput("pendingBox"),
                valueBoxOutput("failedBox")
              ),
              fluidRow(
                tabBox(
                  title = "Jobs",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", width=NULL,
                  tabPanel("Running", DT::dataTableOutput("spot_running")),
                  tabPanel("Runnable", DT::dataTableOutput("spot_runnable")),
                  tabPanel("Succeeded", DT::dataTableOutput("spot_succeeded")),
                  tabPanel("Starting", DT::dataTableOutput("spot_starting")),
                  tabPanel("Pending", DT::dataTableOutput("spot_pending")),
                  tabPanel("Failed", DT::dataTableOutput("spot_failed"))
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "refdbs_on_demand",
              h4("Queue: refdbs_on_demand"),
              fluidRow(
                valueBoxOutput("runningBox2"),
                valueBoxOutput("runnableBox2"),
                valueBoxOutput("succeededBox2"),
                valueBoxOutput("startingBox2"),
                valueBoxOutput("pendingBox2"),
                valueBoxOutput("failedBox2")
              ),
              fluidRow(
                tabBox(
                  title = "Jobs",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2", width=NULL,
                  tabPanel("Running", DT::dataTableOutput("on_demand_running")),
                  tabPanel("Runnable", DT::dataTableOutput("on_demand_runnable")),
                  tabPanel("Succeeded", DT::dataTableOutput("on_demand_succeeded")),
                  tabPanel("Starting", DT::dataTableOutput("on_demand_starting")),
                  tabPanel("Pending", DT::dataTableOutput("on_demand_pending")),
                  tabPanel("Failed", DT::dataTableOutput("on_demand_failed"))
                )
              )
      )
    )
  )
  
)

server <- function(input, output) {
  
  get_job_list <- function(job_queue, job_status){
    r <- system(paste('aws batch list-jobs --job-queue', job_queue, '--job-status', toupper(job_status)), intern=TRUE)
    d <- fromJSON(r)
    d <- d$jobSummaryList
    
    if(job_status == "RUNNING" & length(d) > 0){
      d <- d[,c("jobName", "jobId", "startedAt", "createdAt")]
    }
    if(job_status == "RUNNABLE" & length(d) > 0){
      d <- d[,c("jobName", "jobId", "createdAt")]
    }
    if(job_status == "SUCCEEDED" & length(d) > 0){
      d <- d[,c("jobName", "jobId", "startedAt", "createdAt", "stoppedAt")]
    }
    if(job_status == "PENDING" & length(d) > 0){
      d <- d[,c("jobName", "jobId", "createdAt")]
    }
    if(job_status == "STARTING" & length(d) > 0){
      d <- d[,c("jobName", "jobId", "createdAt")]
    }
    if(job_status == "FAILED" & length(d) > 0){
      d <- d[,c("jobName", "statusReason", "jobId", "startedAt", "createdAt", "stoppedAt")]
    }
    if(length(d) > 0){
      if(nrow(d) > 0){
        d <- format_timestamp_columns(d)
      }
    }
    return(d)
  }
  
  format_timestamp_columns <- function(d){
    if(length(d) > 0){
      if(nrow(d) > 0){
        if('startedAt' %in% colnames(d)){
          d$started <- sapply(d$startedAt, format_timestamp)
          d$startedAt <- NULL
        }
        if('createdAt' %in% colnames(d)){
          d$created <- sapply(d$createdAt, format_timestamp)
          d$createdAt <- NULL
        }
        if('stoppedAt' %in% colnames(d)){
          d$stopped <- sapply(d$stoppedAt, format_timestamp)
          d$stoppedAt <- NULL
        }
      }
    }
    return(d)
  }
  
  current_time <- as.numeric(Sys.time())*1000
  
  format_timestamp <- function(t){
    if(is.numeric((t)) & length(t) == 1 & is.na(t) == FALSE){
      td <- duration(num = as.duration((current_time - t) / 1000), units = "seconds")
      output <- c()
      for(u in c("days", "hours", "minutes", "seconds")){
        v <- as.numeric(td, unit=u)
        if(v > 1 & u != "seconds"){
          output <- c(output, paste(as.character(floor(v)), substr(u, 1, 1), sep=''))
          td <- td - duration(num=floor(as.numeric(td, unit=u)), units=u)
        }
        if(u == "seconds"){
          output <- c(output, paste(as.character(floor(v)), 's'))
        }
      }
      return(paste(paste(output, collapse=", "), "ago"))
    }
    return(t)
  }
  
  njobs <- function(job_queue, job_status){
    d <- get_job_list(job_queue, job_status)
    if(length(d) > 0){return(nrow(d))}else{return(0)}
  }
  output$runningBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'RUNNING'), "Running", icon = icon("list"),
      color = "green"
    )
  })
  output$runnableBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'RUNNABLE'), "Runnable", icon = icon("list"),
      color = "blue"
    )
  })
  output$succeededBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'SUCCEEDED'), "Succeeded", icon = icon("list"),
      color = "orange"
    )
  })
  output$startingBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'STARTING'), "Starting", icon = icon("list"),
      color = "teal"
    )
  })
  output$pendingBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'PENDING'), "Pending", icon = icon("list"),
      color = "aqua"
    )
  })
  output$failedBox <- renderValueBox({
    valueBox(
      njobs('spot-test', 'FAILED'), "Failed", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$runningBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'RUNNING'), "Running", icon = icon("list"),
      color = "green"
    )
  })
  output$runnableBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'RUNNABLE'), "Runnable", icon = icon("list"),
      color = "blue"
    )
  })
  output$succeededBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'SUCCEEDED'), "Succeeded", icon = icon("list"),
      color = "orange"
    )
  })
  output$startingBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'STARTING'), "Starting", icon = icon("list"),
      color = "teal"
    )
  })
  output$pendingBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'PENDING'), "Pending", icon = icon("list"),
      color = "aqua"
    )
  })
  output$failedBox2 <- renderValueBox({
    valueBox(
      njobs('refdbs_on_demand', 'FAILED'), "Failed", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  output$spot_running <- DT::renderDataTable(
    get_job_list('spot-test', "RUNNING")
  )
  output$spot_runnable <- DT::renderDataTable(
    get_job_list('spot-test', "RUNNABLE")
  )
  output$spot_starting <- DT::renderDataTable(
    get_job_list('spot-test', "STARTING")
  )
  output$spot_pending <- DT::renderDataTable(
    get_job_list('spot-test', "PENDING")
  )
  output$spot_succeeded <- DT::renderDataTable(
    get_job_list('spot-test', "SUCCEEDED")
  )
  output$spot_failed <- DT::renderDataTable(
    get_job_list('spot-test', "FAILED")
  )
  output$on_demand_running <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "RUNNING")
  )
  output$on_demand_runnable <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "RUNNABLE")
  )
  output$on_demand_starting <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "STARTING")
  )
  output$on_demand_pending <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "PENDING")
  )
  output$on_demand_failed <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "FAILED")
  )
  output$on_demand_succeeded <- DT::renderDataTable(
    get_job_list('refdbs_on_demand', "SUCCEEDED")
  )
  
}

shinyApp(ui, server)

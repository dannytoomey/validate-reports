lapply(c("shiny","DT","tools","readxl","metafor","meta","esc","grid"),require,character.only=TRUE)
for(f in list.files('./analyses', full.names=TRUE)){source(f)}
for(f in list.files('./summaries', full.names=TRUE)){source(f)}

a1c_report <- tabPanel(
  title="HgA1c Analysis",
  titlePanel("Generate change in HgA1c reports"),
  sidebarLayout(
    sidebarPanel(
    fileInput("a1c_file_input", "Upload a CSV or XLSX file of the HgA1c lab results report on MDR", accept = c(".csv",".xlsx")),
    textInput("a1c_thres","Enter a minimum initial A1c to include in analysis",value="5"),
    textInput("a1c_final","Enter a final A1c value to include in analysis",value="7"),
    selectInput("a1c_group_select", "Select a variable to use for plotting", choices = c("Age","Gender")),
    downloadButton('download_a1c_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          fluidRow(
            column(width=3,strong(textOutput("a1c_stats_title"))),
            column(width=4,tableOutput("a1c_stats")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("a1c_bins_title"))),
            column(width=4,tableOutput("a1c_bins")),
          )
        ),
        tabPanel(
          title = "Plots",
          fluidRow(
            plotOutput("a1c_plot"),
            verbatimTextOutput("a1c_summary")
          )
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("a1c_data")
        ),
      )
    )
  )
)

bp_report <- tabPanel(
  title="Blood Pressure Analysis",
  titlePanel("Generate change in blood pressure reports"),
  sidebarLayout(
    sidebarPanel(
    fileInput("bp_file_input", "Upload a CSV or XLSX file of the blood pressure report on MDR", accept = c(".csv",".xlsx")),
    textInput("bp_thres","Enter a minimum initial systolic BP to include in analysis",value="80"),
    textInput("bp_final","Enter a final systolic BP to include in analysis",value="130"),
    selectInput("bp_group_select", "Select a variable to use for plotting", choices = c("Age")),
    downloadButton('download_bp_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          fluidRow(
            column(width=3,strong(textOutput("bp_stats_title"))),
            column(width=4,tableOutput("bp_stats")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("bp_bins_title"))),
            column(width=4,tableOutput("bp_bins")),
          )
        ),
        tabPanel(
          title = "Plots",
          fluidRow(
            plotOutput("bp_plot"),
            verbatimTextOutput("bp_summary")
          )          
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("bp_data")
        )
      )
    )
  )
)

hld_report <- tabPanel(
  title = "LDL Analysis",
  titlePanel("Generate change in LDL cholesterol"),
  sidebarLayout(
    sidebarPanel(
    fileInput("hld_file_input", "Upload a CSV or XLSX file of the LDL cholesterol report on MDR", accept = c(".csv",".xlsx")),
    textInput("hld_thres","Enter a minimum initial LDL level to include in analysis",value="50"),
    textInput("hld_final","Enter a final LDL level to include in analysis",value="125"),
    selectInput("hld_group_select", "Select a variable to use for plotting", choices = c("Age","Gender")),
    downloadButton('download_hld_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          fluidRow(
            column(width=3,strong(textOutput("hld_stats_title"))),
            column(width=4,tableOutput("hld_stats")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("hld_bins_title"))),
            column(width=4,tableOutput("hld_bins")),
          )
        ),
        tabPanel(
          title = "Plots",
          fluidRow(
            plotOutput("hld_plot"),
            verbatimTextOutput("hld_summary")
          )
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("hld_data")
        )
      )
    )
  )
)

tchol_report <- tabPanel(
  title = "Total Cholesterol Analysis",
  titlePanel("Generate change in total cholesterol"),
  sidebarLayout(
    sidebarPanel(
    fileInput("tchol_file_input", "Upload a CSV or XLSX file of the HDL cholesterol report on MDR", accept = c(".csv",".xlsx")),
    textInput("tchol_thres","Enter a minimum initial total cholesterol level to include in analysis",value="100"),
    textInput("tchol_final","Enter a final cholesterol level to include in analysis",value="200"),
    selectInput("tchol_group_select", "Select a variable to use for plotting", choices = c("Age","Gender")),
    downloadButton('download_tchol_data', 'Download data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
          title = "Summary statistics",
          fluidRow(
            column(width=3,strong(textOutput("tchol_stats_title"))),
            column(width=4,tableOutput("tchol_stats")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("tchol_bins_title"))),
            column(width=4,tableOutput("tchol_bins")),
          )
        ),
        tabPanel(
          title = "Plots",
          fluidRow(
            plotOutput("tchol_plot"),
            verbatimTextOutput("tchol_summary")
          )
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("tchol_data")
        )
      )
    )
  )
)

cz_report <- tabPanel(
  title = "Corazones Sanos Analysis",
  titlePanel("Compare patients in Corazones Sanos to the general patient population"),
  sidebarLayout(
    sidebarPanel(
      fileInput("cz_file_input", "Upload a CSV or XLSX file of the HgA1c, BP, LDL, and total cholesterol lab results report on MDR",multiple=TRUE, accept = c(".csv",".xlsx")),
      downloadButton('download_cz_data', 'Download data')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(  
          title = "Summary statistics",
          fluidRow(
            column(width=3,strong(textOutput("impr_cz_bins_title"))),
            column(width=4,tableOutput("impr_cz_bins")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("impr_cz_stats_title"))),
            column(width=4,tableOutput("impr_cz_stats")),
          ),
          fluidRow(
            column(width=3,strong(textOutput("no_change_cz_bins_title"))),
            column(width=4,tableOutput("no_change_cz_bins")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("no_change_cz_stats_title"))),
            column(width=4,tableOutput("no_change_cz_stats")),
          ),
          fluidRow(
            column(width=3,strong(textOutput("worse_cz_bins_title"))),
            column(width=4,tableOutput("worse_cz_bins")),            
          ),
          fluidRow(
            column(width=3,strong(textOutput("worse_cz_stats_title"))),
            column(width=4,tableOutput("worse_cz_stats")),
          )
        ),
        tabPanel(
          title = "Processed data",
          DT::dataTableOutput("cz_data")
        ),
        tabPanel(
          title = "Uploaded files",
          DT::dataTableOutput("cz_files")
        )
      )
    )
  )
)

get_data <-function(input,file_input,page){
  a1c_thres <- input$a1c_thres
  a1c_final <- input$a1c_final    
  bp_thres <- input$bp_thres
  bp_final <- input$bp_final
  hld_thres <- input$hld_thres
  hld_final <- input$hld_final
  tchol_thres <- input$tchol_thres
  tchol_final <- input$tchol_final
  ext <- tools::file_ext(file_input$datapath)
  req(file_input)
  validate(need(ext == "csv" || ext == "xlsx", "Please upload a csv or xlsx file"))
  if(tools::file_ext(file_input$datapath)=="csv"){
    csv <- read.csv(file_input$datapath,head=TRUE)
    dataframe <- csv
  } else if(tools::file_ext(file_input$datapath)=="xlsx"){
    xl <- data.frame(readxl::read_excel(file_input$datapath))
    dataframe <- xl
  }
  if(page=="HgA1c Analysis"){
    data <- a1c_analysis(dataframe,ext,a1c_thres,a1c_final)
  }
  if(page=="Blood Pressure Analysis"){
    data <- bp_analysis(dataframe,ext,bp_thres,bp_final)
  }
  if(page=="LDL Analysis"){
    data <- hld_analysis(dataframe,ext,hld_thres,hld_final)
  }
  if(page=="Total Cholesterol Analysis"){
    data <- tchol_analysis(dataframe,ext,tchol_thres,tchol_final)
  }
  return(data)
}

ui <- navbarPage(
  title = "Select an analysis:",
  id = "navbarid",
  cz_report,
  a1c_report,
  bp_report,
  hld_report,
  tchol_report
)

get_odds_gender <- function(data,group){
  event_group = nrow(data[data$Gender==group & data$Final_result_below_threshold=='Yes',])
  non_event_group = nrow(data[data$Gender==group & data$Final_result_below_threshold!='Yes',])
  event_non_group = nrow(data[data$Gender!=group & data$Final_result_below_threshold=='Yes',])
  non_event_non_group = nrow(data[data$Gender!=group & data$Final_result_below_threshold!='Yes',])
  return(c(event_group,non_event_group,event_non_group,non_event_non_group))
}
get_odds_age <- function(data,group){
  event_group = nrow(data[data$age_quant_num==group & data$Final_result_below_threshold=='Yes',])
  non_event_group = nrow(data[data$age_quant_num==group & data$Final_result_below_threshold!='Yes',])
  event_non_group = nrow(data[data$age_quant_num!=group & data$Final_result_below_threshold=='Yes',])
  non_event_non_group = nrow(data[data$age_quant_num!=group & data$Final_result_below_threshold!='Yes',])
  return(c(event_group,non_event_group,event_non_group,non_event_non_group))
}
get_esc <- function(data,group,var){
  if(var=="gender"){
      res <- get_odds_gender(data,group)    
  }
  if(var=="age"){
      res <- get_odds_age(data,group)    
  }
  esc::esc_2x2(grp1yes = res[1], grp1no = res[2],
          grp2yes = res[3], grp2no = res[4],
          es.type = "logit",study=group)
}
do_meta_es <- function(combined_es,title){
  meta <- meta::metagen(TE = combined_es$es,
          seTE = combined_es$se,
          studlab = combined_es$study,
          data = combined_es,
          sm = "SMD",
          fixed = FALSE,
          random = TRUE,
          method.tau = "REML",
          hakn = TRUE,
          title = title)
  return(meta)
}
get_cz_data <- function(file_input){
 for(file in file_input$datapath){
    ext <- tools::file_ext(file)
    req(file)
    validate(need(ext == "csv" || ext == "xlsx", "Please upload a csv or xlsx file"))
    if(tools::file_ext(file)=="csv"){
      csv <- read.csv(file,head=TRUE)
      dataframe <- csv
    } else if(tools::file_ext(file)=="xlsx"){
      xl <- data.frame(readxl::read_excel(file))
      dataframe <- xl
    }
    if("Systolic" %in% colnames(dataframe)){
      bp_data <- bp_analysis(dataframe,ext,0,1000)
      bp_data$Final_result_below_threshold <- NULL
    }
    if(is.element("HbA1c MFr Bld",unlist(dataframe))){
      hga1c_data <- a1c_analysis(dataframe,ext,0,1000)
      hga1c_data$Final_result_below_threshold <- hga1c_data$Birth_Year <- NULL
    }
    if(is.element("LDLc SerPl Calc-mCnc",unlist(dataframe))){
      ldl_data <- hld_analysis(dataframe,ext,0,1000)
      ldl_data$Final_result_below_threshold <- ldl_data$Birth_Year <- ldl_data$Gender <- ldl_data$Race <- ldl_data$Ethnicity <- NULL
    }
    if(is.element("HDLc SerPl-mCnc",unlist(dataframe))){
      tchol_data <- tchol_analysis(dataframe,ext,0,1000)
      tchol_data$Final_result_below_threshold <- tchol_data$Birth_Year <- tchol_data$Gender <- tchol_data$Race <- tchol_data$Ethnicity <- NULL
    }
    if("Enrollment.year" %in% colnames(dataframe)){
      cz_enrollees <- dataframe
      colnames(cz_enrollees)[5] <- "Chart_num"
      cz_enrollees$`Last.name` <- cz_enrollees$`First.name` <- cz_enrollees$DOB <- NULL
    }
  }
  if(exists("bp_data")==FALSE){
    stop("Please upload all 4 MDR reports and a list of CZ enrollees. The BP report is missing.")
  } else if(exists("hga1c_data")==FALSE){
    stop("Please upload all 4 MDR reports and a list of CZ enrollees. The HgA1c report is missing.")
  } else if(exists("ldl_data")==FALSE){
    stop("Please upload all 4 MDR reports and a list of CZ enrollees. The LDL report is missing.")
  } else if(exists("tchol_data")==FALSE){
    stop("Please upload all 4 MDR reports and a list of CZ enrollees. The total cholesterol report is missing.")
  } else if(exists("cz_enrollees")==FALSE){
    stop("Please upload all 4 MDR reports and a list of CZ enrollees. A list of CZ enrollees is missing.")
  } else {
    combined_data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(bp_data,hga1c_data,ldl_data,tchol_data,cz_enrollees))
  } 
  return(combined_data)
}

server <- function(input, output, session) {
  observe({
    page <- input$navbarid 
    if(page=="HgA1c Analysis"){
      file_input <- input$a1c_file_input
      output$a1c_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$a1c_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        a1c_summary_stats(analysis,input$a1c_final)
      })
      output$a1c_bins <- renderTable({
        analysis <- get_data(input,file_input,page)
        a1c_summary_bins(analysis,input$a1c_thres)
      })
      output$a1c_stats_title <- renderText({
        "Summary statistics:"
      })
      output$a1c_bins_title <- renderText({
        "Number of patients with a final value of:"
      }) 
      output$a1c_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5250, 
          height = 2750,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$a1c_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving HgA1c below ",input$a1c_final))
          meta::forest.meta(meta,
                            print.tau2 = FALSE,
                            leftlabs = c("Gender", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Gender and odds of improving HgA1c below ",input$a1c_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        if(input$a1c_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          age_quant <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
            age_quant <- c(age_quant,x)
          }
          analysis$age_quant_num <- age_quant_num  
          analysis$age_quant <- age_quant
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving HgA1c below ",input$a1c_final))
          meta::forest.meta(meta,
                            sortvar=unique(age_quant),
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Age and odds of improving HgA1c below ",input$a1c_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=750,
             height=400)
      },deleteFile = TRUE)
      output$a1c_summary <- renderPrint({
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$a1c_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving HgA1c below ",input$a1c_final))
        }
        if(input$a1c_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving HgA1c below ",input$a1c_final))
        }
        meta
      }) 
    }
    if(page=="Blood Pressure Analysis"){
      file_input <- input$bp_file_input
      output$bp_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$bp_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        bp_summary_stats(analysis,input$bp_final)
      })
      output$bp_bins <- renderTable({
        analysis <- get_data(input,file_input,page)
        bp_summary_bins(analysis,input$bp_thres)
      })
      output$bp_stats_title <- renderText({
        "Summary statistics:"
      })
      output$bp_bins_title <- renderText({
        "Number of patients with a final value of:"
      })
      output$bp_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5250, 
          height = 2750,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$bp_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          age_quant <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
            age_quant <- c(age_quant,x)
          }
          analysis$age_quant_num <- age_quant_num  
          analysis$age_quant <- age_quant
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving Blood Pressure below ",input$bp_final))
          meta::forest.meta(meta,
                            sortvar=unique(age_quant),
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Age and odds of improving Blood Pressure below ",input$bp_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=750,
             height=400)
      },deleteFile = TRUE)
      output$bp_summary <- renderPrint({
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$bp_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving Blood Pressure below ",input$bp_final))
        }
        meta
      })
    }
    if(page=="LDL Analysis"){
      file_input <- input$hld_file_input
      output$hld_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$hld_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        hld_summary_stats(analysis,input$hld_final)
      })
      output$hld_bins <- renderTable({
        analysis <- get_data(input,file_input,page)
        hld_summary_bins(analysis,input$hld_thres)
      })
      output$hld_stats_title <- renderText({
        "Summary statistics:"
      })
      output$hld_bins_title <- renderText({
        "Number of patients with a final value of:"
      })
      output$hld_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5250, 
          height = 2750,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$hld_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving LDL cholesterol below ",input$hld_final))
          meta::forest.meta(meta,
                            print.tau2 = FALSE,
                            leftlabs = c("Gender", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Gender and odds of improving LDL cholesterol below ",input$hld_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        if(input$hld_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          age_quant <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
            age_quant <- c(age_quant,x)
          }
          analysis$age_quant_num <- age_quant_num  
          analysis$age_quant <- age_quant
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving LDL cholesterol below ",input$hld_final))
          meta::forest.meta(meta,
                            sortvar=unique(age_quant),
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Age and odds of improving LDL cholesterol below ",input$hld_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=750,
             height=400)
      },deleteFile = TRUE)
      output$hld_summary <- renderPrint({
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$hld_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving LDL cholesterol below ",input$hld_final))
        }
        if(input$hld_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving LDL cholesterol below ",input$hld_final))
        }
        meta
      })
    }
    if(page=="Total Cholesterol Analysis"){ 
      file_input <- input$tchol_file_input
      output$tchol_data <- DT::renderDataTable({
        get_data(input,file_input,page)
      })
      output$tchol_stats <- renderTable({
        analysis <- get_data(input,file_input,page)
        tchol_summary_stats(analysis,input$tchol_final)
      })
      output$tchol_bins <- renderTable({
        analysis <- get_data(input,file_input,page)
        tchol_summary_bins(analysis,input$tchol_thres)
      })
      output$tchol_stats_title <- renderText({
        "Summary statistics:"
      })
      output$tchol_bins_title <- renderText({
        "Number of patients with a final value of:"
      })
      output$tchol_plot <- renderImage({
        outfile <- tempfile(fileext = '.png')
        png(outfile, 
          width = 5250, 
          height = 2750,
          res = 50*10)
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$tchol_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving total cholesterol below ",input$tchol_final))
          meta::forest.meta(meta,
                            print.tau2 = FALSE,
                            leftlabs = c("Gender", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Gender and odds of improving total cholesterol below ",input$tchol_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        if(input$tchol_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          age_quant <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) <= age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
            age_quant <- c(age_quant,x)
          }
          analysis$age_quant_num <- age_quant_num  
          analysis$age_quant <- age_quant
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving total cholesterol below ",input$tchol_final))
          meta::forest.meta(meta,
                            sortvar=unique(age_quant),
                            print.tau2 = FALSE,
                            leftlabs = c("Age", "g", "SE"),
                            fontsize=16,
                            label.left = "<- Less likely",
                            label.right = "More likely ->",
                            spacing = 1.5
                           )
          grid::grid.text(paste0("Age and odds of improving total cholesterol below ",input$tchol_final), x=0.5,y=0.9, gp=gpar(fontsize=18))
        }
        dev.off()
        list(src = outfile,
             width=750,
             height=400)
      },deleteFile = TRUE)
      output$tchol_summary <- renderPrint({
        analysis <- get_data(input,file_input,page)
        esc_array <- list()
        i<-1
        if(input$tchol_group_select=="Gender"){
          for(group in unique(analysis$Gender)){
            esc_array[[i]] <- get_esc(analysis,group,"gender")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2])
          meta <- do_meta_es(combined_es,paste0("Gender and odds of improving total cholesterol below ",input$tchol_final))
        }
        if(input$tchol_group_select=="Age"){
          age_quants <- quantile(as.numeric(analysis$Birth_Year),probs=c(0,0.25,0.5,0.75,1))
          age_quant_num <- vector()
          for(chart in analysis$Chart_num){
            this_chart <- analysis[analysis$Chart_num==chart,]
            if(age_quants[[1]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[2]]){
              x <- 1
            }
            if(age_quants[[2]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[3]]){
              x <- 2
            }
            if(age_quants[[3]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[4]]){
              x <- 3
            }
            if(age_quants[[4]] <= as.numeric(this_chart$Birth_Year) & as.numeric(this_chart$Birth_Year) < age_quants[[5]]){
              x <- 4
            }
            age_quant_num <- c(age_quant_num,paste0("Age quantile ",x," - ",age_quants[[x]], " to ",age_quants[[x+1]],""))
          }
          analysis$age_quant_num <- age_quant_num
          for(group in unique(analysis$age_quant_num)){
            esc_array[[i]] <- get_esc(analysis,group,"age")
            i <- i+1
          }
          combined_es <- esc::combine_esc(esc_array[1],esc_array[2],esc_array[3],esc_array[4])
          meta <- do_meta_es(combined_es,paste0("Age and odds of improving total cholesterol below ",input$tchol_final))
        }
        meta
      })
    }
    if(page=="Corazones Sanos Analysis"){
      file_input <- input$cz_file_input
      output$cz_files <- DT::renderDataTable({file_input})
      output$cz_data <- DT::renderDataTable({
        get_cz_data(file_input)
      })
      output$impr_cz_bins_title <- renderText({
        "Summary overview of improvement:"
      })
      output$impr_cz_bins <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_bins(data,"Improved")
      })
      output$impr_cz_stats_title <- renderText({
        "Relative risk and 95% CI for improvement for patients in CZ compared to patients not in CZ:"
      })
      output$impr_cz_stats <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_stats(data,"Improved")
      })
      output$no_change_cz_bins_title <- renderText({
        "Summary overview of no change:"
      })
      output$no_change_cz_bins <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_bins(data,"No change")
      })
      output$no_change_cz_stats_title <- renderText({
        "Relative risk and 95% CI for no change for patients in CZ compared to patients not in CZ:"
      })
      output$no_change_cz_stats <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_stats(data,"No change")
      })
      output$worse_cz_bins_title <- renderText({
        "Summary overview of worsening:"
      })
      output$worse_cz_bins <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_bins(data,"Not improved")
      })
      output$worse_cz_stats_title <- renderText({
        "Relative risk and 95% CI for who worsened for patients in CZ compared to patients not in CZ:"
      })
      output$worse_cz_stats <- renderTable({
        data <- get_cz_data(file_input)
        cz_summary_stats(data,"Not improved")
      })
    }
  })
  
  observe({
    page <- input$navbarid 
    if(page=="HgA1c Analysis"){
      file_input <- input$a1c_file_input
      output$download_a1c_data <- downloadHandler(
        filename = function() {
          paste("HgA1c-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
    if(page=="Blood Pressure Analysis"){
      file_input <- input$bp_file_input
      output$download_bp_data <- downloadHandler(
        filename = function() {
          paste("BP-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
    if(page=="LDL Analysis"){
      file_input <- input$hld_file_input
      output$download_hld_data <- downloadHandler(
        filename = function() {
          paste("HLD-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
    if(page=="Total Cholesterol Analysis"){
      file_input <- input$tchol_file_input
      output$download_tchol_data <- downloadHandler(
        filename = function() {
          paste("TChol-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_data(input,file_input,page), file)
        }
      )
    }
    if(page=="Corazones Sanos Analysis"){
      file_input <- input$cz_file_input
      output$download_cz_data <- downloadHandler(
        filename = function() {
          paste("CZ-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(get_cz_data(file_input), file)
        }
      )
    }
  })

  autoInvalidate <- reactiveTimer(50000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
}

shinyApp(ui, server)

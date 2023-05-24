#' Generate blood pressure analysis from MDR BP data
#'
#' @param dataframe			A dataframe of the Lab Resulsts reports from MDR
#' @param type				The type of file passed to the analysis. This is clunky and will be 
#'							revised once I have a better understanding on module management in shiny. 
#' @param changeThreshold	The amount a patient's blood pressure needs to change in order to be 
#' 							determined significant. Note - HTN is defined as 130/80. 
#' @return 					A data frame with A1c change results. Idenitfying patient information 
#'							is not analysed or returned. 

bp_analysis <- function(dataframe,type,changeThreshold){
	chartNums = unique(dataframe$`Chart.`)
	bpReport <- data.frame(matrix(ncol = 9, nrow = 0))
	colnames(bpReport) <- c('Chart_num','Birth_Year','First_Reading_Date','First_Reading_Value','Last_Reading_Date','Last_Reading_Value','Change')

	print(dataframe)

	for(chart in chartNums){
		patient = dataframe[dataframe$`Chart.`==chart,]
		results = c(patient$`Chart.`,patient$DOB,patient$Visit.Date,patient$Systolic,patient$Diastolic)

	}

}
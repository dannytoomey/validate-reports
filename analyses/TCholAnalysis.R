#' Generate LDL analysis from MDR Lab Resulsts data
#'
#' @param dataframe		A dataframe of the Lab Resulsts reports from MDR
#' @param type			The type of file passed to the analysis. This is clunky and will be 
#'						revised once I have a better understanding on module management in shiny. 
#' @param TCholThreshold  A cutoff value to exclude patients from analysis.
#' 						Patients whose first total cholesterol within the given range is below this
#' 						value will not be included in analysis. 
#' @param TCholFinalValue A final value to define a significant change.
#' @return 				A data frame with total cholesterol change results. Idenitfying patient information 
#'						is not analysed or returned. 

tchol_analysis <- function(dataframe,type,TCholThreshold,TCholFinalValue){

	TCholThreshold <- as.numeric(TCholThreshold)
	if(is.na(TCholThreshold)){
		stop("Please input a value for total cholesterol threshold.")
	}
	TCholFinalValue <- as.numeric(TCholFinalValue)
	if(is.na(TCholFinalValue)){
		stop("Please input a value for final total cholesterol value.")
	}
	if(FALSE %in% (c("Chart.","DOB","Gender","Race","Ethnicity","Result.Date","Result.Value","Test.Name") %in% colnames(dataframe))){
		stop("The column names in the uploaded file do not match the expected column names for this MDR output. Please check that the correct report is uploaded.")
	}
	
	chartNums = unique(dataframe$`Chart.`)
	TCholReport <- data.frame(matrix(ncol = 11, nrow = 0))
	colnames(TCholReport) <- c('Chart_num','Birth_Year','Gender','Race','Ethnicity','First_Result_Date','First_Result_Value','Last_Result_Date','Last_Result_Value','Change','Final_result_below_threshold')

	for(chart in chartNums){
		patient = dataframe[dataframe$`Chart.`==chart,]
		results = c(patient$`Chart.`,patient$DOB,patient$Gender,patient$Race,patient$Ethnicity,patient$Result.Date,patient$Result.Value,patient$Test.Name)
		array <- data.frame(matrix(ncol = 8, nrow = 0))
		colnames(array) <- c('Chart_num','DOB','Gender','Race','Ethnicity','Result.Date','Result.Value','Test.Name')
		if(length(patient$`Chart.`)>3 & NA %in% patient$Result.Value==FALSE){

			for(visit in 1:length(patient$`Chart.`)){
				extract <- c(visit,
							 visit+length(patient$`Chart.`),
							 visit+length(patient$`Chart.`)*2,
							 visit+length(patient$`Chart.`)*3,
							 visit+length(patient$`Chart.`)*4,
							 visit+length(patient$`Chart.`)*5,
							 visit+length(patient$`Chart.`)*6,
							 visit+length(patient$`Chart.`)*7
							)
				array[nrow(array)+1,] <- c(results[extract])
				if(visit==3){
					first_result_date <- paste0(patient$Result.Date[1])
					first_result_value <- as.numeric(array[nrow(array),]$Result.Value)+as.numeric(array[nrow(array)-1,]$Result.Value)
				}
				if(exists('first_result_value')){
					if(visit==length(patient$`Chart.`)&first_result_value>TCholThreshold){
						last_result_date <- paste0(patient$Result.Date[length(patient$Result.Date)])
						last_result_value <- as.numeric(array[nrow(array),]$Result.Value)+as.numeric(array[nrow(array)-1,]$Result.Value)
						if(first_result_value>last_result_value){
							change <- "Improved"
						}
						if(first_result_value<last_result_value){
							change <- "Not improved"
						}
						if(first_result_value==last_result_value){
							change <- "No change"
						}
						if(last_result_value<TCholFinalValue){
							final_result_below_threshold <- "Yes"
						}
						if(last_result_value>=TCholFinalValue){
							final_result_below_threshold <- "No"
						}

						if(type=="csv"){
							if(as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))<=23){
								age <- as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))+2000
							} else {
								age <- as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))+1900
							}

							date_1 <- as.Date(first_result_date,format="%m/%d/%Y %H:%M")
							date_2 <- as.Date(last_result_date,format="%m/%d/%Y %H:%M")
							if(date_2-date_1>14){
								entry <- c(array[nrow(array),]$Chart_num,
										   age,
										   array[nrow(array),]$Gender,
										   array[nrow(array),]$Race,
										   array[nrow(array),]$Ethnicity,
										   first_result_date,
										   first_result_value,
										   last_result_date,
										   last_result_value,
										   change,
										   final_result_below_threshold
										  )
								TCholReport[nrow(TCholReport)+1,] <- entry						
							}

						} else if(type=="xlsx"){
							date_1 <- as.Date(first_result_date,format="%Y-%m-%d %H:%M:%S")
							date_2 <- as.Date(last_result_date,format="%Y-%m-%d %H:%M:%S")
							if(date_2-date_1>14){
								first_result_date <- substring(first_result_date,1,10)
								last_result_date <- substring(last_result_date,1,10)
								entry <- c(array[nrow(array),]$Chart_num,
										   substring(paste0(patient$DOB[1]),1,4),
										   array[nrow(array),]$Gender,
										   array[nrow(array),]$Race,
										   array[nrow(array),]$Ethnicity,
										   first_result_date,
										   first_result_value,
										   last_result_date,
										   last_result_value,
										   change,
										   final_result_below_threshold
										  )
								TCholReport[nrow(TCholReport)+1,] <- entry
							}
						}			
					}
				}	
			}
		}
	}

	return(TCholReport)
}


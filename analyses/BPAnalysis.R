#' Generate blood pressure analysis from MDR BP data
#'
#' @param dataframe		A dataframe of the Lab Resulsts reports from MDR
#' @param type			The type of file passed to the analysis. This is clunky and will be 
#'						revised once I have a better understanding on module management in shiny. 
#' @param BPThreshold   A cutoff value to exclude patients from analysis.
#' 						Patients whose first BP within the given range is below this
#' 						value will not be included in analysis. 
#' @param BPFinalValue  A final value to define a significant change.
#' @return 				A data frame with BP change results. Idenitfying patient information 
#'						is not analysed or returned. 

bp_analysis <- function(dataframe,type,BPThreshold,BPFinalValue){

	BPThreshold <- as.numeric(BPThreshold)
	if(is.na(BPThreshold)){
		stop("Please input a value for BP threshold.")
	}
	BPFinalValue <- as.numeric(BPFinalValue)
	if(is.na(BPFinalValue)){
		stop("Please input a value for final BP value.")
	}
	if(FALSE %in% (c("Chart.","DOB","Visit.Date","Systolic","Diastolic") %in% colnames(dataframe))){
		stop("The column names in the uploaded file do not match the expected column names for this MDR output. Please check that the correct report is uploaded.")
	}
	
	chartNums = unique(dataframe$`Chart.`)
	BPReport <- data.frame(matrix(ncol = 10, nrow = 0))
	colnames(BPReport) <- c('Chart_num','Birth_Year','First_Result_Date','First_Result_Systolic','First_Result_Diastolic','Last_Result_Date','Last_Result_Systolic','Last_Result_Diastolic','Change','Final_result_below_threshold')

	for(chart in chartNums){
		patient = dataframe[dataframe$`Chart.`==chart,]
		results = c(patient$`Chart.`,patient$DOB,patient$Visit.Date,patient$Systolic,patient$Diastolic)
		array <- data.frame(matrix(ncol = 5, nrow = 0))
		colnames(array) <- c('Chart_num','DOB','Visit.Date','Result.Systolic','Result.Diastolic')
		if(length(patient$`Chart.`)>1){
			for(visit in 1:length(patient$`Chart.`)){
				extract <- c(visit,
							 visit+length(patient$`Chart.`),
							 visit+length(patient$`Chart.`)*2,
							 visit+length(patient$`Chart.`)*3,
							 visit+length(patient$`Chart.`)*4
							)
				array[nrow(array)+1,] <- c(results[extract])
				if(visit==1){
					first_result_date <- paste0(patient$Visit.Date[1])
					first_result_systolic <- as.numeric(array[nrow(array),]$Result.Systolic)
					first_result_diastolic <- as.numeric(array[nrow(array),]$Result.Diastolic)
					if(first_result_diastolic>first_result_systolic){
						d2s <- first_result_diastolic
						s2d <- first_result_systolic
						first_result_diastolic <- s2d
						first_result_systolic <- d2s
					}
				}
				if(visit==length(patient$`Chart.`)&first_result_systolic>BPThreshold){
					last_result_date <- paste0(patient$Visit.Date[length(patient$Visit.Date)])
					last_result_systolic <- as.numeric(array[nrow(array),]$Result.Systolic)
					last_result_diastolic <- as.numeric(array[nrow(array),]$Result.Diastolic)
					if(last_result_diastolic>last_result_systolic){
						d2s <- last_result_diastolic
						s2d <- last_result_systolic
						last_result_diastolic <- s2d
						last_result_systolic <- d2s
					}

					if(first_result_systolic>last_result_systolic){
						change <- "Improved"
					}
					if(first_result_systolic<last_result_systolic){
						change <- "Not improved"
					}
					if(first_result_systolic==last_result_systolic){
						change <- "No change"
					}
					if(last_result_systolic<BPFinalValue){
						final_result_below_threshold <- "Yes"
					}
					if(last_result_systolic>=BPFinalValue){
						final_result_below_threshold <- "No"
					}
					
					if(type=="csv"){
						entry <- c(array[nrow(array),]$Chart_num,
								   substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)),
								   first_result_date,
								   first_result_systolic,
								   first_result_diastolic,
								   last_result_date,
								   last_result_systolic,
								   last_result_diastolic,
								   change,
								   final_result_below_threshold
								  )
					} else if(type=="xlsx"){
						first_result_date <- substring(first_result_date,1,10)
						last_result_date <- substring(last_result_date,1,10)
						entry <- c(array[nrow(array),]$Chart_num,
								   substring(paste0(patient$DOB[1]),1,4),
								   first_result_date,
								   first_result_systolic,
								   first_result_diastolic,
								   last_result_date,
								   last_result_systolic,
								   last_result_diastolic,
								   change,
								   final_result_below_threshold
								  )
					}
					
					BPReport[nrow(BPReport)+1,] <- entry
										
				}	
			}	
		}
	}

	return(BPReport)
}
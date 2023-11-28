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

quantile_df <- function(x, probs, na.rm =F, names = F, type = 7, ...){
  z <- quantile(x, probs, na.rm, names, type)
  return(data.frame(id = probs, values = z))
}

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
	colnames(BPReport) <- c('Chart_num','Birth_Year','BP_First_Result_Date','Systolic_First_Result','Diastolic_First_Result','BP_Last_Result_Date','Systolic_Last_Result','Diastolic_Last_Result','BP_Change','Final_result_below_threshold')
	
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

					if(type=="csv"){
						if(as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))<=23){
							age <- as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))+2000
						} else {
							age <- as.numeric(substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)))+1900
						}
						#print(first_result_date)
						date_1 <- as.Date(first_result_date,format="%m/%d/%Y")
						date_2 <- as.Date(last_result_date,format="%m/%d/%Y")
						if(date_1>date_2){
							frda_store <- last_result_date
							frsy_store <- last_result_systolic
							frdy_store <- last_result_diastolic
							lrda_store <- first_result_date
							lrsy_store <- first_result_systolic
							lrdy_store <- first_result_diastolic
						} else {
							lrda_store <- last_result_date
							lrsy_store <- last_result_systolic
							lrdy_store <- last_result_diastolic
							frda_store <- first_result_date
							frsy_store <- first_result_systolic
							frdy_store <- first_result_diastolic
						}

						if(frsy_store>lrsy_store){
							change <- "Improved"
						}
						if(frsy_store<lrsy_store){
							change <- "Not improved"
						}
						if(frsy_store==lrsy_store){
							change <- "No change"
						}
						if(lrsy_store<BPFinalValue){
							final_result_below_threshold <- "Yes"
						}
						if(lrsy_store>=BPFinalValue){
							final_result_below_threshold <- "No"
						}
						entry <- c(array[nrow(array),]$Chart_num,
								   age,
								   frda_store,
								   frsy_store,
								   frdy_store,
								   lrda_store,
								   lrsy_store,
								   lrdy_store,
								   change,
								   final_result_below_threshold
								  )
					} else if(type=="xlsx"){
						first_result_date <- substring(first_result_date,1,10)
						last_result_date <- substring(last_result_date,1,10)
						date_1 <- as.Date(first_result_date,format="%Y-%m-%d")
						date_2 <- as.Date(last_result_date,format="%Y-%m-%d")
						if(date_1>date_2){
							frda_store <- last_result_date
							frsy_store <- last_result_systolic
							frdy_store <- last_result_diastolic
							lrda_store <- first_result_date
							lrsy_store <- first_result_systolic
							lrdy_store <- first_result_diastolic
						} else {
							lrda_store <- last_result_date
							lrsy_store <- last_result_systolic
							lrdy_store <- last_result_diastolic
							frda_store <- first_result_date
							frsy_store <- first_result_systolic
							frdy_store <- first_result_diastolic
						}

						if(frsy_store>lrsy_store){
							change <- "Improved"
						}
						if(frsy_store<lrsy_store){
							change <- "Not improved"
						}
						if(frsy_store==lrsy_store){
							change <- "No change"
						}
						if(lrsy_store<BPFinalValue){
							final_result_below_threshold <- "Yes"
						}
						if(lrsy_store>=BPFinalValue){
							final_result_below_threshold <- "No"
						}
						entry <- c(array[nrow(array),]$Chart_num,
								   substring(paste0(patient$DOB[1]),1,4),
								   frda_store,
								   frsy_store,
								   frdy_store,
								   lrda_store,
								   lrsy_store,
								   lrdy_store,
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
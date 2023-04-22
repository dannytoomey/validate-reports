#' Generate HgA1c analysis from MDR Lab Resulsts data
#'
#' @param csv			A csv file of the Lab Resulsts reports from MDR
#' @param A1cThreshold  A cutoff value to exclude patients from analysis.
#' 						Patients whose first A1c within the given range is below this
#' 						value will not be included in analysis. 
#' @return 				A data frame with A1c change results. Idenitfying patient information 
#'						is not analysed or returned. 

a1c_analysis <- function(csv,A1cThreshold){

	csv = read.csv(csv,head=TRUE)

	chartNums = unique(csv$`Chart.`)

	A1cReport <- data.frame(matrix(ncol = 9, nrow = 0))
	colnames(A1cReport) <- c('Chart_num','Birth_Year','Race','Ethnicity','First_Result_Date','First_Result_Value','Last_Result_Date','Last_Result_Value','Change')

	for(chart in chartNums){
		patient = csv[csv$`Chart.`==chart,]
		results = c(patient$`Chart.`,patient$DOB,patient$Race,patient$Ethnicity,patient$Result.Date,patient$Result.Value)
		array <- data.frame(matrix(ncol = 7, nrow = 0))
		colnames(array) <- c('Chart_num','DOB','Race','Ethnicity','Result.Date','Result.Value')
		if(length(patient$`Chart.`)>1){
			for(visit in 1:length(patient$`Chart.`)){
				extract <- c(visit,
							 visit+length(patient$`Chart.`),
							 visit+length(patient$`Chart.`)*2,
							 visit+length(patient$`Chart.`)*3,
							 visit+length(patient$`Chart.`)*4,
							 visit+length(patient$`Chart.`)*5,
							 visit+length(patient$`Chart.`)*6
							)
				array[nrow(array)+1,] <- c(results[extract])
				if(visit==1){
					first_result_date <- array[nrow(array),]$Result.Date
					first_result_value <- as.numeric(array[nrow(array),]$Result.Value)
					
				}
				if(visit==length(patient$`Chart.`)&first_result_value>A1cThreshold){
					last_result_date <- array[nrow(array),]$Result.Date
					last_result_value <- as.numeric(array[nrow(array),]$Result.Value)

					if(first_result_value>last_result_value){
						change <- "Improved"
					}
					if(first_result_value<last_result_value){
						change <- "Not improved"
					}
					if(first_result_value==last_result_value){
						change <- "No change"
					}
					
					entry <- c(array[nrow(array),]$Chart_num,
							   substr(array[nrow(array),]$DOB, nchar(array[nrow(array),]$DOB)-2+1, nchar(array[nrow(array),]$DOB)),
							   array[nrow(array),]$Race,
							   array[nrow(array),]$Ethnicity,
							   first_result_date,
							   first_result_value,
							   last_result_date,
							   last_result_value,
							   change
							  )

					A1cReport[nrow(A1cReport)+1,] <- entry
					
				}	
			}	
		}
	}

	return(A1cReport)
}


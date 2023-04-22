source("./analyses/A1cAnalysis.R")

a1c_summary_stats <- function(df){
	n_improved <- sum(df$Change == "Improved")
	n_no_change <- sum(df$Change == "No change")
	n_decreased <- sum(df$Change == "Not improved")
	table <- data.frame(Change=c('Improved','No change','Worsened'),
						Number=c(n_improved,n_no_change,n_decreased))

	return(table)
}
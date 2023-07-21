bp_summary_stats <- function(df,finalValue){
	n_imp_below_thres <-  sum(df$Change == "Improved" & df$Final_result_below_threshold == "Yes")
	n_improved <- sum(df$Change == "Improved")
	n_no_change <- sum(df$Change == "No change")
	n_decreased <- sum(df$Change == "Not improved")
	table <- data.frame(Change=c(paste0('Improved below ',finalValue),'Improved','No change','Worsened'),
						Number=c(n_imp_below_thres,n_improved,n_no_change,n_decreased))
	return(table)
}

bp_summary_bins <- function(df,initialValue){
	bins <- vector()
	vals <- vector()
	for(val in seq(initialValue,200,10)){
		bin_label <- paste0("",val," - ",val+10,"")
		n_in_bin <- sum(val < df$Last_Result_Systolic & df$Last_Result_Systolic <= val+10)
		bins <- c(bins,bin_label)
		vals <- c(vals,n_in_bin)
	}
	table <- data.frame(Bin=bins,
						Frequency=vals)
	return(table)

}
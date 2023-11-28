bp_summary_stats <- function(df,finalValue){
	n_imp_below_thres <-  sum(df$BP_Change == "Improved" & df$Final_result_below_threshold == "Yes")
	n_improved <- sum(df$BP_Change == "Improved")
	n_no_change <- sum(df$BP_Change == "No change")
	n_decreased <- sum(df$BP_Change == "Not improved")
	table <- data.frame(Change=c(paste0('Improved below ',finalValue),'Improved','No change','Worsened'),
						Number=c(n_imp_below_thres,n_improved,n_no_change,n_decreased))
	return(table)
}

bp_summary_bins <- function(df,initialValue){
	bins <- vector()
	vals <- vector()
	for(val in seq(70,190,10)){
		bin_label <- paste0("",val," - ",val+10,"")
		n_in_bin <- sum(val < df$Systolic_Last_Result & df$Systolic_Last_Result <= val+10)
		bins <- c(bins,bin_label)
		vals <- c(vals,n_in_bin)
	}
	table <- data.frame(Bin=bins,
						Frequency=vals)
	return(table)

}
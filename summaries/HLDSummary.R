hld_summary_stats <- function(df,finalValue){
	n_imp_below_thres <-  sum(df$LDL_Change == "Improved" & df$Final_result_below_threshold == "Yes")
	n_improved <- sum(df$LDL_Change == "Improved")
	n_no_change <- sum(df$LDL_Change == "No change")
	n_decreased <- sum(df$LDL_Change == "Not improved")
	table <- data.frame(Change=c(paste0('Improved below ',finalValue),'Improved','No change','Worsened'),
						Number=c(n_imp_below_thres,n_improved,n_no_change,n_decreased))
	return(table)
}

hld_summary_bins <- function(df,initialValue){
	bins <- vector()
	vals <- vector()
	for(val in seq(25,275,25)){
		bin_label <- paste0("",val," - ",val+25,"")
		n_in_bin <- sum(val < df$LDL_Last_Result_Value & df$LDL_Last_Result_Value <= val+25)
		bins <- c(bins,bin_label)
		vals <- c(vals,n_in_bin)
	}
	table <- data.frame(Bin=bins,
						Frequency=vals)
	return(table)

}
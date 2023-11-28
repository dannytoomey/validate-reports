a1c_summary_stats <- function(df,finalValue){
	n_imp_below_thres <-  sum(df$HgA1c_Change == "Improved" & df$Final_result_below_threshold == "Yes")
	n_improved <- sum(df$HgA1c_Change == "Improved")
	n_no_change <- sum(df$HgA1c_Change == "No change")
	n_decreased <- sum(df$HgA1c_Change == "Not improved")
	table <- data.frame(Change=c(paste0('Improved below ',finalValue),'Improved','No change','Worsened'),
						Number=c(n_imp_below_thres,n_improved,n_no_change,n_decreased))
	return(table)
}

a1c_summary_bins <- function(df,initialValue){
	bins <- vector()
	vals <- vector()
	for(val in 1:13){
		bin_label <- paste0("",val," - ",val+1,"")
		n_in_bin <- sum(val < df$HgA1c_Last_Result_Value & df$HgA1c_Last_Result_Value <= val+1)
		bins <- c(bins,bin_label)
		vals <- c(vals,n_in_bin)
	}
	table <- data.frame(Bin=bins,
						Frequency=vals)
	return(table)

}
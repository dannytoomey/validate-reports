cz_summary_bins <- function(df,status){
	cz_data <- df[!(is.na(df$Enrollment.year)),]
	non_cz_data <- df[(is.na(df$Enrollment.year)),]

	n_cz_impr_bp <- length(which(cz_data$BP_Change==status))
	n_cz_impr_a1c <- length(which(cz_data$HgA1c_Change==status))
	n_cz_impr_ldl <- length(which(cz_data$LDL_Change==status))
	n_cz_impr_tchol <- length(which(cz_data$TChol_Change==status))

	percent_cz_impr_bp <- n_cz_impr_bp / nrow(cz_data)
	percent_cz_impr_a1c <- n_cz_impr_a1c / nrow(cz_data)
	percent_cz_impr_ldl <- n_cz_impr_ldl / nrow(cz_data)
	percent_cz_impr_tchol <- n_cz_impr_tchol / nrow(cz_data)

	n_non_impr_bp <- length(which(non_cz_data$BP_Change==status))
	n_non_impr_a1c <- length(which(non_cz_data$HgA1c_Change==status))
	n_non_impr_ldl <- length(which(non_cz_data$LDL_Change==status))
	n_non_impr_tchol <- length(which(non_cz_data$TChol_Change==status))

	percent_non_impr_bp <- n_non_impr_bp / nrow(non_cz_data)
	percent_non_impr_a1c <- n_non_impr_a1c / nrow(non_cz_data)
	percent_non_impr_ldl <- n_non_impr_ldl / nrow(non_cz_data)
	percent_non_impr_tchol <- n_non_impr_tchol / nrow(non_cz_data)

	table <- data.frame(`Improvement in`=c("BP","HgA1c","LDL","Total Cholesterol"),
						`Patients in CZ`=c(n_cz_impr_bp,n_cz_impr_a1c,n_cz_impr_ldl,n_cz_impr_tchol),
						`Percent CZ`=c(percent_cz_impr_bp,percent_cz_impr_a1c,percent_cz_impr_ldl,percent_cz_impr_tchol),
						`Patients not enrolled`=c(n_non_impr_bp,n_non_impr_a1c,n_non_impr_ldl,n_non_impr_tchol),
						`Percent not CZ`=c(percent_non_impr_bp,percent_non_impr_a1c,percent_non_impr_ldl,percent_non_impr_tchol))

	colnames(table)[colnames(table)=="Improvement.in"] <- "Value of interest"
	colnames(table)[colnames(table)=="Patients.in.CZ"] <- paste0(status," patients in CZ")
	colnames(table)[colnames(table)=="Percent.CZ"] <- "Percent of CZ enrollees"
	colnames(table)[colnames(table)=="Patients.not.enrolled"] <- paste0(status," patients not enrolled")
	colnames(table)[colnames(table)=="Percent.not.CZ"] <- "Percent of patients not in CZ"

	return(table)
}

cz_summary_stats <- function(df,status){
	cz_data <- df[!(is.na(df$Enrollment.year)),]
	non_cz_data <- df[(is.na(df$Enrollment.year)),]

	percent_cz_impr_bp <- length(which(cz_data$BP_Change==status)) / nrow(cz_data)
	percent_cz_impr_a1c <- length(which(cz_data$HgA1c_Change==status)) / nrow(cz_data)
	percent_cz_impr_ldl <- length(which(cz_data$LDL_Change==status)) / nrow(cz_data)
	percent_cz_impr_tchol <- length(which(cz_data$TChol_Change==status)) / nrow(cz_data)

	percent_non_impr_bp <- length(which(non_cz_data$BP_Change==status)) / nrow(non_cz_data)
	percent_non_impr_a1c <- length(which(non_cz_data$HgA1c_Change==status)) / nrow(non_cz_data)
	percent_non_impr_ldl <- length(which(non_cz_data$LDL_Change==status)) / nrow(non_cz_data)
	percent_non_impr_tchol <- length(which(non_cz_data$TChol_Change==status)) / nrow(non_cz_data)

	rr_impr_bp <- percent_cz_impr_bp/percent_non_impr_bp
	rr_impr_alc <- percent_cz_impr_a1c/percent_non_impr_a1c
	rr_impr_ldl <- percent_cz_impr_ldl/percent_non_impr_ldl
	rr_impr_tchol <- percent_cz_impr_tchol/percent_non_impr_tchol

	lb_impr_bp <- exp(1)^log(rr_impr_bp)-1.96*sqrt(
		(1/length(which(cz_data$BP_Change==status)))+
		(1/length(which(non_cz_data$BP_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_alc <- exp(1)^log(rr_impr_alc)-1.96*sqrt(
		(1/length(which(cz_data$HgA1c_Change==status)))+
		(1/length(which(non_cz_data$HgA1c_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_ldl <- exp(1)^log(rr_impr_ldl)-1.96*sqrt(
		(1/length(which(cz_data$LDL_Change==status)))+
		(1/length(which(non_cz_data$LDL_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_tchol <- exp(1)^log(rr_impr_tchol)-1.96*sqrt(
		(1/length(which(cz_data$TChol_Change==status)))+
		(1/length(which(non_cz_data$TChol_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)

	ub_impr_bp <- exp(1)^log(rr_impr_bp)+1.96*sqrt(
		(1/length(which(cz_data$BP_Change==status)))+
		(1/length(which(non_cz_data$BP_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_alc <- exp(1)^log(rr_impr_alc)+1.96*sqrt(
		(1/length(which(cz_data$HgA1c_Change==status)))+
		(1/length(which(non_cz_data$HgA1c_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_ldl <- exp(1)^log(rr_impr_ldl)+1.96*sqrt(
		(1/length(which(cz_data$LDL_Change==status)))+
		(1/length(which(non_cz_data$LDL_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_tchol <- exp(1)^log(rr_impr_tchol)+1.96*sqrt(
		(1/length(which(cz_data$TChol_Change==status)))+
		(1/length(which(non_cz_data$TChol_Change==status)))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)

	table <- data.frame(`Outcome`=c("BP","HgA1c","LDL","Total Cholesterol"),
						`Relative risk`=c(rr_impr_bp,rr_impr_alc,rr_impr_ldl,rr_impr_tchol),
						`CI LB`=c(lb_impr_bp,lb_impr_alc,lb_impr_ldl,lb_impr_tchol),
						`CI UB`=c(ub_impr_bp,ub_impr_alc,ub_impr_ldl,ub_impr_tchol))

	colnames(table)[colnames(table)=="Outcome"] <- "Value of interest"
	colnames(table)[colnames(table)=="Relative.risk"] <- "Relative risk"
	colnames(table)[colnames(table)=="CI.LB"] <- "Lower bound CI"
	colnames(table)[colnames(table)=="CI.UB"] <- "Upper bound CI"

	return(table)
}


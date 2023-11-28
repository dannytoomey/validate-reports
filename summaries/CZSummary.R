cz_summary_bins <- function(df){
	cz_data <- df[!(is.na(df$Enrollment.year)),]
	non_cz_data <- df[(is.na(df$Enrollment.year)),]

	n_cz_impr_bp <- length(which(cz_data$BP_Change=="Improved"))
	n_cz_impr_a1c <- length(which(cz_data$HgA1c_Change=="Improved"))
	n_cz_impr_ldl <- length(which(cz_data$LDL_Change=="Improved"))
	n_cz_impr_tchol <- length(which(cz_data$TChol_Change=="Improved"))

	percent_cz_impr_bp <- n_cz_impr_bp / nrow(cz_data)
	percent_cz_impr_a1c <- n_cz_impr_a1c / nrow(cz_data)
	percent_cz_impr_ldl <- n_cz_impr_ldl / nrow(cz_data)
	percent_cz_impr_tchol <- n_cz_impr_tchol / nrow(cz_data)

	n_non_impr_bp <- length(which(non_cz_data$BP_Change=="Improved"))
	n_non_impr_a1c <- length(which(non_cz_data$HgA1c_Change=="Improved"))
	n_non_impr_ldl <- length(which(non_cz_data$LDL_Change=="Improved"))
	n_non_impr_tchol <- length(which(non_cz_data$TChol_Change=="Improved"))

	percent_non_impr_bp <- n_non_impr_bp / nrow(non_cz_data)
	percent_non_impr_a1c <- n_non_impr_a1c / nrow(non_cz_data)
	percent_non_impr_ldl <- n_non_impr_ldl / nrow(non_cz_data)
	percent_non_impr_tchol <- n_non_impr_tchol / nrow(non_cz_data)

	table <- data.frame(`Improvement in`=c("BP","HgA1c","LDL","Total Cholesterol"),
						`Impr patients in CZ`=c(n_cz_impr_bp,n_cz_impr_a1c,n_cz_impr_ldl,n_cz_impr_tchol),
						`Percent CZ`=c(percent_cz_impr_bp,percent_cz_impr_a1c,percent_cz_impr_ldl,percent_cz_impr_tchol),
						`Impr patients not enrolled`=c(n_non_impr_bp,n_non_impr_a1c,n_non_impr_ldl,n_non_impr_tchol),
						`Percent not CZ`=c(percent_non_impr_bp,percent_non_impr_a1c,percent_non_impr_ldl,percent_non_impr_tchol))
	return(table)
}

cz_summary_stats <- function(df){
	cz_data <- df[!(is.na(df$Enrollment.year)),]
	non_cz_data <- df[(is.na(df$Enrollment.year)),]

	percent_cz_impr_bp <- length(which(cz_data$BP_Change=="Improved")) / nrow(cz_data)
	percent_cz_impr_a1c <- length(which(cz_data$HgA1c_Change=="Improved")) / nrow(cz_data)
	percent_cz_impr_ldl <- length(which(cz_data$LDL_Change=="Improved")) / nrow(cz_data)
	percent_cz_impr_tchol <- length(which(cz_data$TChol_Change=="Improved")) / nrow(cz_data)

	percent_non_impr_bp <- length(which(non_cz_data$BP_Change=="Improved")) / nrow(non_cz_data)
	percent_non_impr_a1c <- length(which(non_cz_data$HgA1c_Change=="Improved")) / nrow(non_cz_data)
	percent_non_impr_ldl <- length(which(non_cz_data$LDL_Change=="Improved")) / nrow(non_cz_data)
	percent_non_impr_tchol <- length(which(non_cz_data$TChol_Change=="Improved")) / nrow(non_cz_data)

	rr_impr_bp <- percent_cz_impr_bp/percent_non_impr_bp
	rr_impr_alc <- percent_cz_impr_a1c/percent_non_impr_a1c
	rr_impr_ldl <- percent_cz_impr_ldl/percent_non_impr_ldl
	rr_impr_tchol <- percent_cz_impr_tchol/percent_non_impr_tchol

	lb_impr_bp <- exp(1)^log(rr_impr_bp)-1.96*sqrt(
		(1/length(which(cz_data$BP_Change=="Improved")))+
		(1/length(which(non_cz_data$BP_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_alc <- exp(1)^log(rr_impr_alc)-1.96*sqrt(
		(1/length(which(cz_data$HgA1c_Change=="Improved")))+
		(1/length(which(non_cz_data$HgA1c_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_ldl <- exp(1)^log(rr_impr_ldl)-1.96*sqrt(
		(1/length(which(cz_data$LDL_Change=="Improved")))+
		(1/length(which(non_cz_data$LDL_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	lb_impr_tchol <- exp(1)^log(rr_impr_tchol)-1.96*sqrt(
		(1/length(which(cz_data$TChol_Change=="Improved")))+
		(1/length(which(non_cz_data$TChol_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)

	ub_impr_bp <- exp(1)^log(rr_impr_bp)+1.96*sqrt(
		(1/length(which(cz_data$BP_Change=="Improved")))+
		(1/length(which(non_cz_data$BP_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_alc <- exp(1)^log(rr_impr_alc)+1.96*sqrt(
		(1/length(which(cz_data$HgA1c_Change=="Improved")))+
		(1/length(which(non_cz_data$HgA1c_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_ldl <- exp(1)^log(rr_impr_ldl)+1.96*sqrt(
		(1/length(which(cz_data$LDL_Change=="Improved")))+
		(1/length(which(non_cz_data$LDL_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)
	ub_impr_tchol <- exp(1)^log(rr_impr_tchol)+1.96*sqrt(
		(1/length(which(cz_data$TChol_Change=="Improved")))+
		(1/length(which(non_cz_data$TChol_Change=="Improved")))-
		(1/nrow(cz_data))-
		(1/nrow(non_cz_data))
	)

	table <- data.frame(`Outcome`=c("BP","HgA1c","LDL","Total Cholesterol"),
						`Relative risk`=c(rr_impr_bp,rr_impr_alc,rr_impr_ldl,rr_impr_tchol),
						`CI LB`=c(lb_impr_bp,lb_impr_alc,lb_impr_ldl,lb_impr_tchol),
						`CI UB`=c(ub_impr_bp,ub_impr_alc,ub_impr_ldl,ub_impr_tchol))
	return(table)
}


#this function contructs a data frame in wide format based on 
#the selected/specified FLEX experiment ID.
	#INPUT: array of FLEX_ID names
	#OUTPUT: dataframe in wide format
#NOTE: This function is used by "igExt_plots.R"

dataFrameConstruct <- function(csvfilenames, key, expInterestNames){

	indexList <- match(key$FLEX_ID, expInterestNames)
	indexList <- which(indexList != "NA")
	igExt_times <- key[indexList, ]

	# print(igExt_times)

	IG_timeIndexMatchGlobal <- numeric(nrow(igExt_times))
	EXT_timeIndexMatchGlobal <- numeric(nrow(igExt_times)) 
	FC_timeIndexMatchGlobal <- numeric(nrow(igExt_times))
	pointer <- numeric(nrow(igExt_times)-1)  #pointer to start of each data set in global data frame
	for (i in 1:nrow(igExt_times) ){
		expName <- igExt_times$FLEX_ID[i]
		currentExpData_index <- pmatch(expName,csvfilenames)
		currentExpData <- read.csv(file=csvfilenames[currentExpData_index])

		current_nu <- ( igExt_times$nu_g[i] )*100  #kinematic viscosity mm^2/s

		# create array of repeated experiment names to be used as factors in global dataframe
		expName_Array <- rep(expName,nrow(currentExpData))

		# print(i)
		# print(csvfilenames[currentExpData_index])
		# determine row in global matrix with time corresponding to 
		# the time of droplet IGNITION
		IG_timeIndexMatchLocal <- which.min(abs(currentExpData$time - igExt_times$T_IGNITION[i]))
		if (i == 1){
			IG_timeIndexMatchGlobal[i] <- IG_timeIndexMatchLocal
		}else{
			IG_timeIndexMatchGlobal[i] <- dataFrameSize + IG_timeIndexMatchLocal
		}

		# determine row in global matrix with time corresponding to 
		# the time of droplet EXTINCTION
		EXT_timeIndexMatchLocal <- which.min(abs(currentExpData$time - igExt_times$T_EXTINCTION[i]))
		if (i == 1){
			EXT_timeIndexMatchGlobal[i] <- EXT_timeIndexMatchLocal
		}else{
			EXT_timeIndexMatchGlobal[i] <- dataFrameSize + EXT_timeIndexMatchLocal
		}

		# determine row in global matrix with time corresponding to 
		# the time of droplet EXTINCTION
		FC_timeIndexMatchLocal <- which.min(abs(currentExpData$time - igExt_times$tofc[i]))
		if (i == 1){
			if (currentExpData$time[FC_timeIndexMatchLocal] < igExt_times$tofc[i] ){
					FC_timeIndexMatchGlobal[i] <- NA
				}else{
					FC_timeIndexMatchGlobal[i] <- FC_timeIndexMatchLocal
			}
		}else{
			if (currentExpData$time[FC_timeIndexMatchLocal] < igExt_times$tofc[i] ){
					FC_timeIndexMatchGlobal[i] <- NA
				}else{
					FC_timeIndexMatchGlobal[i] <- dataFrameSize + FC_timeIndexMatchLocal
			}			
			# FC_timeIndexMatchGlobal[i] <- dataFrameSize + FC_timeIndexMatchLocal
		}		

		# create temporary dataframe
		tempDataframe <- data.frame(expName_Array, 
							currentExpData$time,		# s
							currentExpData$k_bw1,       # mm^2/s
							currentExpData$tot_vel_fit, # mm/s 
							currentExpData$d2_bw1,		# mm^2
							sqrt(currentExpData$d2_bw1)*(currentExpData$tot_vel_fit)/current_nu, # Reynolds numbers
							currentExpData$tot_acc_fit,
							currentExpData$x_loc_fit,
							currentExpData$y_loc_fit,
							currentExpData$do,
							currentExpData$x_acc_fit,
							currentExpData$y_acc_fit)  

		# create array containing the velocities at extinction
		velocity_ext_temp <- rep(currentExpData$tot_vel_fit[EXT_timeIndexMatchLocal],nrow(currentExpData))

		# create array containing the acceleration at extinction
		acc_ext_temp <- rep(currentExpData$tot_acc_fit[EXT_timeIndexMatchLocal],nrow(currentExpData))

		# create array containing the times at extinction
		time_ext_temp <- rep(currentExpData$time[EXT_timeIndexMatchLocal],nrow(currentExpData))

		# create array containing the times at extinction
		fc_ext_temp <- rep(currentExpData$time[FC_timeIndexMatchLocal],nrow(currentExpData))

		if (i == 1){
			# create global data frame
			globalDataframe <- tempDataframe
			velocity_ext_global <- velocity_ext_temp
			time_ext_global <- time_ext_temp
			acc_ext_global <- acc_ext_temp
		}else{
			pointer[i-1] <- nrow(globalDataframe) + 1		# pointer indicating the start
															# of each dataframe in globalDataframe
			globalDataframe <- rbind(globalDataframe, tempDataframe)		
			velocity_ext_global <- abind(velocity_ext_global,velocity_ext_temp)
			time_ext_global <- abind(time_ext_global, time_ext_temp)
			acc_ext_global <- abind(acc_ext_global, acc_ext_temp)
		}

		dataFrameSize <- nrow(globalDataframe)
	} # end for i

	# set names in global data frame
	globalDataframe <- setNames(globalDataframe, c("expname","time","k_bw1","vel_mag",
						"d2_bw1","reynolds","total_acc","x_loc_fit","y_loc_fit","do",
						"x_acc_fit", "y_acc_fit") )

	# create array of string to indicate data points prior to ignition,
	# during, and after extinction
	timeIndicator <- rep('after_extinction',nrow(globalDataframe))
	for(i in 1:length(IG_timeIndexMatchGlobal)){
		timeIndicator[IG_timeIndexMatchGlobal[i]:EXT_timeIndexMatchGlobal[i]] <- 'combustion'
		if (i == 1){
			timeIndicator[1:(IG_timeIndexMatchGlobal[i]-1)] <- 'pre_ignition'
		}else{
			timeIndicator[ pointer[i-1]:(IG_timeIndexMatchGlobal[i]-1)] <- 'pre_ignition'
		}
	}


	# create array of string to indicate data points prior to ignition,
	# during, and after extinction
	timeIndicator <- rep('after_extinction',nrow(globalDataframe))
	for(i in 1:length(IG_timeIndexMatchGlobal)){
		timeIndicator[IG_timeIndexMatchGlobal[i]:EXT_timeIndexMatchGlobal[i]] <- 'combustion'
		if (i == 1){
			timeIndicator[1:(IG_timeIndexMatchGlobal[i]-1)] <- 'pre_ignition'
		}else{
			timeIndicator[ pointer[i-1]:(IG_timeIndexMatchGlobal[i]-1)] <- 'pre_ignition'
		}
	}

	globalDataframe <- cbind(globalDataframe, timeIndicator, 
		time_ext_global, velocity_ext_global, acc_ext_global)

	list( globalDataframe = globalDataframe, 
		IG_timeIndexMatchGlobal=IG_timeIndexMatchGlobal,
		EXT_timeIndexMatchGlobal = EXT_timeIndexMatchGlobal,
		FC_timeIndexMatchGlobal = FC_timeIndexMatchGlobal )	
}


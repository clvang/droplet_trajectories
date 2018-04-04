
rm(list=ls(all=TRUE))		#clear workspace of all variables

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
# library(cowplot)
library(ggpubr)
library(abind)

currentDirectory <- getwd()
setwd(currentDirectory)
graphics.off()

########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

#read in key containing ignition and extinction times of experiments
key <- read.csv(file = "/Users/changvang/mygitFiles/flex_experiment_information/ignitionExtinctionTimes.csv",
			head=TRUE, sep=",",
			stringsAsFactors=FALSE)

# specify which sets of experimetns to plot
#specify experiments with heptane droplets with cool falmes
# expInterestNames <- c("X126H02",
# 					"X129H01",
# 					"X140H02",
# 					"X140H05",
# 					"X144H01",
# 					"X144H02",
# 					"X170H01",
# 					"X170H03",
# 					"X177H01",
# 					"X177H02",
# 					"X177H05",
					# "X177H07")
# # #specify experiments with methanol, propanol/glycerol droplets (alcohol)
# expInterestNames <- c("C101D05",
# 					"C101D02",
# 					"C077D06",
# 					"C085D03",
# 					"C104D06",
# 					"X168M07",
# 					"X168M03",
# 					"X168M02",
# 					"X168M01",
# 					"X163M07",
# 					"X163M04",
# 					"X163M03",
# 					"X119M04",
# 					"X121M02",
# 					"X129M05",
# 					"X149M02",
# 					"X161M02",
# 					"X161M04",
# 					"X163M01")
#specify experiments with methanol droplets who have categroy 2 trajectories
expInterestNames <- c("X175M06",
					"X163M04",
					"X168M07",
					"X144M05",
					"X163M02",
					"X161M05",
					"X163M03",
					"X121M03", 
					"X144M01",
					"X144M06",
					"X175M04",  
					"X161M03",
					"X168M05",
					"X168M06",
					"X168M07",
					"X175M01",
					"X175M02",
					"X175M03",
					"X175M05")

indexList <- match(key$FLEX_ID, expInterestNames)
indexList <- which(indexList != "NA")
igExt_times <- key[indexList, ]

IG_timeIndexMatchGlobal <- numeric(nrow(igExt_times))
EXT_timeIndexMatchGlobal <- numeric(nrow(igExt_times)) 
pointer <- numeric(nrow(igExt_times)-1)  #pointer to start of each data set in global data frame
for (i in 1:nrow(igExt_times) ){
	expName <- igExt_times$FLEX_ID[i]
	currentExpData_index <- pmatch(expName,csvfilenames)
	currentExpData <- read.csv(file=csvfilenames[currentExpData_index])

	current_nu <- ( igExt_times$nu[i] )*100  #kinematic viscosity mm^2/s

	# create array of repeated experiment names to be used as factors in global dataframe
	expName_Array <- rep(expName,nrow(currentExpData))

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
						currentExpData$do)  

	# create array containing the velocities at extinction
	velocity_ext_temp <- rep(currentExpData$tot_vel_fit[EXT_timeIndexMatchLocal],nrow(currentExpData))

	# create array containing the acceleration at extinction
	acc_ext_temp <- rep(currentExpData$tot_acc_fit[EXT_timeIndexMatchLocal],nrow(currentExpData))

	# create array containing the times at extinction
	time_ext_temp <- rep(currentExpData$time[EXT_timeIndexMatchLocal],nrow(currentExpData))


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
					"d2_bw1","reynolds","total_acc","x_loc_fit","y_loc_fit","do") )

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

# overall velocity magnitude plot
velMagnitudePlot <- ggplot(globalDataframe )
velMagnitudePlot <- velMagnitudePlot + 
					geom_point(mapping=aes(x=time, y=vel_mag, colour=expname)) 
velMagnitudePlot <- velMagnitudePlot + 	
					geom_point(data=globalDataframe[IG_timeIndexMatchGlobal, ],
					 mapping=aes(x=time, y=vel_mag),
					 colour="black", size=3)
velMagnitudePlot <- velMagnitudePlot + 	
					geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
					 mapping=aes(x=time, y=vel_mag),
					 shape=17,
					 colour="black", size=3)					
velMagnitudePlot <- velMagnitudePlot + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("Time (s)") ) +
	ylab(expression("Velocity Magnitude (mm/s)") ) 

# size.w <- 10	    #specifies width of .pdf of plot in units specified by un
# size.h <- 6		#specifies height of .pdf of plot in units specified by un
# un <- "in"		#specifies unit of size.w and size.h
# ggsave(velMagnitudePlot, file="velocityMagnitudePlots.pdf", width=size.w, height=size.h, units=un)
# ggsave(velMagnitudePlot, file="velocityMagnitudePlots_heptaneCF.pdf", width=size.w, height=size.h, units=un)


bp <- ggplot(aes(x=timeIndicator,y=vel_mag), data=globalDataframe)
bp <- bp + theme_bw() + 
	  	stat_boxplot(geom='errorbar',coef=20) +
	  	geom_boxplot(aes(fill=timeIndicator), show.legend=FALSE, notch=FALSE,
	  	outlier.shape=NA )     #avoids plotting outliers twice  	
bp <- bp + theme(axis.title.x = element_blank(),
				axis.title.y = element_blank() )	  	
	  	# geom_jitter(position=position_jitter(width=.05, height=0))

combined <- ggarrange(velMagnitudePlot, bp,
          ncol = 2, nrow = 1,  align = "h", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = FALSE)
size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(combined, file="velocityMagnitudePlots.pdf", width=size.w, height=size.h, units=un)
#ggsave(combined, file="velocityMagnitudePlots_heptaneCF.pdf", width=size.w, height=size.h, units=un)


# overall velocity magnitude plot
sub_globalDataframe <- subset(globalDataframe, timeIndicator == "after_extinction" )

velMagnitudeNormalized <- ggplot(sub_globalDataframe)
velMagnitudeNormalized <- velMagnitudeNormalized + 
					geom_point(mapping=aes(x=time/time_ext_global, 
						y=vel_mag/velocity_ext_global, colour=expname)) 
# velMagnitudeNormalized <- velMagnitudeNormalized + 	
# 					geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
# 					 mapping=aes(x=time/time_ext_global[EXT_timeIndexMatchGlobal], 
# 					 	y=vel_mag/velocity_ext_global[EXT_timeIndexMatchGlobal]),
# 					 shape=17,
# 					 colour="black", size=3)					
velMagnitudeNormalized <- velMagnitudeNormalized + 	
	ggtitle("Normalized Velocities After Extinction") +
	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=12),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("Time/ Time at Extinction") ) +
	ylab(expression("Velocity Magnitude/ Velocity at Extinction") ) 
size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(velMagnitudeNormalized, file="velocityMagnitudeNormalized.pdf", width=size.w, height=size.h, units=un)


dropAccPlot <- ggplot(globalDataframe)
dropAccPlot <- dropAccPlot + 
					geom_point(mapping=aes(x=time, 
						y=total_acc, colour=expname)) 
dropAccPlot <- dropAccPlot + 	
					geom_point(data=globalDataframe[IG_timeIndexMatchGlobal, ],
					 mapping=aes(x=time, y=total_acc),
					 colour="black", size=3)
dropAccPlot <- dropAccPlot + 	
					geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
					 mapping=aes(x=time, y=total_acc),
					 shape=17,
					 colour="black", size=3)						
dropAccPlot <- dropAccPlot + 	
	ggtitle("Droplet Accerlations") +
	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=12),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("Time (s)") ) +
	ylab(expression("Droplet Acceleration (mm"^2*"/s)" ) ) 
size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(dropAccPlot, file="AccelerationDroplet.pdf", width=size.w, height=size.h, units=un)


accMagnitudeNormalized <- ggplot(sub_globalDataframe)
accMagnitudeNormalized <- accMagnitudeNormalized + 
					geom_point(mapping=aes(x=time/time_ext_global, 
						y=total_acc/acc_ext_global, colour=expname)) 
# accMagnitudeNormalized <- accMagnitudeNormalized + 	
# 					geom_point(data=globalDataframe[EXT_timeIndexMatchGlobal, ],
# 					 mapping=aes(x=time/time_ext_global[EXT_timeIndexMatchGlobal], 
# 					 	y=vel_mag/velocity_ext_global[EXT_timeIndexMatchGlobal]),
# 					 shape=17,
# 					 colour="black", size=3)					
accMagnitudeNormalized <- accMagnitudeNormalized + 	
	ggtitle("Normalized Velocities After Extinction") +
	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=12),
	legend.position=c(0.9, 0.75),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	xlab(expression("Time/ Time at Extinction") ) +
	ylab(expression("Acceleration Magnitude/ Acceleration at Extinction") ) 
size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(accMagnitudeNormalized, file="accelerationMagnitudeNormalized.pdf", width=size.w, height=size.h, units=un)




kRate <- ggplot(sub_globalDataframe )
kRate <- kRate +
		 geom_point(mapping=aes(x=time/time_ext_global, 
				y=k_bw1, colour=expname)) +
		ggtitle("Burning Rates After Extinction") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.9, 0.75),
		legend.title = element_blank(),
		legend.text = element_text(size=6), 
		axis.title.x = element_text(size=12),
		axis.title.y = element_text(size=12),
		legend.background = element_rect(fill="white"),
		legend.key.height = unit(5,"mm"),
		panel.background = element_rect(fill = "gray90"),
		axis.text = element_text(size=12,colour="black") ) +
		xlab(expression("Time/ Time at Extinction") ) +
		ylab(expression("K (mm"^2*"/s)") ) 


ReynoldsNo <- ggplot(globalDataframe )
ReynoldsNo <- ReynoldsNo +
		 geom_point(mapping=aes(x=time, y=reynolds, colour=expname)) +
		ggtitle("Reynolds Numbers") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.9, 0.75),
		legend.title = element_blank(),
		legend.text = element_text(size=6), 
		axis.title.x = element_text(size=12),
		axis.title.y = element_text(size=12),
		legend.background = element_rect(fill="white"),
		legend.key.height = unit(5,"mm"),
		panel.background = element_rect(fill = "gray90"),
		axis.text = element_text(size=12,colour="black") ) +
		xlab(expression("Time") ) +
		ylab(expression("Re") ) 
ggsave(ReynoldsNo, file="ReynoldsNumbers.pdf", width=size.w, height=size.h, units=un)

# N <- nrow(subset(globalDataframe, timeIndicator=="after_extinction" ))
# ReynoldsNo <- ReynoldsNo + geom_point(mapping=aes(y=vel_mag, colour=expname))

# df.preignition <- subset(globalDataframe,timeIndicator=="pre_ignition") 
xyPreIgnition_VelMagGroup <- ggplot( subset(globalDataframe, timeIndicator=="pre_ignition") )
xyPreIgnition_VelMagGroup <- xyPreIgnition_VelMagGroup +
		 geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=vel_mag)) +
		ggtitle("XY Droplet Paths: Grouped by Velocity Magnitude") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.9, 0.75),
		legend.title = element_blank(),
		legend.text = element_text(size=6), 
		axis.title.x = element_text(size=12),
		axis.title.y = element_text(size=12),
		legend.background = element_rect(fill="white"),
		legend.key.height = unit(5,"mm"),
		panel.background = element_rect(fill = "gray90"),
		axis.text = element_text(size=12,colour="black") ) +
		xlab(expression("x (mm)") ) +
		ylab(expression("y (mm)") ) 
ggsave(xyPreIgnition_VelMagGroup, file="xyPreIgnition_VelMagGroup.pdf", width=size.w, height=size.h, units=un)




xyPreIgnition_doGroup <- ggplot( subset(globalDataframe, timeIndicator=="pre_ignition") )
xyPreIgnition_doGroup <- xyPreIgnition_doGroup +
		 geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=do)) +
		ggtitle("XY Droplet Paths: Grouped by do") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.9, 0.75),
		legend.title = element_blank(),
		legend.text = element_text(size=6), 
		axis.title.x = element_text(size=12),
		axis.title.y = element_text(size=12),
		legend.background = element_rect(fill="white"),
		legend.key.height = unit(5,"mm"),
		panel.background = element_rect(fill = "gray90"),
		axis.text = element_text(size=12,colour="black") ) +
		xlab(expression("x (mm)") ) +
		ylab(expression("y (mm)") ) 
ggsave(xyPreIgnition_doGroup, file="xyPreIgnition_doGroup.pdf", width=size.w, height=size.h, units=un)


graphics.off()
#end for loop 
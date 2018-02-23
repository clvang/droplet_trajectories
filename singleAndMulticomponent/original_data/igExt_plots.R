
rm(list=ls(all=TRUE))		#clear workspace of all variables

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
# library(cowplot)
library(ggpubr)

currentDirectory <- getwd()
setwd(currentDirectory)
graphics.off()

########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

#read in key containing ignition and extinction times of experiments
igExt_times <- read.csv(file = "/Users/changvang/mygitFiles/flex_experiment_information/ignitionExtinctionTimes.csv",
			head=TRUE, sep=",",
			stringsAsFactors=FALSE)
# igExt_times <- read.csv(file = "/Users/changvang/mygitFiles/flex_experiment_information/heptane_coolFlames.csv",
# 			head=TRUE, sep=",",
# 			stringsAsFactors=FALSE)

IG_timeIndexMatchGlobal <- numeric(nrow(igExt_times))
EXT_timeIndexMatchGlobal <- numeric(nrow(igExt_times)) 
pointer <- numeric(nrow(igExt_times)-1)  #pointer to start of each data set in global data frame
for (i in 1:nrow(igExt_times) ){
	expName <- igExt_times$FLEX_ID[i]
	currentExpData_index <- pmatch(expName,csvfilenames)
	currentExpData <- read.csv(file=csvfilenames[currentExpData_index])

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
						currentExpData$time,
						currentExpData$tot_vel_fit)

	if (i == 1){
		# create global data frame
		globalDataframe <- tempDataframe
	}else{
		pointer[i-1] <- nrow(globalDataframe) + 1		# pointer indicating the start
														# of each dataframe in globalDataframe
		globalDataframe <- rbind(globalDataframe, tempDataframe)		
	}

	dataFrameSize <- nrow(globalDataframe)
} # end for i

# set names in global data frame
globalDataframe <- setNames(globalDataframe, c("expname","time","vel_mag") )

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

globalDataframe <- cbind(globalDataframe, timeIndicator)

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


#end for loop 
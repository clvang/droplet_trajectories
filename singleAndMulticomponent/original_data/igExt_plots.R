
rm(list=ls(all=TRUE))		#clear workspace of all variables

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
# library(cowplot)
library(ggpubr)
library(abind)

source("/Users/changvang/mygitFiles/droplet_trajectories/singleAndMulticomponent/original_data/dataFrameConstruct.R")

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
# specify experiments with heptane droplets with cool falmes
heptane_coolFlames <- c("X126H02",
						"X129H01",
						"X140H02",
						"X140H05",
						"X144H01",
						"X144H02",
						"X170H01",
						"X170H03",
						"X177H01",
						"X177H02",
						"X177H05",
						"X177H07")
# specify experiments with methanol
methXenon_all <- c("X168M07",
					"X168M03",
					"X168M02",
					"X168M01",
					"X163M07",
					"X163M04",
					"X163M03",
					"X119M04",
					"X121M02",
					"X129M05",
					"X149M02",
					"X161M02",
					"X161M04",
					"X163M01",
					"X175M06",
					"X144M05",
					"X163M02",
					"X161M05",
					"X121M03", 
					"X144M01",
					"X144M06",
					"X175M04",  
					"X161M03",
					"X168M05",
					"X168M06",
					"X175M01",
					"X175M02",
					"X175M03",
					"X175M05")
# specify propanol/glycerol droplets
propanolGlycerol <- c("C101D05",
					"C101D02",
					"C077D06",
					"C085D03",
					"C104D06",
					"C080D05",
					"C104D05",
					"C077D02",
					"C080D01",
					"C101D06",
					"C077D04",
					"C108D01",
					"C108D02",
					"C108D03")
# specify experiments with methanol droplets who have categroy 2 trajectories
methXenon_cat2 <- c("X175M06",
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
					"X175M01",
					"X175M02",
					"X175M03",
					"X175M05")

dataTable <- dataFrameConstruct(csvfilenames=csvfilenames, 
				   				key=key, 
				   				expInterestNames=propanolGlycerol)

globalDataframe <- dataTable$globalDataframe
IG_timeIndexMatchGlobal <- dataTable$IG_timeIndexMatchGlobal
EXT_timeIndexMatchGlobal <- dataTable$EXT_timeIndexMatchGlobal

# overall velocity magnitude plot
velMagnitudePlot <- ggplot( globalDataframe )
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

bp <- ggplot(aes(x=timeIndicator,y=vel_mag), data=globalDataframe)
bp <- bp + theme_bw() + 
	  	stat_boxplot(geom='errorbar',coef=20) +
	  	geom_boxplot(aes(fill=timeIndicator), 
	  	show.legend=FALSE, notch=FALSE,
	  	outlier.shape=NA ) +    #avoids plotting outliers twice 
		geom_jitter(position=position_jitter(width=.05, height=0))
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


# overall normalized velocity magnitude plot vs normalized time
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
	ggtitle("Normalized Acceleration After Extinction") +
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
size.h <- 6		    #specifies height of .pdf of plot in units specified by un
un <- "in"		    #specifies unit of size.w and size.h
ggsave(accMagnitudeNormalized, file="AccelerationMagnitudeNormalized.pdf", width=size.w, height=size.h, units=un)




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
		ggtitle("XY Methanol Droplet Paths: Grouped by Velocity Magnitude") +
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
		ggtitle("XY Methanol Droplet Paths: Grouped by do") +
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


heptaneXenon_cat1 <- c("X177H06",
					   # "X177H12",
					   "X177H07",
					   "X100H07",
					   "X177H04",
					   "X126H07",
					   "X129H02",
					   # "X140H05",
					   "X100H01",
					   "X126H01",
					   "X126H02",
					   "X126H06",
					   "X161H01",
					   "X170H03")
heptane_cat1 <- c(heptaneXenon_cat1)


dataTable_Heptane <- dataFrameConstruct(csvfilenames=csvfilenames,
										key = key,
										expInterestNames = heptane_cat1 )

globalDataframe <- dataTable_Heptane$globalDataframe

xyPreIgnitionHeptane_doGroup <- ggplot( subset(globalDataframe, timeIndicator=="pre_ignition") )
xyPreIgnitionHeptane_doGroup <- xyPreIgnitionHeptane_doGroup +
		 geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=do)) +
		ggtitle("XY Heptane Droplet Paths: Grouped by do") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.85, 0.8),
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
ggsave(xyPreIgnitionHeptane_doGroup, file="xyPreIgnitionHeptane_doGroup.pdf", width=size.w, height=size.h, units=un)

xyPreIgnitionHeptane_velGroup <- ggplot( data=subset(globalDataframe, timeIndicator=="pre_ignition") )
xyPreIgnitionHeptane_velGroup <- xyPreIgnitionHeptane_velGroup +
	    geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=vel_mag)) +
		ggtitle("XY Heptane Droplet Paths: Grouped by Velocity Magnitude") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.85, 0.8),
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
ggsave(xyPreIgnitionHeptane_velGroup, file="xyPreIgnitionHeptane_velGroup.pdf", width=size.w, height=size.h, units=un)

heptaneNonXenon_cat1 <- c("H05H103",
						  "H19H201",
						  "H03H201",
						  "H03H202")

heptane_cat2 <- c("H04H201",
				  "H11H101",
				  "H19H101",
				  "H10H101")

heptane_cat3 <- c("H04H101",
				  "H03H101",
				  "H12H101")


propGly_cat4 <- c("C080D05",
				  "C085D03",
				  "C104D05",
				  "C077D02",
				  "C080D01",
				  "C101D05",
				  "C104D06",
				  "C077D06",
				  "C101D06",
				  "C077D04")
dataTable_propGly_cat4 <- dataFrameConstruct(csvfilenames=csvfilenames,
										key = key,
										expInterestNames = propGly_cat4 )

globalDataframePropgly <- dataTable_propGly_cat4$globalDataframe
xyPreIgnitionPropGly_doGroup <- ggplot( subset(globalDataframePropgly, timeIndicator=="pre_ignition") )
xyPreIgnitionPropGly_doGroup <- xyPreIgnitionPropGly_doGroup +
		 geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=do)) +
		ggtitle("XY Propnaol/Glycerol Droplet Paths: Grouped by do") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.85, 0.8),
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
ggsave(xyPreIgnitionPropGly_doGroup, file="xyPreIgnitionPropGly_doGroup.pdf", width=size.w, height=size.h, units=un)

xyPreIgnitionPropGly_velGroup <- ggplot( subset(globalDataframePropgly, timeIndicator=="pre_ignition") )
xyPreIgnitionPropGly_velGroup <- xyPreIgnitionPropGly_velGroup +
		 geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=vel_mag)) +
		ggtitle("XY Propanol/Glycerol Droplet Paths: Grouped by Velocity Magnitude") +
		theme_bw() +
		theme(plot.title = element_text(colour="black",face="bold",size=12),
		legend.position=c(0.85, 0.8),
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
ggsave(xyPreIgnitionPropGly_velGroup, file="xyPreIgnitionPropGly_velGroup.pdf", width=size.w, height=size.h, units=un)


propGly_cat2 <- c("C077D03",
	 			  "C101D02")

propGly_cat3 <- c("C108D03")




graphics.off()
#end for loop 
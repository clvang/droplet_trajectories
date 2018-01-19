# script to read _XYdispData.csv files and plot droplet x-y paths
# as predicted by the MAMS and SAMS data.
# place all _XYdispData.csv files in the same folder as this script!
# Use this script only to plot droplet paths from MAMS & SAMS data
# Another script "dropletPaths.R" should be use to plot droplet
# x-y paths from centroid locaiton data.  The reason is because
# SAMS data may not be available for all droplet experiments analyzed.
# Centroid location data however, is available for all droplet experiments
# analyzed.

# Since _XYdispData.csv files contain both the x-y droplet paths as measured
# from centroid movement (factors "drop_disp"), and as predicted 
# by SAMS/MAMS data (factors "sams+mams_disp"), this script could
# also be used to plot x-y droplet paths measured from centroid movement

rm(list=ls(all=TRUE))		#clear workspace of all variables

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots
library(grid)			#this is needed for the multiple plot function
try({
  source("/Users/changvang/mygitFiles/data_analysis/rFunctions/multiPlot.R")
})
try({
  source("/home/ucdkemper/mygitfiles/data_analysis/rFunctions/multiPlot.R")
})



currentDirectory <- getwd()
setwd(currentDirectory)
########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_XYdispData.csv")

#read experimental parameters for single component droplet experiments
keyfilename <- dir(pattern="single_key_v2")
key <- read.csv(file=keyfilename,head=TRUE,sep=",",
		stringsAsFactors=FALSE)


# read each csv file and save variables of interest
for (i in 1:length(csvfilenames)){
	
	#store experiment name
	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]
	temp <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
		stringsAsFactors=FALSE )	

	#grab do for current experiment
	keyRow <- which(key$expname == expname)
	do <- key$do[keyRow]
	if ( as.numeric(do) > 3){
		dsize <- "Large"
	}else{
		dsize <- "Small"
	}

	#grad pressure for current experiment
	pressure <- key$p[keyRow]
	

	#extract only SAMS/MAMS data
	dftemp_SamsMams <- subset(temp,variablename == "sams+mams_disp")

	#extract only centroid x-y data
	dftemp_centroid <- subset(temp,variablename == "drop_disp")	

	#create factors to distinguish centroid data
	#from PAD data
	expname1 <- rep(expname,nrow(dftemp_SamsMams))
	expname2 <- rep(expname,nrow(dftemp_centroid))

	#create factors to group based on do
	dofactors1 <- rep(dsize,nrow(dftemp_SamsMams))
	dofactors2 <- rep(dsize,nrow(dftemp_centroid))

	#create factor to group based on pressure
	pfactors1 <- rep(pressure,nrow(dftemp_SamsMams))
	pfactors2 <- rep(pressure, nrow(dftemp_centroid))
	
	dftemp_SamsMams <- cbind(expname1, dftemp_SamsMams, 
						dofactors1, pfactors1 )
	dftemp_centroid <- cbind(expname2, dftemp_centroid, 
						dofactors2, pfactors2 )

	if ( i == 1){
		dfglobal_SamsMams <- dftemp_SamsMams
		dfglobal_centroid <- dftemp_centroid
	}else{
		dfglobal_SamsMams <- rbind(dfglobal_SamsMams, dftemp_SamsMams)
		dfglobal_centroid <- rbind(dfglobal_centroid, dftemp_centroid)
	}
}
dfglobal_SamsMams[ ,2] <- NULL  # remove column of index from dataframe
dfglobal_centroid[ ,2] <- NULL



## ----- plot trajectories grouped by experiment ID -------####

# plot superimposed plot of mams/sams x-y displacement
p1 <- ggplot(dfglobal_SamsMams)
p1 <- p1 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=expname1)) 
p1 <- p1 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position="none", #c(0.85, 0.4),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p1 <- p1 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 21	    #specifies width of .pdf of plot in units specified by un
size.h <- 12		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p1, file="samsMams_trajectories.pdf", width=size.w, height=size.h, units=un)

# plot superimposed plot of droplet centroid x-y displacement
p2 <- ggplot(dfglobal_centroid)
p2 <- p2 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=expname2)) 
p2 <- p2 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position="none", #c(0.85, 0.70),
	legend.title = element_blank(),
	legend.text = element_text(size=6), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p2 <- p2 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 21	    #specifies width of .pdf of plot in units specified by un
size.h <- 12		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p2, file="droplet_trajectories.pdf", width=size.w, height=size.h, units=un)

dev.new()
pdf("trajectories_expID.pdf", width=11, height=9)		#save plot as pdf
finalplot <- multiplot(p2, p1, cols=1)
dev.off()



## ----- plot trajectories grouped by initial diameters -------####

# plot superimposed plot of droplet centroid x-y displacement
p3 <- ggplot(dfglobal_centroid)
p3 <- p3 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=dofactors2)) 
p3 <- p3 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.85, 0.65),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p3 <- p3 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p3, file="droplet_trajectories_do.pdf", width=size.w, height=size.h, units=un)


# plot superimposed plot of mams/sams x-y displacement
p4 <- ggplot(dfglobal_SamsMams)
p4 <- p4 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=dofactors1)) 
p4 <- p4 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.3),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p4 <- p4 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p4, file="samsMams_trajectories_do.pdf", width=size.w, height=size.h, units=un)

dev.new()
pdf("trajectories_do.pdf", width=11, height=9)		#save plot as pdf
finalplot <- multiplot(p3, p4, cols=1)
dev.off()







## ----- plot trajectories grouped by chamber pressures -------####

# plot superimposed plot of droplet centroid x-y displacement
p5 <- ggplot(dfglobal_centroid)
p5 <- p5 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=pfactors2)) 
p5 <- p5 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.85, 0.65),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=2)) +
	guides(linetype=guide_legend(ncol=2))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p5 <- p5 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p5, file="droplet_trajectories_p.pdf", width=size.w, height=size.h, units=un)


# plot superimposed plot of mams/sams x-y displacement
p6 <- ggplot(dfglobal_SamsMams)
p6 <- p6 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=pfactors1)) 
p6 <- p6 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.3),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=2)) +
	guides(linetype=guide_legend(ncol=2))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p6 <- p6 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
# ggsave(p6, file="samsMams_trajectories_p.pdf", width=size.w, height=size.h, units=un)

dev.new()
pdf("trajectories_pressures.pdf", width=11, height=9)		#save plot as pdf
finalplot <- multiplot(p5, p6, cols=1)
dev.off()



# list of methanol-xenon experiments with similar trajectories (category 1) to
# that of X182M01 (an evaporating droplet)
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
					"X175M04")
p7 <- ggplot(subset(dfglobal_SamsMams, expname1==methXenon_cat2) )
p7 <- p7 + geom_point(mapping=aes(x=x_disp, y=y_disp, colour=dofactors1)) 
p7 <- p7 + 	theme_bw() +
	theme(plot.title = element_text(colour="black",face="bold",size=6),
	legend.position=c(0.8, 0.3),
	legend.title = element_blank(),
	legend.text = element_text(size=12), 
	axis.title.x = element_text(size=12),
	axis.title.y = element_text(size=12),
	legend.background = element_rect(fill="white"),
	legend.key.height = unit(5,"mm"),
	panel.background = element_rect(fill = "gray90"),
	axis.text = element_text(size=12,colour="black") ) +
	guides(colour=guide_legend(ncol=2)) +
	guides(linetype=guide_legend(ncol=2))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 
p7 <- p7 + annotate("pointrange",x=0,y=0, 
			ymin = -0.002, ymax = 0.002,
			colour="black", size = 1.5)

size.w <- 10	    #specifies width of .pdf of plot in units specified by un
size.h <- 6		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p7, file="methanolXenon_cat2_SAMSMAMS.pdf", width=size.w, height=size.h, units=un)

# dev.new()
# pdf("trajectories_pressures.pdf", width=11, height=9)		#save plot as pdf
# finalplot <- multiplot(p5, p6, cols=1)
dev.off()








graphics.off()  #close all graphics windows

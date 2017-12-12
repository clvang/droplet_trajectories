# script to read _d2KROUT.csv files and plot droplet x-y paths
# from droplet centroid location data ONLY.
# place all _d2KROUT.csv files in the same folder as this script!

library(ggplot2)		#this is needed to generate plots
library(RColorBrewer)	#this is needed for the color package used in generating plots

rm(list=ls(all=TRUE))		#clear workspace of all variables
currentDirectory <- getwd()
setwd(currentDirectory)

########## READ RELEVANT FILES/DATA AND ASK FOR USER INPUT ##########
#read in names of all .csv files in current directory
csvfilenames <- dir(pattern = "*_D2KROUT.csv")

# read each csv file and save variables of interest
for (i in 1:length(csvfilenames)){
	
	#store experiment name
	expname <- unlist( strsplit(csvfilenames[i],"_") )[1]
	temp <- read.csv(file=csvfilenames[i],head=TRUE,sep=",",
		stringsAsFactors=FALSE)	

	#extract only variables of interest
	df.temp <- data.frame(temp$time, temp$do, temp$x_loc_fit,
						temp$x_vel_fit, temp$y_loc_fit,
						temp$y_vel_fit)
	expname <- rep(expname,nrow(temp))
	df.temp <- cbind(expname, df.temp )

	if ( i == 1){
		df.global <- df.temp
	}else{
		df.global <- rbind(df.global, df.temp)
	}
}
df.global <- setNames(df.global, c("expname","time","do",
									"x_loc_fit","x_vel_fit",
									"y_loc_fit","y_vel_fit") )

# create vector grouping do sizes and add to df.global
doSize <- as.character(nrow(df.global))
for (i in 1:nrow(df.global)){
	if ( df.global$do[i] > 3.0 ){
		doSize[i] <- "Large"
	}else{
		doSize[i] <- "Small"
	}
}
df.global <- cbind(df.global, doSize)


p1 <- ggplot(df.global)
p1 <- p1 + geom_point(mapping=aes(x=x_loc_fit, y=y_loc_fit, colour=expname)) 
p1 <- p1 + 	theme_bw() +
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
	guides(colour=guide_legend(ncol=5)) +
	guides(linetype=guide_legend(ncol=5))	+
	xlab(expression("X (mm)") ) +
	ylab(expression("Y (mm)") ) 	

size.w <- 21	    #specifies width of .pdf of plot in units specified by un
size.h <- 12		#specifies height of .pdf of plot in units specified by un
un <- "in"		#specifies unit of size.w and size.h
ggsave(p1, file="all_droplet_trajectories.pdf", width=size.w, height=size.h, units=un)






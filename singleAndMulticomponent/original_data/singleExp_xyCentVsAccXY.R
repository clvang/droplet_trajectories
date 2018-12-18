rm(list=ls())
library(locpol)
library(ggplot2)
library(reshape2)


# dflame_data <- read.csv(file="N01H101_dflame_UV.csv")
droplet_datafile <- "X149M02.csv"
acc_datafile <- "X149M02_samsMamsVibeData.csv"

expname <- unlist( strsplit(droplet_datafile,"\\.") )[1]
centroid_data <- read.csv(file=droplet_datafile)
acc_data <- read.csv(file=acc_datafile)


bw <- 0.45
deg <- 2

time <- centroid_data$t 
# d_flame <- flame_diametermm
x_centroid <- centroid_data$x_loc
y_centroid <- centroid_data$y_loc
data <- data.frame(time, 
					x_centroid, 
					y_centroid )

xcent_fit <- locpol(x_centroid ~ time, data=data, deg=deg, xeval=time, kernel=gaussK, bw=bw)
ycent_fit <- locpol(y_centroid ~ time, data=data, deg=deg, xeval=time, kernel=gaussK, bw=bw)

xcent_locpol <- xcent_fit$lpFit$x_centroid
x_vel <- xcent_fit$lpFit$x_centroid1
x_acc <- xcent_fit$lpFit$x_centroid2

ycent_locpol <- ycent_fit$lpFit$y_centroid
y_vel <- ycent_fit$lpFit$y_centroid1
y_acc <- ycent_fit$lpFit$y_centroid2

size.w <- 3.25	    #specifies width of .pdf of plot in units specified by un
size.h <- 2.8		#specifies height of .pdf of plot in units specified by un
ptsize <- 0.5
txtsize <- 8
titlsize <- 7
minSize <- 0.5
maxSize <- 2.0


# plot radiometer plot with droplet centroid accelerations
acc_time <- acc_data$time
x_acc <- acc_data$x_acc
y_acc <- acc_data$y_acc

deg <- 2
bw <- 0.3

# offset_xacc <- 0.0 #1.0
# offset_yacc <- 0.0 #2.0
# x_acc <- x_acc - offset_xacc
# y_acc <- y_acc - offset_yacc

scaleFactor <- 1.5    #increase to scale down radiometer curve
plot_title <- paste0(expname,"_Droplet_Path")
p2 <- ggplot() + 
			geom_line(mapping=aes(x=time,y=value, linetype=variable, 
			colour = variable, group=variable), 
			data= melt( data.frame(time,xcent_locpol, ycent_locpol), id='time' ), size=ptsize)  +
			geom_line(mapping=aes(x=acc_time,y=value / scaleFactor, 
				linetype=variable, colour = variable, group=variable), 
				data = melt( data.frame(acc_time, x_acc, y_acc), id='acc_time'), size=ptsize ) +		
	      ggtitle(plot_title) +
			# scale_shape_manual( values=c(25,22,17,18,15,16,20) ) +
			scale_colour_manual(
				values=c(
					"#377eb8",  #blue (set1)					
					"#e41a1c",  #red (set1)
					"#4daf4a",  #green (set1)
					"black",
					"#377eb8",  #blue (set1)
					"#377eb8",  #blue (set1)
					"#66c2a5")) 	+
			scale_linetype_manual( values=c("twodash","longdash","solid","dotdash")) +
			scale_y_continuous(name = "Drop Position (mm)",
				sec.axis = sec_axis(~.*scaleFactor , name = expression( paste("Acceleration (", mu, "-g)") ) ) ) +			
			xlab(expression("Time (s)") ) + 
			theme_bw() +
			theme(plot.title = element_text(colour="black",face="bold",size=titlsize),
			legend.position=c(0.85, 0.89),	
			legend.title = element_blank(),
			legend.text = element_text(size=4), 
			axis.title.x = element_text(size=txtsize),
			axis.title.y = element_text(size=txtsize),
			legend.background = element_rect(fill="white"),
			legend.key.height = unit(0.5,"mm"),
			legend.key.width = unit(3.0,"mm"),
			panel.background = element_rect(fill = "gray90"),
			axis.text = element_text(size=txtsize,colour="black") ) 
un <- "in"		#specifies unit of size.w and size.h
ggsave(p2, file=paste0(expname,"_CentroidAccelerationPlot.pdf"), width=size.w, height=size.h, units=un)

# graphics.off()

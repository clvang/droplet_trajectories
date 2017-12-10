# droplet_trajectories
Contains codes used to analyze droplet trajectories.

* "samsMamsDropletPaths.R" is the script to read _XYdispData.csv files and plot droplet x-y paths as predicted by the MAMS and SAMS data. Place all _XYdispData.csv files in the same folder as this script! Use this script only to plot droplet paths from MAMS & SAMS data. 

* Another script "dropletPaths.R" should be use to plot droplet x-y paths from centroid locaiton data.  The reason is because SAMS data may not be available for all droplet experiments analyzed. Centroid location data however, is available for all droplet experiments analyzed.

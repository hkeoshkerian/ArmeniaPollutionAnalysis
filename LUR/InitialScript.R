#Workflow: 
#Define observations of air pollution by point and time using limiting criteria.  
#Divide up 100m grid cells by population areas, limit to Yerevan

#For each 100m grid cell: 
#Compute length of roads in 100, 200, 500m radii
#Measure distance to major road
#Compute Land cover categories in 100, 200, 500m radii
#Compute volume (area*height) of vegetation in 100, 200, 500m radii
#Retrieve elevation
#Retrieve population


#For each sensor location: Retrieve the value from the 100m square above
#For each hourly observation: retrieve weather data 
#Use the above data as inputs to a regression
#Use the regression coefficients to predict air pollution levels for each 100m grid cell/hour in Yerevan
#Average these over the entire year for each grid cell
#Using populations and age structure in each grid cell, calculate exposure and mortality in each grid cell (GEMM script)
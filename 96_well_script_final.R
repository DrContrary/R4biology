if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}

if(!require(Cairo)){
  install.packages("Cairo")
  library(Cairo)
}

if(!require(viridis)){
  install.packages("viridis")
  library(viridis)
}

if(!require(platetools)){
  install.packages("platetools")
  library(platetools)
}


################################
### Manually input Variables ###
################################

#add some variables here
std_units = "For Example: cell Number, ug, mg"
assay_id = "Name of Assay for Reference"
abs_nm = "510"  # what wavelength did you read at?
file = "96_well_example.xls" # what file in your directory are you analyzing?
dir.create("96-well_output") # save output files

######################################
### Read in Data from Excel Output ###
######################################
plate_cols = c("alpha", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
# the first sheet contains the measurements
readings = read_excel(file, sheet = 1) 
colnames(readings) = plate_cols
# melt the data to take it from wide to long
readings = melt(data = readings, id.vars = "alpha", variable.name = "col_num" )
readings = unite(readings, "well", c("alpha", "col_num"), sep = "")
# the second sheet contains the plate layout
template = read_excel(file, sheet = 2)
colnames(template) = plate_cols
# melt the data to take it from wide to long
template = melt(data = template, id.vars = "alpha", variable.name = "col_num", na.rm = TRUE) # na.rm is very important for downstream steps
template = unite(template, "well", c("alpha", "col_num"), sep = "")
# put the key and readings together
# inner join is a filtering join -only matches in well present in both sets are kept
plate = inner_join(template, readings, by = "well")
names(plate) <- c("well", "sample", "abs")

#####################################
### Standard Curve Concentrations ###
#####################################

# a newly added 3rd sheet has the standards
standards = read_excel(file, sheet = 3)

#############################
###   calculate Mean Abs  ###
#############################

mean_stats = plate %>%
  group_by(sample) %>%
  summarise(mean_abs = mean(abs), sd_abs = sd(abs))

curve = mean_stats %>% 
  dplyr::filter(str_detect(sample, "^Standard")) # include all the standard data
curve = left_join(curve, standards, by = "sample")

##################################
### Get Linear Regression Data ###
##################################

fit = lm(formula = std_con ~ mean_abs, data = curve)
summary = summary(fit)   

m = coefficients(fit)[["mean_abs"]]
b = coefficients(fit)[["(Intercept)"]]
r_sqr = summary[["r.squared"]]
equation = paste0("y=", round(m, 2), "x", "+", round(b, 2))

###################################
### Find Unknown Concentrations ###
###################################

samples = mean_stats %>% 
  filter(!str_detect(sample, "^Standard")) %>%   # remove all the standard data
  dplyr::mutate(sample_con = (mean_abs * m + b ))

######################
### Plot your Data ###
######################

p = ggplot(curve, aes(x = mean_abs, y= std_con)) +
  geom_point() +
  geom_smooth(method  = lm)+
  geom_point(data = samples, aes(x = mean_abs , y = sample_con, color = sample)) +
  geom_text(data = samples,  mapping = aes(x = mean_abs , y = sample_con, label = round(sample_con, 2), color = sample, vjust=-2, hjust=0.5 )) +
  theme_bw() +
  labs(title = "96-well plate data",
       subtitle = paste0(assay_id, " ", Sys.Date()),
    tag = "Generated in R",
    y = paste("Concentration", std_units ,sep = " "),
    x = paste0("Mean Absorbance ", "(", abs_nm, ")" ), 
    caption = paste0("y=", round(m, 2), "x", "+", round(b, 2), "    ", "R Squared: ", round(r_sqr, 4)) # print your equation and round the values to 2 digits
  )


###################################
### Platetools Package Examples ###
###################################

# Use plate map to generate extra columns
readings_map = plate_map(data = readings$value,
                 well = readings$well)
readings_map = dplyr::right_join(plate, readings_map, by = "well") 

# Genreate a ggplot variable for plotting
plate_plot = raw_map(data = readings_map$abs,
                     well = readings_map$well,
                     plate = 96)

################################
### Save outputs: Plate Maps ###
################################

setwd("96-well_output")
# Use cairo to save high res images
cairo_pdf(file= paste0(Sys.Date(),"_", assay_id, "_", "Heatplate_mapped", ".pdf"), 
          width=8.46, 
          height=4.7,
          family = "Arial",
          fallback_resolution = 300) 
plate_plot +
  scale_fill_viridis() +
  theme_bw() +
  geom_text(mapping = aes(label = readings_map$sample), vjust=-4, size=1.5) +
  labs(title = "96-well plate data (Mapped Samples)",
       subtitle = paste0(assay_id, " ", Sys.Date()),
       caption = paste0("Reading Wavelength: ", abs_nm, "nm")
  )
dev.off()

#####################################
### Save outputs: Graph and Table ###
#####################################

# make a new folder to output results

# Use cairo to save high res images
cairo_pdf(file= paste0(Sys.Date(),"_", assay_id, ".pdf"), 
          width=6, 
          height=5,
          family = "Arial",
          fallback_resolution = 300) 
p
dev.off()

# output the data table
write.xlsx(samples, file = paste0(Sys.Date(),"_", assay_id, "_", "calculated_96_well_assay_output", ".xlsx"), sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE)
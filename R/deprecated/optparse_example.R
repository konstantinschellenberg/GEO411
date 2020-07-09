# Optparse example for R from command line


# Preprocess -------------------------------------------------------------------

require("optparse", attach.required = T)

# if needed, run the coherence preprocessing steps in preprocess_coherence.R
file = "D:/Geodaten/Master/projects/GEO411/R/preprocess_coherence.R"
system(command = sprintf("Rscript %s --help", file)) # run help

cmd = sprintf("Rscript %s --directory=%s --lidar=%s --targetresolution=%s", file, directory, lidar, targetresolution)
print(cmd)

# run pre-processing
# system(cmd)
system("gdalinfo")

# Plotting ---------------------------------------------------------------------

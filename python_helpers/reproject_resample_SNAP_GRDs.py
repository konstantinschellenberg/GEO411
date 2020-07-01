# Script to convert the output of SNAP to EPSG: 32632 and resample it to 20x20

import os
import sys

new_dir = "GRDs_Multilooked_32632_50x50_median_filter"
# check if already exists
if os.path.exists(new_dir):
	pass
else:
	os.makedirs(new_dir)


files = [x for x in os.listdir(".") if x.endswith("Cal_TC.tif")]

# process each
for i in files:
	name = i[0:-4]
	new_name = os.path.join(new_dir, name + "_32632_50x50_medFilter.tif")
	cmd = "gdalwarp -t_srs EPSG:32632 -r med -tr 50 50 {} {}".format(i, new_name)
	os.system(cmd)

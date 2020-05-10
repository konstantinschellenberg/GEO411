import os
import re
import shutil

main_dirs = [x for x in os.listdir(".") if x.startswith("0000")]

cwd = os.getcwd()

for scene in main_dirs:
    path = os.path.join(scene, "summary.txt")
    with open(path) as summary:
        for line in summary:
            if re.search("Img_SceneCenterDateTime", line): 
               date = line.split("=")[1][1:9]
               new = os.path.join(cwd, date)
               path_scene = os.path.join(cwd, scene)
               print(path_scene)
               os.makedirs(new, exist_ok=True)
               shutil(path_scene, new)

               


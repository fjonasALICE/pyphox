Code used for easy steering of JetPhox with python based on https://github.com/hpoppenb/BashPhox by Hendrik Poppenborg

I am not the author of JetPhox! Original sourcecode can be found here: https://lapth.cnrs.fr/PHOX_FAMILY/jetphox.html
Please cite the authors, e.g. Phys.Rev. D73 (2006) 094007

The code allows to perform calculations of the (isolated) prompt photon cross section, as well as gamma-jet correlations.

# Usage
## Modifications of original source code
While the source code of jetphox is taken from https://lapth.cnrs.fr/PHOX_FAMILY/jetphox.html version 1.4, several modifications have been made, many of which are taken from https://github.com/hpoppenb/BashPhox
- code allows to specify different PDF for both beams, allowing to calculate e.g. p-Pb and Pb-p collisions
- the hadlib.f file has been modified to improve stability for very low-x at very forward rapidities. The file was given to me by Marco van Leeuwen in private communications

## General/Prerequisites/Docker
As I always struggled to compile JetPhox reliably on multiple systems, I created a docker container that contains all dependencies to compile jetphox with ROOT and LHAPDF support. The docker image can be found here: https://hub.docker.com/repository/docker/fjonas/jetphoxenv/general . The docker image contains an installation of root, lhapdf and a small selection of PDFs. The MAKEFILE found in this repository under /jptemplate/MAKEFILE is already adjusted to work with the docker container, assuming the code of this repo is mounted under /home/jetphox inside the docker container

To get the container, use:
```
docker pull fjonas/jetphoxenv:latest
```

You can run an interactive sesseion inside the container, mounting your directory of this repo under /home/pyphox
```
docker run -it --rm -v /alf/data/flo/PyPhox/:/home/PyPhox fjonas/jetphoxenv:latest
```
A small selection of PDF sets can be found under /home/pdfsets

If you want to build the docker image yourself or modify it to fit your needs, the corresponding dockerfile can be found in ./dockertestapp/Dockerfile
# Running Jetphox
To make steering of jetphox easier, I created the python script run_jetphox , which allows building and steering of the JetPhox code in multiple photon pt bins to improve statistics. The program is split into two parts, which handle compilation of the source code (which has to be done for each pt bin and each config file) and the running/submission of the created executable.

Example to build JetPhox code for a Pb-Pb collisions with isolation 2 GeV in R=0.4 for direct photons:
```
./run_jetphox build --prefix Test_2023_01_16 --process dir --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy 2 --numberofevents 5000000 --randomseed 12345 --directcalculation 2 --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false
```
Example to build JetPhox code for a Pb-Pb collisions with isolation 2 GeV in R=0.4 for fragmentation photons:
```
./run_jetphox build --prefix Test_2023_01_16 --process onef --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy 2 --numberofevents 5000000 --randomseed 12345 --directcalculation 2 --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false
```
The script will create separate folder for each pt bin, labelled jp0 to jp17 by default. The used bins can be modified in the sourcode of run_jetphox. Each folder contains a compiled executable file, which then can be run to perform the calculation.

After compilation, the calculation is started using e.g. 
```
./run_jetphox build --prefix Test_2023_01_16 --process dir --pdf1 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember1 0 --pdf2 nNNPDF30_nlo_as_0118_A208_Z82 --pdfmember2 0 --scaleis 1.0 --scalefs 1.0 --scalerenorm 1.0 --higherorder true --cme 5020 --maxrap 0.8 --minrap -0.8 --isoconeradius 0.4 --isoenergy 2 --numberofevents 5000000 --randomseed 12345 --directcalculation 2 --jetrapmin -5.8 --jetrapmax 5.8 --skipcopy false
```
# Running jetphox on computing clusters
## Clusters that use singularity
Since one commonly runs these calculations on computing clusters rather than locally, run_jetphox also contains options submit jobs to a cluster via the 
```
run_jetphox submit
```
An example for job submission on a cluster using singularity is given in
```
sbatch submit_jetphox.sh
```
In this file, the build job is automatically started inside a singularity using the docker container that contains all the build requirements. After completion of the build job, the program will run 
```
./run_jetphox submit RUNOPTIONS
```
This is done OUTSIDE the docker container, since this part only handles the submission of individual jobs. In the backend, the program calls a copy of jptemplate/submitJobsSlurm.sh, which handles automatically the loading of the docker image and running of the job.
## Running on Perlmutter
The macro contains a dedicated option to run on PERLMUTTER, which uses shifter to handle docker images. A simple example script for launching jobs on perlmutter is provided, which has to be launched via:
```
bash build_and_submit.sh
```
The script automatically calls the assosiated bash scripts for compilation and submission, which are build_jetphox_perlmutter.sh and submit_jetphox_perlmutter.sh

# Post processing
Each job will automatically create a merging script in the main pyphox directory, which allows to merge the output for all pt bins. For example, for the direct contribution given above:
```
merge_Test_2024_01_16_nNNPDF30_0_nNNPDF30_0_scl10_10_10_dir_5020_yminneg08_ymax08_R04_E2.sh
```
After merging the direct contribution (dir) and fragmentation contribution (onef), the histograms from both files have to be added manually to obtain the full isolated prompt photon cross section!

# Modification of the macro
- Isolation, PDF sets, centre-of-mass energy etc can be modified freely using the arguments given to run_jetphox
- The histograms stored as output can be modified in ./configfiles/param_histo.indat_template
- if you want to use an additional PDF from LHAPDF, assuming you store them in /my/path/pdfsets, make sure to do:
  ```
  export LHAPDF_DATA_PATH="/my/path/pdfsets:${LHAPDF_DATA_PATH}"
  ```




CFS
===
CFS is an R package for spatial single cell genomics.
Documentation can be found at:
[Tutorial and instructions]()

The documentation is still under construction, and will soon be released. The tool can already be installed and is still un beta but is being actively worked on for full release. Thank you.

# CFS
# Table of Contents
- [Installation](#Installation)
- [CFS's interface](#CFS’s-interface)
- [Exemple](#Exemple)
- [Citation](#Citation)

## Installation
Installing the package from the github repository requires the package devtools which can be installed using the following commands :
```{r}
install.packages("devtools")
```
The command line to install CFS :
```{r}
devtools::install_github("c-thuil/CFS"
                         ,ref="main")
```
## CFS's interface
### Load data
The load data tab is plit into 4 loadable objects.
- **Load Visium output folder**
Select the folder which contains Space Ranger output. The data will then be loaded directly from it and can then be directly analysed in the Pre-Processing tab.
- **Load processed data**
Select an RDS file output from the tool or from an external visium analysis using Seurat. (`.rds`)
- **Load high res image**
Select High res image from Space Ranger output. The image will then be used for every spatial plot within the tool, offering higher quality visualisation.
- **Load full res image**
Select the original full res image from samples. Spatial plot will then be clickable and allow to visualise a zoomed image of the slice at à selected location.
### Pre-Processing
Allows to carry out the analysis from the output of Space ranger to the ICA analysis and enrichment. Once this is done, IC can be annoted with the tool.
### ICA
Show data related to ICs. IC, genes, spatial plot and enrichment can be observed. Each IC can be annoted in the table above. 
### ICA Table
Annotation table of the ICs, can be directly edited, downloaded or uploaded. (`.csv`)
### Visualisation
Visualisation of the UMAP reduction, density and Scatter Pie representation of cell types. 
### Marker table
Once clusterisation analysis has been carried out, this tab will display marker genes of each clusters.
### Output
Tab used to download the RDS output of the analysis, as well as subclusters of the object by cluster of cell types.
## Exemple  

## Citation  

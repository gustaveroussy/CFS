---
editor_options:
  markdown: null
output: pdf_document
---

# CellsFromSpace

CellsFromSpace (CFS) is an R package for spatial transcriptomics analysis using ICA. Documentation can be found at: [Tutorial and instructions](https://codimd.univ-rouen.fr/s/w0oZMV6fz)

# Table of Contents

-   [Installation](#installation)
-   [CFS Shiny interface](#Shiny)
    -   [Load data](#Load)
    -   [Pre-Processing](#process)
    -   [ICA](#ICA)
    -   [ICA Table](#table)
    -   [Visualization](#Viz)
    -   [Marker Table](#marker)
    -   [Output](#outputTab)
    -   [Other](#other)
-   [Examples](#examples)
    -   [No dataset](#No-dataset)
    -   [Visium dataset](#Visium-dataset)
-   [CFS functions](#Functions)
-   [Citation](#Citation)

------------------------------------------------------------------------

## Installation {#installation}

Installing the package from the github repository requires the package devtools which can be installed using the following command :

```{r}
install.packages("devtools")
```

The command line to install CFS :

```{r}
devtools::install_github("/gustaveroussy/CFS"
                         ,ref="main")
```

CFS has the following dependencies : - shiny - shinydashboard - shinyFiles - shinyWidgets - shinyalert - pheatmap - Seurat - plotly - raster - RColorBrewer - scatterpie - destiny - rclipboard - tibble - DT - interp - stringr - png - jpeg - heatmaply - scales - enrichR - e1071

## Usage

CFS contains functions for the processing of Spatial Transcriptomics (ST) data from all major sources as well as a companion shiny interface. Most pre-processing functionalities and all processing, analysis and visualization functionalities of CFS can be accessed through the companion shiny interface, providing an all-in-one easy-to-use analytical tool for ST data.

------------------------------------------------------------------------

## CFS Shiny interface {#Shiny}

Below is a rapid description of all panels found on the lefthand selection window. Panels are found in the order of standard analysis workflow.

All graphical outputs can be directly exported - as `.png` images using the built-in snapshot button in the top right of the plotting area or - as `.png`, `.jpg`, `.pdf`, or `.svg` with user-defined resolution using the download icon (left of the "info" button) in the figure title's banner

#### Keep in mind that objects are not saved dynamically. They need to be exported with the [Output](#output) tab. Save early and often to avoid bad surprises!

$~$

$~$

### Load data {#Load}

The load data window displays 4 data loading options:

-   **LoadVisium output folder :** Select the folder which contains Space Ranger output ("outs" folder). Necessary files will be loaded directly from the provided path for processing in the following Pre-Processing tab.
-   **Load processed data :** Select a Seurat RDS file previously pre-processed with CFS or not. Pre-processing outside of or within the shiny app (through the Pre-processing tab) is necessary to access CFS functionalities (`.rds`)
-   **Load high res image :** (Optional) Select the High resolution image from Space Ranger output (`.png`). This image will then be used for every spatial plot within the tool, offering higher quality visualization.
-   **Load full res image :** (Optional) Select the original full resolution image for this sample (`.jpg`). Spatial plots will then enable to visualize a full resolution zoom-in upon clicking on a single spot.

$~$

$~$

### Pre-Processing {#process}

Allows to carry out the ICA and functional enrichment from the output of Space ranger or any spatial Seurat object. Upon completion, resulting ICs can be visualized and annotated with the ICA tab

#### Options:

-   **Normalization Process**
    -   **Select organism :** Human or Mouse
    -   **Variable Features :** Number of variables features (genes) to use for normalization and analysis
-   **ICA process**
    -   **Number of ICs :** Number of ICs to generate
    -   **Maximum iterations :** Number of iteration of the ICA (recommended 600)
    -   **ICA fonction to run :** ICA algorithm to use (between icafast, icaimax and icajade)
    -   **Kurtosis filter :** Set the kurtosis value to filter ICs (default = 3)
    -   **Genes standard deviation :** Set sd threshold for the determination of IC contributive gene.
-   **Enrichment**
    -   **Enrichment database :** Select enrichment libraries from EnrichR, see <https://maayanlab.cloud/Enrichr/#libraries> for full list of avaialable libraries

#### Output {#output}

-   **Pre-processing :** displays the number of spots and features present in the object

$~$

$~$

### ICA {#ICA}

Cornerstone of the CFS workflow, this tab shows data related to individual ICs. Allows for the interpretation and annotation of each IC. *This is where the magic happens.* Hover on any point or bar for additional information.

**Choose IC to observe :** Dropdown menu of all ICs above the selected kurtosis threshold. Also displays the editable corresponding row in the IC annotation table for dierct input of user-defined annotation.

**IC top genes heatmap :** Displays a heatmap of the expression of top contributive genes across all ICs.

-   **Number of top genes per IC :** Determine the number of top n genes for each ICs to be plotted
-   **Select color :** color palette to use
-   **Color range :** Set the expression range for the visualization. Out-of-bounds values are squished
-   ***Cogwheel menu***
    -   Log scale: use log 10 transformation of gene expression
    -   Cluster ICs: cluster the ICs using the ward.D2 method. Shows IC similarity at a glance
    -   Kurtosis filter: display only ICs above the pre-processing kurtosis filter
    -   Invert Color scale: invert the selected color scale

**Plot IC weight :** Plot the spatial distribution of the selected IC

-   **Color Range :** Set the IC weight range for the visualization. Out-of-bounds values are squished
-   **Alpha type :** Set the type of transparency between constant (all spots have the same user-defined alpha) and scaling (spot alpha increases with weight)
-   **Alpha :** set max of the transparency value
-   **Spot size :** Set size of the displayed dots for each spot
-   **Select color :** color palette to use
-   ***Cogwheel menu***
    -   Invert Color scale: invert the selected color scale

**Plot IC related Genes weight** Spatial plots of gene expression.

-   **Choose gene to plot :** select the gene(s) to display (organized by importance in the IC)
-   **Alpha :** set max of the transparency value
-   **Spot size :** Set size of the displayed dots for each spot
-   **Select color :** color palette to use
-   ***Cogwheel menu***
    -   Invert Color scale: invert the selected color scale

**Build heatmap of genes related to IC** Heatmap representation of expression levels of top IC contributing genes for the selected IC across all ICs.

-   **Color Range :** Set the expression range for the visualization. Out-of-bounds values are squished
-   **Number of genes to display :** Top n contributive genes to be displayed
-   **Select color :** color palette to use
-   ***Cogwheel menu***
    -   Cluster ICs: cluster the ICs using the ward.D2 method. Shows IC similarity at a glance
    -   Invert Color scale: invert the selected color scale
-   Numbers of **total** and **positive** IC contributive genes displayed below the chart. gene lists can be copied to clipboard using the adjacent buttons.

**Build bar chart of enrichment** Bar chart representation of the enrichment analysis carried out by EnrichICA. bars are ordered by p-value with most significant ones on top. Bar length

-   **Choose database related to enrichment :** Select the EnrichR library results to display for the selected IC.
-   **Select color :** color palette to use
-   **Enrichment to display :** display the top n terms
-   **Gene expression :** Choose between *All* (enrichment of all IC contributive genes) *Positive* (Default option, enrichment of only positive IC contributive genes) *Negative* (enrichment of only negative IC contributive genes)
-   ***Cogwheel menu***
    -   Invert Color scale: invert the selected color scale

**Build heatmap of genes related to spot** Heatmap representation of expression levels of top IC contributing genes for the selected IC across all spots. Does not display if \>5000 spots.

-   **Color Range :** Set the expression range for the visualization. Out-of-bounds values are squished
-   **Number of genes to display :** display the top n genes
-   **Select color :** color palette to use
-   ***Cogwheel menu***
    -   Cluster cells: Cluster the spots based on expression

$~$

$~$

### ICA Table {#table}

Full IC annotation table. can be directly edited, downloaded or uploaded. If imported, ensure the table structure is followed and data is saved as a `.csv` using comma separators (not semicolon or spaces). We recommend downloading the empty table and filling outside of the shiny UI as data is not saved dynamically. The complete table can then be easily imported back into the UI. The searchbox is useful for quick corrections or selection of all ICs containing the searched keyword. Total number of kurtosis-filtered ICs is displayed below the table.

-   **Choose CSV File :** Import a `.csv` annotation table (CFS format needs to be followed rigorously)
-   **table :** IC annotation table containing 3 data columns:
    -   Use : Whether the IC is to be used for downstream analysis? (TRUE/FALSE)
    -   Type : Broad annotation of the IC
    -   Annotation : Detailed annotation of the IC
-   **Download table :** Export annotation table in CSV format

$~$

$~$

### Visualization {#Viz}

Calculation of the spot clustering and UMAP embedding and various data visualization options on both the UMAP (top window) and spatial (bottom window) embeddings.

**Select method to use :** Split into 3

-   **UMAP :** project the signal of choice onto the UMAP and spatial embeddings
    -   **Select what to color :** Allows the display of any metadata (such as clusters) as well as genes or ICs
    -   **Select color :** color palette to use for continuous data
    -   **Choose cell type to plot :** Select ICs with the defined "Type" for clustering and UMAP calculations
    -   **Choose IC to plot :** Select individual ICs for clustering and UMAP calculations. To automatically use all ICs, use the "Select all ICs" button at the bottom of the window
    -   **Plot resolution :** Resolution parameter for louvain clustering
    -   **Spread :** Spread parameter for UMAP calculation
    -   **When visualizing genes** & **ICs** :
        -   **Color Range :** Set the expression range for the visualization. Out-of-bounds values are squished
        -   **Alpha type :** Set the type of transparency between constant (all spots have the same user-defined alpha) and scaling (spot alpha increases with weight)
        -   **Alpha :** set max of the transparency value
        -   **Choose gene/IC :** select the gene/IC to display
-   **Density :** displays kernel density projection on both UMAP and spatial embeddings
    -   **Choose cell type to plot :** Select the IC "Type" to be displayed using kernel density
    -   **Display image :** Display image behind density
    -   **threshold :** Signal intensity threshold to display. values below this threshold are squished
    -   **alpha :** Transparency of spots
-   **Scatter Pie :** Deconvolute the signal in each spot by the relative weight of ICs
    -   **Choose cell type:** Select ICs to plot based on their annotated "Type". Mutliple types can be selected
    -   **Choose IC to plot:** Manually select ICs to display.

**Cogwheel menus :**

-   **Plot** window:
    -   Invert color scale: invert the selected color scale
    -   Display: Whether or not this plot should be rendered
    -   Display plotly grid: Whether or not to display the grid behind the UMAP
    -   Use ggplot scatter pie: *currently not functional*
    -   Full annotation of scatter: Display enhanced annotations upon hovering over spots with detailed IC annotation for the top ICs of each spot
    -   Display only the UMAP of the currently selected image: When working with integrated objects with multiple images, display only the spots of the selected sample
    -   Spot size: Adjust the size of each plotted spot
-   **Plot_Spatial** window:
    -   Invert color scale: invert the selected color scale
    -   Display: Whether or not this plot should be rendered
    -   Use ggplot: *currently not functional*
    -   Full annotation for scatter: Display enhanced annotations upon hovering over spots with detailed IC annotation for the top ICs of each spot
    -   Display image: Toggle de display of the background H&E image. *Not 100% functional*
    -   Black background for scatter pie: Use a black background for scatter pies for increased visibility.
    -   Spot size: Adjust the size of each plotted spot

Contrary to visualizations in the ICA tab, plots are not dynamically rendered by changing parameters. the "Start plot" button must be pressed to render new plots because some renderings are computationally intensive (scatter plots most notably). To increase performance, disable undesired plot renderings by ticking off the "Display" box in the cogwheel menus.

New UMAP and clustering calculations are made upon clicking the "Start plot" button whenever ICs are selected in the **Choose cell type to plot** or **Choose IC to plot** fields. Otherwise, the values from the last calculations are used. Previous clustering calculations made with different **resolution** parameters are stored in the metadata.

$~$

$~$

### Marker table {#marker}

Once clustering analysis has been carried out in the **Visualization** tab, differential gene expression analysis can be initiated and visualized within this tab for each clusters.

**Choose cluster marker gene:** Select the cluster to interrogate by rendering the volcano plot and marker gene table which can be downloaded using the "Download Table" button at the end of the tab.

#### Options

-   **p-value:** p-value threshold to cut off genes
-   **top genes:** n top genes to label on the volcano plot.
-   **Alpha:** set max of the transparency value

$~$

$~$

### Output {#outputTab}

Configure the export parameters for the analysed object. Writes out an `.rds` object containing the specified features. Also enables dataset subclustering by cluster identity or IC "Type" density.

#### Options

-   **Type of Data to add to the downloaded data**: Select which additional features, if present, to include in the output object. Press *Download RDS* to start the export procedure for the full object or *Download subcluster* for the subset object. May take some time depending on data size:
    -   Output Annotation table: Include IC annotation table
    -   Output UMAP + Clustering: include UMAP embeddings and clustering metadata
    -   Output marker genes: Include marker gene tables from marker table for each clusters
    -   Download RDS
-   **Type of Data to subcluster**:
    -   UMAP Cluster: subset data by cluster cluster identity
    -   IC Cell Types:
        -   Manual : *currently not functional*
        -   Automated: subsets the object based on IC signal in each spots.
            -   *Choose cell type to export*: Export all spots containing ICs with the specified "Type".\
            -   *Density threshold*: Uses kernel density calculations of the specified "Type" for subsetting. Spots above the defined *Density threshold* (between 0 et 1) are exported

$~$

$~$

### Other options {#other}

The side menu also offers some additional options for object visualization:

-   **List of ICs**: *Not currently functional*
-   **Image to display**: When multiple images are found within the loaded object, select the image to be used in spatial plots across all tabs
-   mirror X, mirror Y, 90° turn: mirror or rotate spot position. Useful for datasets with no underlying image (such as Curio Seeker, nanostring CosMX, or Vizgen MERSCOPE)

$~$

$~$

------------------------------------------------------------------------

## Examples {#examples}

The entirety of the pipeline can be accomplished from the visium output to the results interpretation. The package uses the Seurat format for Visium analysis.

### No dataset {#no-dataset}

The analysis can be directly accomplished within a Seurat object directly by following the following steps in R:

```{r}
library(CFS)
library(Seurat)
```

-   Loading visium object:

```{r}
data <- Load10X_Spatial(
  data.dir,
  filename = "filtered_feature_bc_matrix.h5",
  assay = "Spatial",
  slice = "slice1",
  filter.matrix = TRUE,
  to.upper = FALSE,
  image = NULL,
  ...
)
```

-   normalisation step

```{r}
data <- PrepNormData(data=data,organism="Hs"/"Mm",variable_features=2000)
```

-   ICA step

```{r}
data <- ICASpatial(data=data,nics=100,maxit=600,method="icafast",kurtosis = 3,sd=3)
```

-   enrichment step

```{r}
data = Enrich_ICA(data=data,
                  dbs=c("GO_Biological_Process_2015"),
                  kurtosis = 3, 
                  overwrite = TRUE)
```

Once this step is reached, the shiny tool is necessary to annotate the ICA and observe the outputs from the analysis. To be loaded the Seurat object needs to be saved into an RDS format using the command `saveRDS`:

```{r}
saveRDS(data, file = "my_data.rds")
```

-   Annotation in Shiny RDS object can be loaded into CFS using the Load processed data loading bar.

![](https://codimd.univ-rouen.fr/uploads/1ddedebd-d3d0-43ab-9e2e-1c89a332fe19.png)

### Using the shiny tool

The analysis can be fully carried out in the shiny tool. For this, to launch the shiny tool in r, start with:

```{r}
library(CFS)
launchCFS()
```

#### Load Data

-   Loading Visium output Visium output folder can be loaded in the load data window in the Load Visium output folder Select folder. This will load a Seurat object that needs to be preprocessed.

-   Load high res image: The high res image can be loaded from the visium output folder to be display on spatial plot

-   Load ful res image: Load the image of the full res image given to space Ranger before processing

#### Pre-Processing

![](https://codimd.univ-rouen.fr/uploads/605dac18-3129-42e4-8de9-92087d462ca8.png) The Pre-Processing tab is used to carry out the functions PrepNormData, ICASpatial and Enrich_ICA before ICs annotation, arguments have the same use as the functions.

#### ICA {#ica}

This tab allow to display the different properties of each ICs to determine annotations. Annotation can be filled from this page directly for the ICs - Choose IC to observe Choose which ICs to observe the properties of. ![](https://codimd.univ-rouen.fr/uploads/7fb066e6-a3c1-47e4-b711-39ddc296fb6b.png) - Annotation table Display the line in the annotation table related to the IC, can be filled from here. ![](https://codimd.univ-rouen.fr/uploads/e7ff2f9c-908f-41dc-8f83-f5d1299adefc.png) - IC top genes heatmap This window can be used directly to observe each of the ICs and their top genes compared to one another. ![](https://codimd.univ-rouen.fr/uploads/06bfdd91-f09f-4b17-bc5b-332e610ec7e3.png) - Plot IC weight Use this window the localisation of the IC's signal ![](https://codimd.univ-rouen.fr/uploads/0a3fc15c-ed5c-4ff4-b998-758f9a564335.png) - Plot IC related Genes weight Use this window to determine the top genes of each ICs and their spatial localisation ![](https://codimd.univ-rouen.fr/uploads/cd9f629f-b919-4c8e-abf2-340f2c995fdc.png) - Build heatmap of genes related to IC Use this window to see the top genes of the selected IC in a heatmap format compared to other ICs. ![](https://codimd.univ-rouen.fr/uploads/4ff5746d-d93b-4a3c-8ed3-c690a87e3bc5.png) - Build bar chart of enrichment Use this to see the enrichment of genes related to the IC. ![](https://codimd.univ-rouen.fr/uploads/b528ae1e-3075-4290-aff9-8b6b12ab9d2b.png) - Build heatmap of genes related to spot Build the heatmap between each spots and the expression of the genes related to the IC (\<5000 spots) ![](https://codimd.univ-rouen.fr/uploads/989eaa16-ffa0-401a-b338-a9d9d326597b.png)

#### ICA table

Once the ICA table has been filled, You can check the table with all of the annotations in this window. Check if all of the petinent ICs are annotated and if the Use column is filled properly. ![](https://codimd.univ-rouen.fr/uploads/273208b5-f41c-43b7-9034-568a1bc10f4c.png)

#### Visualisation

Use this window to display the metadata, densi display and scatter pie as explained above ![](https://codimd.univ-rouen.fr/uploads/85560f95-3230-4b93-be80-995f63d5d2f1.png) ![](https://codimd.univ-rouen.fr/uploads/9a1e752b-36fb-44a8-a0c3-c5d1f783a530.png)

#### Output

If you wish to record all changes made in CFS Shiny, go to the output window and follow the the instruction explained above. ![](https://codimd.univ-rouen.fr/uploads/448fab46-c442-4051-90a8-44dc2f368327.png)

### Visium dataset {#visium-dataset}

#### Preprocessing

##### Without the Shiny tool

-   Loading the Seurat stxBrain dataset

``` r
library(Seurat)
library(SeuratData)
InstallData("stxBrain")
brain <- LoadData("stxBrain", type = "anterior1")
```

-   Preprocessing

``` r
brain = PrepNormData(data=brain,organism="Hs",variable_features=20000)
brain = ICASpatial(data=brain,nics=100,maxit=600,method="icafast", kurtosis = 3, sd=3)
brain = Enrich_ICA(data=brain, dbs=c("PanglaoDB_Augmented_2021",
                                "CellMarker_Augmented_2021",
                                "Azimuth_Cell_Types_2021",
                                "Tabula_Sapiens",
                                "Tabula_Muris",
                                "GO_Biological_process_2023",
                                "GO_Molecular_Function_2023",
                                "GO_Cellular_Compartment_2023",
                                "MSigDB_Hallmark_2020",
                                "Reactome_2022",
                                "KEGG_2021_Human"),
                    kurtosis = 3,
                    overwrite = TRUE)
```

##### With the Shiny tool

-   Use the load folder button
-   Start the preprocessing window

#### IC annotation

The IC annotation is accomplished with the shiny tool. Fill the annotation table using the help of the ICA tab.

## CFS functions {#Functions}

### launchCFS

#### usage

Initiates the shiny UI for data preprocessing, analysis, annotation and visualisation. First initiation of the shiny UI may take a while as a miniconda environment is automatically created with the requisite python libraries. The entire analysis pipeline can be accomplished from within the tool (see examples below). The other functions listed below allow for data preprocessing outside the shiny UI should the user prefer to do so.

#### arguments

-   offline_mode : boolean; if TRUE, disables initiation of connection to external database for pathway enrichment at start-up.

#### example

```{r}
launchCFS()
```

### PrepNormData

#### usage

Determine mitochondrial and hemoglobin gene ratios, normalises and scales counts using Seurat's SCTransform function.

#### arguments

-   data : Seurat object to analyse
-   organism : "Hs" (human) or "Mm" (mice) (default = "Hs")
-   variable_features : Number of variable features to use for the SCTransform (default = 20000)

#### output

a Seurat object containing an SCT assay.

#### example

```{r}
data <- PrepNormData(data=data,organism="Hs",variable_features=2000)
```

### ICASpatial

#### usage

Run the Independant Component Analysis on the seurat object outputed from PrepNormData. Proceeds to IC sign correction and statistical analysis of ICs and IC contributive genes.

#### arguments

-   data : Seurat object to analyse
-   nics : Number of independant component to separate in the sample (default=100)
-   maxit : Number of iterations (default = 600)
-   method : Method to use between "icafast", "icaimax", "icajade" (default = "icafast")
-   kurtosis : Value of the kurtosis filter on IC (default = 3)
-   sd : Value of the normal standard deviation used for ICA contributive gene determination (default = 3)

#### output

a Seurat object containing "ica" reduction slot and annotation tables in the "misc" slot

#### example

```{r}
data <- ICASpatial(data=data,nics=100,maxit=600,method="icafast",kurtosis = 3,sd=3)
```

### Enrich_ICA

#### usage

Search and enrich IC contributive genes using EnrichR.

#### arguments

-   data : Seurat object\
-   dbs : names of enrichR compatible libraries, see <https://maayanlab.cloud/Enrichr/#libraries> for full list of avaialable libraries (default = "GO_Biological_Process_2015")
-   kurtosis : kurtosis filter value (default = 3)
-   overwrite : boolean; overwrite preexisting annotations in the object. (default = TRUE)

#### output

a Seurat object containing annotation tables for each IC in the "misc" slot

#### example

```{r}
data = Enrich_ICA(data=data,dbs=c("GO_Biological_Process_2023", "Reactome_2022"), kurtosis = 3, overwrite = TRUE)
```

### Create_vizgen_seurat

#### usage

Convert transcripts table output from Vizgen's MERSCOPE experiment into a Seurat object using a grid signal binning method.

#### arguments

-   spot_gene_expression : table of transcripts
-   pixel_format : bin size in unit used in the transcrpits table. Usually µm for MERSCOPE output (default = 40)
-   min.features : threshold of number of transcripts for pseudospot to be included. (default = 5)

#### output

a Seurat object

#### example

```{r}
data <- Create_vizgen_seurat(spot_gene_expression=transcript_table,
                             pixel_format = 40,
                             min.features = 5)
```

### Create_CosMX_seurat

#### usage

Convert transcripts table output from Nanostring's CosMX experiment into a Seurat object using a grid signal binning method.

#### arguments

-   tx : table of transcripts
-   pixel_format : bin size in unit used in the transcrpits table. Usually pixels for CosMX output (default = 600)
-   min.features : threshold of number of transcripts for pseudospot to be included. (dafault = 5)

### output

a Seurat object

#### example

```{r}
data <- Create_CosMX_seurat(tx=transcript_table,
                            pixel_format = 40,
                            min.features = 5)
```

### Cluster_ICA

#### usage

Run neighbor analysis, spot clustering cells and calculate UMAP embedding for

#### arguments

-   data : Seurat object containing "ica" reduction slot and annotation
-   ICs : Vector of IC numbers to use for the clustering and UMAP calculations
-   res : Resolution of the clustering (default = 1.2)
-   spread : Spread parameter for UMAP calculation (default = 3)

#### output

a Seurat object containing "snn" graphs, "umap" reduction and clustering metadata

#### example

```{r}
data <- Cluster_ICA(data=data,
                    ICs=c(1,2,3,4,5,6,7,8,9),
                    res=1.2)
```

## Citation {#citation}

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" alt="Licence Creative Commons" style="border-width:0"/></a><br />This work is distributed under the the terms of the creative commons attribution <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Licence Creative Commons Attribution - No commercial use - shared in the same conditions 4.0 International</a>.

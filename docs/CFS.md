CellsFromSpace (CFS) is an R package for spatial transcriptomics analysis using ICA. Documentation can be found at: [Tutorial and instructions](https://codimd.univ-rouen.fr/s/w0oZMV6fz)

# Table of Contents

-   [Installation](#installation)
-   [CFS Shiny interface](#Shiny)
    -   [Load data](#Load)
    -   [Pre-Processing](#process)
    -   [ICA](#ICA)
    -   [IC gene weights](#ICweights)
    -   [ICA Table](#table)
    -   [Visualization](#Viz)
    -   [Marker Table](#marker)
    -   [Output](#outputTab)
    -   [Other](#other)
-   [CFS functions](#Functions)
-   [Citation](#Citation)

------------------------------------------------------------------------

## Installation {#installation}

CellsFromSpace can be installed from the github repository using the package devtools :

``` r
install.packages("devtools")
devtools::install_github("/gustaveroussy/CFS"
                         ,ref="main")
```

To install the development branch with additional (potentially unstable) features, use:

``` r
devtools::install_github("/gustaveroussy/CFS"
                         ,ref="development")
```

CellsFromSpace has the following dependencies : 

-   shiny 
-   shinydashboard 
-   shinyFiles 
-   shinyWidgets 
-   shinyalert 
-   pheatmap 
-   Seurat 
-   plotly 
-   raster 
-   RColorBrewer 
-   scatterpie 
-   destiny 
-   rclipboard 
-   tibble 
-   DT 
-   interp 
-   stringr 
-   png 
-   jpeg 
-   heatmaply 
-   scales 
-   enrichR 
-   e1071


## Usage

CFS contains functions for the processing of Spatial Transcriptomics (ST) data from all major sources as well as a companion shiny interface. Most pre-processing functionalities and all processing, analysis and visualization functionalities of CFS can be accessed through the interactive shiny interface, providing an all-in-one easy-to-use analytical tool for ST data.

------------------------------------------------------------------------

## CFS Shiny interface {#Shiny}

The Shiny graphical interface for CFS is initiated with

``` r
launchCFS()
```

**Note** that the first launch of CFS will initiate the creation of a python environment with required libraries and as such will necessitate an internet connection and may take several minutes.

$~$

Below is a detailed description of all panels found on the lefthand selection window. Panels are found in the order of standard analysis workflow.

All graphical outputs can be directly exported 

- as `.png` images using the built-in snapshot button in the top right of the plotting area or 
- as `.png`, `.jpg`, `.pdf`, or `.svg` with user-defined resolution using the download icon (left of the "info" button) in the figure title's banner

#### Keep in mind that objects are not saved dynamically. They need to be exported with the [Output](#output) tab. Save early and often to avoid bad surprises!


$~$

### Load data {#Load}

The load data window displays 5 data loading options:

-   **LoadVisium output folder :** Select the folder which contains Space Ranger output ("outs" folder). Necessary files will be loaded directly from the provided path for processing in the following Pre-Processing tab.
-   **Load processed data :** Select a Seurat RDS file previously pre-processed with CFS or not. Pre-processing outside of or within the shiny app (through the Pre-processing tab) is necessary to access CFS functionalities (`.rds`)
-   **Load high res image :** (Optional) Select the High resolution image from Space Ranger output (`.png`). This image will then be used for every spatial plot within the tool, offering higher quality visualization.
-   **Load full res image :** (Optional) Select the original full resolution image for this sample (`.jpg`). Spatial plots will then enable to visualize a full resolution zoom-in upon clicking on a single spot.
-   **Load Visium output folder Integration :** Select multiple Space Ranger "outs" folders to be integrated for downstream analysis. The designated folders must be uniquely named as the folder name will be assigned as sample name.

$~$

$~$

### Pre-Processing {#process}

Carry out the pre-processing steps of the loaded object. Launching this step will normalize data, run ICA, filter ICs and correct sign, identify contributive genes and query enrichment databases. 

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
    -   **Enrichment database :** Select enrichment libraries from EnrichR, see <https://maayanlab.cloud/Enrichr/#libraries> for full list of available libraries
      -  *Only process positively associated genes :* Run EnrichR functional enrichment only on the positive

#### Output {#output}

-   **Pre-processing :** displays the number of spots and features present in the object

$~$

$~$

### ICA {#ICA}

Cornerstone of the CFS workflow, this tab shows data related to individual ICs. Allows for the interpretation and annotation of each IC. *This is where the magic happens.* Hover on any point or bar for additional information.

**Choose IC to observe :** Dropdown menu of all ICs above the selected kurtosis threshold. Also displays the editable corresponding row in the IC annotation table for dierct input of user-defined annotation.

**Global ICA heatmap :** Displays a heatmap of the expression of top contributive genes across all ICs.

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

**IC-specific heatmap** Heatmap representation of expression levels of top IC contributing genes for the selected IC across all ICs.

-   **Color Range :** Set the expression range for the visualization. Out-of-bounds values are squished
-   **Number of genes to display :** Top n contributive genes to be displayed
-   **Select color :** color palette to use
-   ***Cogwheel menu***
    -   Cluster ICs: cluster the ICs using the ward.D2 method. Shows IC similarity at a glance
    -   Invert Color scale: invert the selected color scale
-   Numbers of **total** and **positive** IC contributive genes displayed below the chart. gene lists can be copied to clipboard using the adjacent buttons.

**Functional enrichment** Bar chart representation of the enrichment analysis carried out by EnrichICA. bars are ordered by p-value with most significant ones on top. Bar length

-   **Database selection :** Select the EnrichR library results to display for the selected IC.
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

### IC gene weights {#ICweights}

Searchable table of gene weights per IC. Genes can be filtered to query gienes of interest using the top left search box.

When integrated analysis of multiple samples is performed, the **Sample based dotplot** shows the global distribution of ICs across samples with dot size representing the proportion of spots in each sample above the percentile threshold and the color indicating the percentile threshold value per sample. These metrics indicate samples where specific ICs are enriched or absent.

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

Calculation of the spot clustering and UMAP/tSNE embedding and various data visualization options on both the UMAP/tSNE (top window) and spatial (bottom window) embeddings.

**Clustering parameters :** 

-   **Choose cell type to cluster :** Automatically selects ICs with the defined "Type" for clustering calculation
-   **Choose IC to cluster :** (Alternative to above) Manually select ICs for clustering calculation 
-   **Select Algorithm :** Dropdown menu of available clustering algorithms. 
-   **Clustering resolution :** Select the resolution setting for the clustering calculation. Typically between 0.1 and 5. Smaller values generate fewer clusters.
-   **Clustering name :** Enter custom clustering ID to be stored in the object metadata
-   ***Select all ICs :*** Automatically enters all ICs in the **Choose IC to cluster** field
-   ***Start Cluster :*** Launch clustering. A progress bar appears in the bottom right corner of the shiny window

**Dimension reduction parameters :** 

-   **Select method to use :** Choose between UMAP and tSNE visual dimensionality reduction (Dimred)
-   **Dimred name :** Enter custom name for calculated Dimred
-   **Choose cell type for reduction :** Automatically selects ICs with the defined "Type" for Dimred calculation
-   **Choose IC for reduction :** (Alternative to above) Manually select ICs for Dimred calculation
-   **n.neighbors :** Hyperparameter for UMAP calculation. Higher values favor global structure preservation at the expense of local structure. Typical range: 5-50
-   **min.dist :** Hyperparameter for UMAP calculation. Controls compactness of local structures by setting the minimum distance between points in the embedding. Smaller values optimise local structure accuracy. Typical range: 0.001-0.5
-   **Spread :** Hyperparameter for UMAP calculation. Scaling parameter of the embedding. In combination with min.dist, controls the compactness of the UMAP embedding. Typical range: 0.1-10
-   **Perplexity :** Hyperparameter for tSNE calculation. Relates to the number of nearest neighbors for tSNE calculation. Typical range: 5-50
-   ***Select all ICs :*** Automatically enters all ICs in the **Choose IC to cluster** field
-   ***Calculate DimRed :*** Launch Dimred calculation

**Display parameters :** 

-   **Dimensional reduction :** Project the signal of choice onto the Dimred and spatial embeddings
    -   **Reduction to display :** Select which Dimred embedding to use
    -   **Select what to color :** Select data to project. Can be any column of the metadata table, calculated clusters, genes or ICs
    -   **Select color :** Color palette to use for continuous data
    -   **When visualizing genes** & **ICs** :
        -   **Choose gene/IC :** Select the feature to display
        -   **Color Range :** Set the expression range for the visualization. Out-of-bounds values are squished
        -   **Alpha type :** Set the type of transparency between constant (all spots have the same user-defined alpha) and scaling (spot alpha increases with weight)
        -   **Alpha :** set max of the transparency value
-   **Density :** displays kernel density projection on both UMAP and spatial embeddings
    -   **Reduction to display :** Select which Dimred embedding to use
    -   **Select what to color :** Select data to project underneath density. Can be any column of the metadata table, calculated clusters, genes or ICs
    -   **Select color :** Color palette to use for continuous data
    -   **Choose cell type for density :** Select the IC "Type" to be displayed using kernel density
    -   **threshold :** Signal intensity threshold to display. values below this threshold are squished
    -   **alpha :** Transparency of underlying spots
-   **Scatter Pie :** Deconvolute the signal in each spot by the relative weight of ICs
    -   **Reduction to display :** Select which Dimred embedding to use
    -   **Values for Scatterpie :** Choose between IC and metadata
    -   ***Pie chart sized by signal intensity :*** Check to have pie charts sized proportionally to total signal of individual spots
    -   **Choose cell type:** (if `IC` is selected above) Select ICs to plot based on their annotated "Type". Mutliple types can be selected
    -   **Choose IC to plot:** (Alternative to automatic IC Type) Manually select ICs/metadata to display.

**Cogwheel menus :**

-   **Plot** window:
    -   Invert color scale: invert the selected color scale
    -   Display plotly grid: Whether or not to display the grid behind the UMAP
    -   Use ggplot scatter pie: *currently not functional*
    -   **Select annotation :** Dropdown menu to select the information provided when hovering over spots. Choice between `None`, `IC`, `Mean IC`, `Full annotation` & `Mean full annotation`
    -   Display only the UMAP of the currently selected image: When working with integrated objects with multiple images, display only the spots of the sample selected in the leftmost menu `Sample to display`
    -   Spot size: Adjust the size of each plotted spot
-   **Plot_Spatial** window:
    -   Invert color scale: invert the selected color scale
    -   Use ggplot: *currently not functional*
    -   **Select annotation :** Dropdown menu to select the information provided when hovering over spots. Choice between `None`, `IC`, `Mean IC`, `Full annotation` & `Mean full annotation`
    -   Display image: Toggle de display of the background H&E image. *Not 100% functional*
    -   Black background for scatter pie: Use a black background for scatter pies for increased visibility.
    -   Spot size: Adjust the size of each plotted spot

Contrary to visualizations in the ICA tab, plots are not dynamically rendered by changing parameters. User can select whether to render only Dimred or Spatial plots or both simultaneously using the `Display Dimred`, `Display Spatial` and `Display both` buttons respectively.

$~$

$~$

### Marker table {#marker}

Once clustering analysis has been carried out in the **Visualization** tab, differential gene expression analysis can be initiated and visualized within this tab for the calculated clusters. The FindMarkers Seurat function is used for DGE analysis.

**Choose cluster marker gene:** After DGE calculation has been made, Use this dropdown menu to select the Differential gene expression pair to interrogate. Renders a volcano plot for the selected comparison and displays the marker gene table which can be downloaded using the `Download Table` button at the end of the tab.

#### Options

-   **Log2 fold change :** Log2 fold change threshold selector for volcano plot visualization
-   **Cluster list to compare :** Dropdown menu to select the clustering metadata slot to interrogate
-   **Clustering method :** Dropdown menu to select the computing method for DGE. Default: Wilcoxon Rank Sum test (wilcox)
-   **Clusters to compare :** Select the clusters to be analyzed. If:
        -   ***a single cluster is selected :*** test this cluster versus all others combined
        -   ***two clusters are selected :*** Test each cluster against the other
        -   ***more than two clusters are selected :*** iteratively test each cluster against the others combined
-   **p-value :** p-value threshold to cut off genes for volcano plot visualization
-   **top genes :** n top genes to label on the volcano plot.
-   **Alpha :** Transparency value selector for volcano plot.

$~$

$~$

### Output {#outputTab}

Configure the export parameters for the analysed object. Writes out an `.rds` object containing the specified features. Also enables dataset subclustering by cluster identity or IC "Type" density.

#### Options
**Download Output**

-   **Type of Data to add to the downloaded data**: Select which additional features, if present, to include in the output object. Press *Download RDS* to start the export procedure for the full object or *Download subcluster* for the subset object. May take some time depending on data size:
    -   Output Annotation table: Include IC annotation table
    -   Output UMAP + Clustering: include UMAP embeddings and clustering metadata
    -   Output marker genes: Include marker gene tables from marker table for each clusters
    -   `Download RDS` button to initiate RDS creation. May take some time depending on object size. A pop-up window will appear when the object has been created to specify the output folder. **Do not close CFS immediately after selecting the folder**: writing the object from memory to the disk can take some more time
    
**Download Subset**

-   **Type of Data to subcluster :**:
-   **Choose type of subclustering :** Select between modes of subclustering: `UMAP Cluster` or `IC Cell Types`
    -   `UMAP Cluster`:
        -   **Choose cluster to export :** Select clusters of the *seurat_clusters* metadata to use for subsetting
    -   `IC Cell Types`:
        -   **Select method :** Choose between `Manual` and `Automated` method of spot selection
            -   `Manual` : *currently not functional*
            -   `Automated`: subsets the object based on IC signal in each spots.
                -   **Choose cell type to export :** Export all spots containing ICs with the specified "Type".\
                -   **Density threshold :** Uses kernel density calculations of the specified "Type" for subsetting. Spots above the defined Density threshold (between 0 et 1) are exported

$~$

$~$

### Other options {#other}

The side menu also offers some additional options for object visualization:

-   **List of ICs**: *Not currently functional*
-   **Sample to display**: When multiple samples are found within the loaded object, select the sample(s) to be used in spatial plots across all tabs. For computational and rendering visibility concerns (depending on screen size), it is not advised to select more than 12 samples simultaneously.
- `Select all samples` button automatically fills the field above with all samples found within the object
-   mirror X, mirror Y, 90° turn: mirror or rotate spot position. Useful for datasets with no underlying image (such as Curio Seeker, nanostring CosMX, or Vizgen MERSCOPE)

$~$

$~$

------------------------------------------------------------------------


## CFS functions {#Functions}

### launchCFS

#### usage

Initiates the shiny UI for data preprocessing, analysis, annotation and visualization. First initiation of the shiny UI may take a while as a miniconda environment is automatically created with the requisite python libraries. The entire analysis pipeline can be accomplished from within the tool (see examples below). The other functions listed below allow for data preprocessing outside the shiny UI should the user prefer to do so.

#### arguments

-   offline_mode : boolean; if TRUE, disables initiation of connection to external database for pathway enrichment at start-up.

#### example

``` r
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

``` r
data <- PrepNormData(data=data,organism="Hs",variable_features=2000)
```

### ICASpatial

#### usage

Run the Independant Component Analysis on the seurat object outputed from PrepNormData. Proceeds to IC sign correction and statistical analysis of ICs and IC contributive genes.

#### arguments

-   data : Seurat object to analyse
-   ncis : Number of independant component to separate in the sample (default=100)
-   maxit : Number of iterations (default = 600)
-   method : Method to use between "icafast", "icaimax", "icajade" (default = "icafast")
-   kurtosis : Value of the kurtosis filter on IC (default = 3)
-   sd : Value of the normal standard deviation used for ICA contributive gene determination (default = 3)

#### output

a Seurat object containing "ica" reduction slot and annotation tables in the "misc" slot

#### example

``` r
data <- ICASpatial(data=data,ncis=100,maxit=600,method="icafast",kurtosis = 3,sd=3)
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

``` r
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

``` r
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

``` r
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

``` r
data <- Cluster_ICA(data=data,
                    ICs=c(1,2,3,4,5,6,7,8,9),
                    res=1.2)
```

## Citation {#citation}

This tool is not yet published, in the meantime, please cite our [preprint](https://www.biorxiv.org/content/10.1101/2023.08.30.555558v2)
``` r
@article {Thuilliez2023.08.30.555558,
	author = {Corentin Thuilliez & Gaël Moquin-Beaudry et al.},
	title = {CellsFromSpace: A fast, accurate and reference-free tool to deconvolve and annotate spatially distributed Omics data},
	elocation-id = {2023.08.30.555558},
	year = {2024},
	doi = {10.1101/2023.08.30.555558},
	publisher = {Cold Spring Harbor Laboratory},
	URL = {https://www.biorxiv.org/content/early/2024/02/15/2023.08.30.555558},
	eprint = {https://www.biorxiv.org/content/early/2024/02/15/2023.08.30.555558.full.pdf},
	journal = {bioRxiv}
}

``` 

## License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" alt="Licence Creative Commons" style="border-width:0"/></a><br />This work is distributed under the the terms of the creative commons attribution <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Licence Creative Commons Attribution - No commercial use - shared in the same conditions 4.0 International</a>.

---
editor_options:
  markdown: null
title: Visium Tutorial
output:
  html_document:
    df_print: paged
---

CellsFromSpace (CFS) was designed to provide all the tools to process, interpret, and analyse spatial transcriptomics data by non-bioinformaticians. As such, the entirety of the processing and analysis pipeline can be run using CFS' shiny graphical interface. For 10X Genomics Visium datasets, processing can be initiated with SpaceRanger output files.

# Table of Contents

-   [Installation](#installation)
-   [Preprocessing](#Preprocessing)
    -   [Interactive preprocessing](#Shiny)
    -   [Programmatic preprocessing](#Prog_Preproc)
-   [ICA analysis](#Analysis)
    -   [IC annotation](#ICA)
    -   [IC gene weight](#Geneweight)
    -   [ICA table](#ICAtable)
    -   [Visualization](#Viz)
    -   [Marker table](#marker)
    -   [Output](#outputTab)
-   [Object subsetting and reanalysis](#subset)
-   [Citation](#Citation)


------------------------------------------------------------------------

# Installation {#installation}

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

------------------------------------------------------------------------

CellsFromSpace is principally meant to be used locally through the interactive Shiny Interface. While computationally lightweight, sample processing and visualization can be resource intensive for larger datasets, especially regarding memory. For standard unique Visium datasets, we recommend at least 16GB of RAM and larger amounts for integrated datasets.

# Preprocessing {#Preprocessing}

To analyse Visium data using CellsFromSpace, SpaceRanger outputs must first be processed to generate Seurat objects with he appropriate structure. This step can be done either within CFS' [interactively](#Shiny) or [programmatically](#Prog_Preproc).

In both cases, initiate *CFS* and *Seurat* using

``` r
library(CFS)
library(Seurat)
```

For this tutorial, the [Adult Mouse Brain (FFPE)](https://www.10xgenomics.com/datasets/adult-mouse-brain-ffpe-1-standard-1-3-0) 10X Visium dataset is used. To follow along, you may download the output files `Feature / barcode matrix HDF5 (filtered)` and `Spatial imaging data` from the 10X website linked above or import the corresponding *Seurat* object directly using the *SeuratData* package and save it on your computer as an `.rds` file.

``` r
library(SeuratData)

InstallData("stxBrain")
data <- LoadData("stxBrain", type = "anterior1")

saveRDS(data, file="mouse_brain.rds")
```


## Interactive Preprocessing {#Shiny}


### Load Data 

Multiple Load options are offered through CFS' interactive Shiny interface. 

[Load_Visium image] ![](https://codimd.univ-rouen.fr/uploads/1ddedebd-d3d0-43ab-9e2e-1c89a332fe19.png)

- Unprocessed data generated through the SpaceRanger fastq processing pipeline can be loaded directly into CFS by pressing the `Select a folder` button under **Load Visium output folder** (Red ellipse in the screenshot above). **Note** that not all files are essential for object initiation. Only the *filtered_feature_bc_matrix.h5* file and *spatial* folder are necessary. The `outs` folder may also be renamed. Renaming of the `outs` folder (to reflect sample name) is necessary for multi-sample analysis.

[Load_Visium_folder image]

- Preprocessed spatial *Seurat* objects can also be imported to CFS by selecting the appropriate `.rds` file in the **Load processed data** section (Green ellipse in the screenshot above).

- (Optional) uploading high resolution (**Load high res image**) and full resolution images (**Load full res image**) for improved visualization functionalities within the CFS interactive tool is also done on this tab.

$~$

### Pre-Processing

The Pre-Processing tab allows the user to select the parameters used for ICA calculation, filtering and functional enrichment analysis. Default values will be sufficient for most samples. We recommend a **Number of ICs** value between 50-100 for most analyses to capture all potential biological sources. Overdecomposed or redundant ICs can subsequently be removed or collapsed, so the impact of overshooting is minimal. See the [Programmatic Preprocessing](#Prog_Preproc) section below for a more detailed explanation of preprocessing steps.

[Preprocessing image]

$~$

### Programmatic Preprocessing {#Prog_Preproc}

CellsFromSpace also allows for programmatic pre-processing of ST data using the Seurat object format. This feature is useful for non-standard formats or pre-processing automation

<details><summary><b>Programmatic pre-processing</b></summary>

#### Loading Visium object:

The default Load10X_Spatial function generates a Seurat object from SpaceRanger ouputs
    
``` r
data.dir <- "pathway/to/SpaceRanger/outs/"
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

$~$

#### Normalisation

Object normalization is done using CFS' custom feature `PrepNormData`, a wrapper for *Seurat*'s `SCTransform` function:

``` r
data <- PrepNormData(data=data,organism="Hs",variable_features=20000)
```

$~$

#### ICA

ICA computing, IC fitlering with kurtosis and identification of IC contributory genes is done with the `ICASpatial` function. By default, 100 components are calculated with the *icafast* method and automatic IC filtering is done with a kurtosis value of 3 (through the `kurtosis` parameter), meaning all ICs with low tailedness of the distribution are filtered out. This value can be increased for more stringent filtering at the expense of potential loss of biologically relevant signal. Identification of IC contributory genes is done through automatic thresholding (through the `sd` parameter) of feature loadings by z-score. Genes with |z-score| above threshold are considered contributory.

``` r
data <- ICASpatial(data=data,ncis=100,maxit=600,method="icafast",kurtosis = 3,sd=3)
```

$~$

#### Functional enrichment

Essential part of the IC annotation pipeline, the functional enrichment step is done with the `Enrich_ICA` function and automatically queries the contributory gene lists (positive, negative and total) for each IC against the specified *EnrichR* libraries (`dbs` parameter). 

``` r
data = Enrich_ICA(data=data,
                  dbs=c("PanglaoDB_Augmented_2021",
                                "CellMarker_Augmented_2021",
                                "Azimuth_Cell_Types_2021",
                                "Tabula_Sapiens",
                                "Tabula_Muris",
                                "GO_Biological_Process_2023",
                                "GO_Molecular_Function_2023",
                                "GO_Cellular_Component_2023",
                                "MSigDB_Hallmark_2020",
                                "Reactome_2022",
                                "KEGG_2021_Human"),
                    kurtosis = 3,
                    overwrite = TRUE)
```

A list of all available databases can be found with the `enrichR::listEnrichrDbs()`, with `Enrich_ICA` taking the *libraryName* as input.


For subsequent IC annotation using the CFS Shiny interface, the preprocessed *Seurat* object must be exported as a `.rds` file using the `saveRDS` command :

``` r
saveRDS(data, file = "my_data.rds")
```

The saved `.rds` filed must then be loaded into the CFS shiny tool as described in the [Interactive Preprocessing](#Shiny) section.

</details> 

$~$

------------------------------------------------------------------------


## ICA Analysis{#Analysis}

ICA is a blind source separation algorithm able to isolate independent signal sources (i.e. cell types) in mixed detectors (i.e. Visium spots)

[ICA_in_ST.png]

Once ICA calculation is done, the CFS interactive interface helps the user interpret and analyse the data with a guided workflow to identify the biological sources associated with individual ICs.

### ICA annotation{#ICA}

The central step of the CFS workflow if the annotation of individual ICs. To assist the user tasked with this process (biologist, pathologist, bioinformatician, etc.), CFS provides visualization tools for multiple relevant properties of each IC.

In this tab, ICs are visualized one at a time. To select the IC to analyze, use the dropdown menu at the top of the page 

[ICA_Select_IC] ![](https://codimd.univ-rouen.fr/uploads/7fb066e6-a3c1-47e4-b711-39ddc296fb6b.png)

The interactive table below the selection menu allows the user to fill in annotations for the observed IC directly within this tab 

[ICA_IC_anno] ![](https://codimd.univ-rouen.fr/uploads/e7ff2f9c-908f-41dc-8f83-f5d1299adefc.png)

The IC annotation contains 4 mandatory fields: 

- IC: name of the IC (character)
- Use: Whether or not the IC should be used for downstream analysis (Boolean)
- Type: User-defined low level annotation (ex. Specific tissue region, Lymphoid), used to select multiple ICs to conduct analysis (character)
- Annotation: User-defined IC Descriptor, used to help interpretation of downstream analysis (character)

**Note** to avoid data loss in case of a crash, it is advised to fill in the annotation table in a separate `.csv` spreadsheet which can be imported in the [**ICA Table**](#ICAtable) Tab

$~$

### IC visualizations

Multiple visualization options are rendered when an IC is selected. All visualizations are interactive with the ability to modify paramaters (in the left box), zoom in, hover for more information, save snapshot as .png, and more. See the [CellsFromSpace documentation](#ADDURL) for detailed descriptions of all functions

- **Global ICA heatmap**: Display the top contributor genes for each IC. In the cogwheel menu, user can select to cluster ICs based on feature weight profile. Values represent gene weights in each IC. *Note* this visualization is constant between ICs and may be collapsed to decrease render time between ICs.

[ICA_Global_heatmap] ![](https://codimd.univ-rouen.fr/uploads/06bfdd91-f09f-4b17-bc5b-332e610ec7e3.png)

- **Plot IC weight**: Spatial projection of IC weights per spot. 

[ICA_Spatial] ![](https://codimd.univ-rouen.fr/uploads/0a3fc15c-ed5c-4ff4-b998-758f9a564335.png)

- **Plot IC related Genes weight**: Spatial projection of user-defined genes. Any gene can be selected in the **Choose gene to plot** field and they are presented in order of contribution to the selected IC. Selecting multiple genes will display each of them separately in the window.

[ICA_topGenes] ![](https://codimd.univ-rouen.fr/uploads/cd9f629f-b919-4c8e-abf2-340f2c995fdc.png)

- **IC-specific heatmap**: Display weights of selected IC's top contributory gene across all ICs. allows for rapid interpretation of gene list by expert and rapid identification of similar ICs. Underneath the heatmap, the number of total and positive contributor genes are displayed with buttons allowing the copy of the associated gene lists to clipboard.

[ICA_ICWeight] ![](https://codimd.univ-rouen.fr/uploads/4ff5746d-d93b-4a3c-8ed3-c690a87e3bc5.png)

- **Functional enrichment**: Display the result of functional enrichment analysis. Select the database to display in the drop down menu in the left box. 

[ICA_enrich] ![](https://codimd.univ-rouen.fr/uploads/b528ae1e-3075-4290-aff9-8b6b12ab9d2b.png)


- **IC-contributory genes per spot**: Heatmap of top contributor genes for the selected IC across all spots.
    
[ICA_spotWeight] ![](https://codimd.univ-rouen.fr/uploads/989eaa16-ffa0-401a-b338-a9d9d326597b.png)


$~$

### IC gene weight{#Geneweight}

Allows the manual exploration of genes of interest across ICs

[ICgeneWeight]

$~$

### ICA table{#ICAtable}

This tab displays the searchable, editable IC annotation table. An IC annotation table can be filled dynamically in the [**ICA**](#ICA) tab, or imported using the **Choose CSV File** atop this tab. The annotation table can also be downloaded using the `Download table` button at the bottom of the table
You can find an example of annotation table for the mouse brain visium sample [here](LINK TO THE TABLE), but exact IC ordering can vary slightly between runs, leading to downstream misinterpretation. Use with caution.

[ICAtable] ![](https://codimd.univ-rouen.fr/uploads/273208b5-f41c-43b7-9034-568a1bc10f4c.png)

$~$

### Visualization{#Viz}

Once IC annotation is complete, different visualization options are offered within CFS in the **Visualization** tab.

First, spots can be clustered and a dimensionality reduction (DimRed) calculated using the ICA latent space. To do so, Select one or more IC *Type* to be used for calculation in the **Choose cell type to cluster/for reduction** fields of both the **Clustering parameters** and **Dimension reduction parameters** panes. In our mouse brain example, we can calculate clusters and DimRed choosing all of the annotated IC Types with default parameters and a clustering resolution of 3.8 as shown below. 

[Vis_clust] [Vis_dimred]

With the selected clustering parameters, 37 clusters are obtained along with a UMAP projection showing distinctive cluster organization. Spatial projection of these clusters shows the ability of ICA to closely match the [mouse brain ontology](http://atlas.brain-map.org/atlas?atlas=1&plate=100960236#atlas=1&plate=100960236&resolution=11.97&x=5472.254464285715&y=4096.1328125&zoom=-3&structure=155&z=6)

[Vis_UMAP] [Vis_spatial] ![](https://codimd.univ-rouen.fr/uploads/85560f95-3230-4b93-be80-995f63d5d2f1.png) ![](https://codimd.univ-rouen.fr/uploads/9a1e752b-36fb-44a8-a0c3-c5d1f783a530.png)

However, it is important to keep in mind that most spots in Visium samples are composed of cell mixtures and that clustering and DimRed calculations are driven mostly by the cellular composition rather than phenotype of cells found within. In highly organized tissues with relatively homogeneous subregions such as the mouse brain presented here, the resulting clusters and UMAP clearly carry biological insight, but for more disorganized or heterogeneous tissues such as tumors, clustering and DimRed projections are often of lower interest. In such cases, visualization and interpretation of the IC latent space directly is more relevant and informative.

CFS offers some solutions to visualize the IC latent space. 

- First, by selecting `IC` in the **Select what to color** field of **Display parameters**, user can select an IC of interest to project onto the Dimred and Spatial embeddings. Here, IC 44 corresponds to Oligodendrocytes scattered across the tissue, but still captured by ICA. ***Note*** *That individual genes can also be visualized this way by selecting* `gene` *in the* **Select what to color** *field.*

[Vis_IC44_UMAP] [Vis_IC44_spatial]

- By selecting `Density` in the **Select method to use** field of **Display parameters**, user can visualize the signal density for an IC Type of interest. Here, the density of `Cortex` associated ICs is displayed. *Again, in a highly organized tissues like the brain, the relevance of this visualization is limited, but can be very useful in other contexts (ex. Immune infiltration of tumors, Identification of cancer cells in a sample, etc.)*

[Vis_Cortex_spatial]

- By selecting `Scatter pie` in the **Select method to use** field of **Display parameters**, user can visualize the IC composition of each spot for one or more IC Types. Here, relative IC weights associated to `Cortex` are displayed for each spot on both the UMAP and spatial projections, with spot size representing the overall Cortex signal abundance.

[Vis_scatter_UMAP] [Vis_scatter_spatial]

$~$

### Marker Table{#marker}

Upon cluster calculation in the [**Visualization** Tab](#Viz) (or any other spot categorization present in the object metadata), user can now perform differential gene expression (DGE) analysis based on identity. Calculated DGE markers for a comparison of interest can then be visualized as volcano plot. In the mouse brain dataset, one can identify marker genes between subregions or specific markers for a single region (versus all others), for example : 

- Differentially expressed genes between CA1-2 pyramidal (cluster 30) and CA3 pyramidal layer (cluster 22) 
[Marker_CA]

- Transcriptional markers of the hypothalamus (cluster 2)
[Marker_HY]

For each comparison, a detailed **Marker table** is found below the volcano plot

$~$

### Output{#outputTab}

At any point during the analysis, a `.rds` file can be exported containing all the complementary analyses. As the Shiny interface cannot automatically save progress, we recommend users manually export the object after major processing and analysis steps. Such exported processed objects can be [reloaded into CFS](#Shiny) via the **Load processed data** method in the **Load** tab. 

[Output_img] ![](https://codimd.univ-rouen.fr/uploads/448fab46-c442-4051-90a8-44dc2f368327.png)

$~$

## Object subsetting and reanalysis{#subset}

Due to the independent nature of ICs, it is possible to isolate and analyse spots containing cell types of interest solely by their composition in a subset of components, effectively isolating the signal associated to the desired population. 

For the mouse brain sample, Cortex-related signal can be isolated by manual selection of all clusters associated to the region in the [initial clustering](#Viz) (clusters 1, 4, 5, 9, 11, 12, 13, 14, 17, 18, 19, 23, 24, 25, 26, 31, 34)

[subset_allClust]

Alternatively, a new low resolution (ex. 0.5 resolution) Cortex-specific clustering may be calculated for easier subsetting. Here all clusters except 0 can then be subsetted and exported into a new `.rds` file.

[subset_CortexClust_UMAP] [subset_CortexClust_spatial]

[subset_CortexClust]

Once the `.rds` file is saved, it can then be reimported into CFS in the **Load** tab and either be reprocessed (processing, IC annotation, etc.) or more finely reanalyzed (clustering + DimRed) using only the the Cortex-associated ICs with higher resolution. By clustering the mouse brain cortex with a resolution of 5 

[subset_Ctx_clust5] [subset_Ctx_clust5_spatial]

DGE analysis can then be performed on clusters of interest in the **Marker Table** tab, for example to identify marker genes between the Lateral versus Basolateral amygdalar nuclei (clusters 20 & 10 respectively) which were not distinguished in the original analysis.

[subset_Ctx_DGE]


$~$

## Citation {#citation}

This tool is not yet published. In the meantime, please cite our [preprint](https://www.biorxiv.org/content/10.1101/2023.08.30.555558v2)
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

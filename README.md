MICCORDEA
========

[![DOI](https://zenodo.org/badge/21346104.svg)](https://zenodo.org/badge/latestdoi/21346104)

This repository includes the LaTeX files, R and Python for the AfricaRice and IRRI project, Mitigating The Impact Of Climate Change On Rice Disease Resistance In East Africa (MICCORDEA) modelling portion of the project, which examines the effects of climate change on rice disease in Tanzania.

The LaTeX files compile the manuscript as submitted.

The R files are used for data analysis and figure generation, figures are generated as .eps files and placed in "[LaTex/Figures](./LaTeX/Figures)" for inclusion in final PDF document.

The Python script is an ArcGIS script that is used to run RICEPEST in a GIS environment using inputs from the EPIRICE model. This script requires a Windows operating system and a valid installation of ArcGIS to run. Two scripts are available. The first, [RICEPEST-Original.py](./Python%20Code/RICEPEST-Original.py), is a replication of the original RICEPEST model minus some features. The second, [RICEPEST-Modified.py](./Python%20Code/RICEPEST-Modified.py), is the version used in this manuscript with a modified PS3.

The Data directory contains the necessary files to reproduce this work. Weather files are used with the EPIRICE and RICEPEST.py models. Output from EPIRICE RICEPEST-Modified.py are also available in this directory for quicker analysis.

The KML directory contains supplementary KML files, [Tanzania_BB_Change.kml](./KML/Tanzania_BB_Change.kml), reproduces [Figure 7](./LaTeX/Figures/Fig7-eps-converted-to.pdf) from the manuscript in GoogleEarth and [Tanzania_attainable_yield.kml](./KML/Tanzania_attainable_yield.kml) shows attainable yields for all time-slices as predicted by RICEPEST. Due to the need to reproject the values are not exactly replicated from Figure 7 in manuscript.

The final published version can be found in the Springer Journal, Climatic Change. <http://link.springer.com/article/10.1007/s10584-015-1580-2?wt_mc=internal.event.1.SEM.ArticleAuthorOnlineFirst>

MICORDEA
========

This repository includes the LaTeX files, R and Python for the AfricaRice and IRRI MICORDEA project for modelling effects of climate change on rice disease in Tanzania.

The LaTeX files compile the manuscript as submitted.

The R files are used for data analysis and figure generation, figures are generated as .eps files and placed in "LaTex/Figures" for inclusion in final PDF document.

The Python script is an ArcGIS script that is used to run RICEPEST in a GIS environment using inputs from the EPIRICE model. This script requires a Windows operating system and a valid installation of ArcGIS to run. Two scripts are available. The first, RICEPEST-Original.py, is a replication of the original RICEPEST model minus some features. The second, RICEPEST-Modified.py, is the version used in this manuscript with a modified PS3.

The Data directory contains the necessary files to reproduce this work. Weather files are used with the EPIRICE and RICEPEST.py models. Output from EPIRICE RICEPEST-Modified.py are also available in this directory for quicker analysis.

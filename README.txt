Open Science in Cuban Medical Journals

This repository contains the R code used for the statistical analysis of Open Science practices in Cuban medical journals.

The script provides a reproducible workflow for data cleaning, validation, descriptive and inferential analyses, Multiple Correspondence Analysis (MCA), cluster diagnostics, and the generation of publication-ready tables and figures.

R script

open_science_cuban_medical_journals.R

The script performs the complete analytical workflow for a study of 68 Cuban medical journals.

It includes:

data import and validation;
normalization and cleaning of journal information;
descriptive analysis of editorial characteristics;
95% Wilson confidence intervals for proportions;
comparison between declared and verifiable implementation of Open Science practices;
exact McNemar tests;
Fisher's exact tests;
Benjamini–Hochberg false discovery rate correction;
Poisson regression with robust variance;
prevalence ratios with 95% confidence intervals;
Multiple Correspondence Analysis (MCA);
Hierarchical Clustering on Principal Components (HCPC) as a diagnostic analysis;
comparison of cluster solutions using the Adjusted Rand Index (ARI);
generation of publication-ready tables;
generation of figures in PNG and TIFF formats;
export of cleaned analytical data and diagnostic files.
Open Science practices evaluated

The analyses include:

general Open Science policy;
acceptance of preprints;
verifiable implementation of preprints;
open data policies;
verifiable implementation of open data;
open peer-review policies;
verifiable implementation of open peer review.

Journal indexing is also evaluated for:

DOAJ;
SciELO;
Latindex Catalog 2.0;
Redalyc.
Main outputs

When executed with the corresponding input datasets, the script generates:

Table 1: Editorial characteristics of the journals
Table 2: Declaration and verifiable implementation of Open Science practices
Table 3: Robust Poisson regression models
Table 4: Open Science and indexing profiles by cluster
Table 5: Journals included in each cluster
supplementary bivariate analyses
MCA coordinates and category contributions
cluster-comparison diagnostics
cleaned analytical dataset
publication-quality figures
Excel, Word, and CSV output files
reproducibility diagnostics and sessionInfo()
Software

The analysis was conducted in R.

Main packages:

readxl
dplyr
tidyr
stringr
stringi
ggplot2
FactoMineR
sandwich
lmtest
openxlsx
flextable
officer
patchwork
scales
tibble
Reproducibility

The script includes automated checks for:

expected sample size;
duplicated journal titles;
unmatched records between input datasets;
invalid binary values;
discrepancies between analytical files;
missing cluster assignments; and
concordance between the predefined cluster classification and a recalculated HCPC solution.

The predefined final cluster assignment is used for the principal analyses. The recalculated HCPC solution is retained as a diagnostic analysis and should not be interpreted as evidence of causal or naturally discrete journal groups.

Input files

To reproduce the analysis, the script requires the corresponding study datasets containing:

the journal-level analytical database; and
the final journal cluster assignment.

Because local file paths may differ between computers, users should modify the configuration section at the beginning of the script before execution.

Citation

If you use this analytical code, please cite the associated research article.

García-Rivero AA, et al.
Open Science in Cuban Medical Journals:
Adoption, Implementation, and Editorial Profiles.
Citation to be updated after publication.
Author

Alexis Alejandro García-Rivero

For questions regarding the analytical workflow, please refer to the corresponding research publication.

# How does social information affect similarity judgments and intertemporal choice?

-   Created on 2022-06-04 by Francine W. Goh (<francinegoh@gmail.com>)
-   Finalized on 2022-06-04

This repository provides the reproducible research materials for our project that investigates the effects of social information on similarity judgments and intertemporal choice. This includes the following:

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript
-   R Markdown file for supplementary materials

## Citation

If you use any of these materials, please cite:

Goh, F. W., & Stevens, J. R. (2022). Social influences on similarity judgments and intertemporal choice. PsyArXiv.

## Summary

Three data sets were collected. Data set 1 involved 69 participants from the University of Nebraska-Lincoln, USA between Apr - Nov 2018. Data set 2 involved 86 participants from the University of Nebraska-Lincoln, USA between Feb - Oct 2019. Data set 3 involved 65 participants from the University of Nebraska-Lincoln, USA between Aug - Nov 2020. In the data file, each row contains a participant's response to questions listed in the column heading.

## License

All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:
-  Share - copy and redistribute the material in any medium or format.
-  Adapt - remix, transform, and build upon the material for any purpose, even commercially.
   Under the following terms:
-  Attribution - You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.

No additional restrictions - You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

## Files

### Data files

`goh_stevens_2022_data.csv` (behavioral and survey responses)

-  study = study number
-  subject_nr = participant number
-  amount_pair = number pair in the amount similarity judgment task
-  delay_pair = number pair in the delay similarity judgment task
-  amount_similarity = participant's similarity judgment in the amount similarity judgment task (0 = dissimilar, 1 = similar)
-  delay_similarity = participant's similarity judgment in the delay similarity judgment task (0 = dissimilar, 1 = similar)
-  itc_control_choice = participant's choice for non-social intertemporal choice questions (0 = smaller, sooner option, 1 = larger, later option)
-  itc_amount_social_info = similarity judgment shown for the amount pair in social intertemporal choice questions (0 = dissimilar, 1 = similar)
-  itc_delay_social_info = similarity judgment shown for the delay pair in social intertemporal choice questions (0 = dissimilar, 1 = similar)
-  amount_delay_social_info = social information condition shown for social intertemporal choice questions (D_D = amount dissimilar, delay dissimilar; S_S = amount similar, delay similar; D_S = amount dissimilar, delay similar (amount-focused condition); S_D = amount similar, delay dissimilar (delay-focused condition))
-  itc_social_choice = participant's choice for social intertemporal choice questions (0 = smaller, sooner option, 1 = larger, later option)
-  miss_score = participant's suggestibility score
-  median_miss_score = median suggestibility score
-  suggestibility_level = participant's suggestibility level
-  sns_score = participant's subjective numeracy score
-  sns_level = participant's subjective numeracy level
-  overall_bnt_score = participant's objective numeracy score
-  age = participant age
-  ethnicity = participant ethnicity
-  gender = participant gender
-  control_social =	non-social (control) or social intertemporal choice question followed by the number pair for the question. For social questions, this is followed by an indication of the similarity judgment shown for the number pair (d = dissimilar, s = similar). For non-social questions, this is followed by "NA" since there is no similarity judgment shown for the number pair.
-  amount_delay = similarity judgment task type (amount or delay)
-  similarity_judgment = participant's similarity judgment in the corresponding similarity judgment task (0 = dissimilar, 1 = similar)
-  task_social_info = type of social information shown for similarity judgment (amount or delay)
-  social_info = similarity judgment shown in intertemporal choice question (0 = no similarity judgment shown, 1 = similar, 2 = dissimilar)

### R code

`goh_stevens_2022_rcode.R` - code for running computations and generating figures

### R Markdown documents

`goh_stevens_2022.Rmd` - R Markdown document with R code embedded for main manuscript
`goh_stevens_2022_SM.Rmd` - R Markdown document with R code embedded for supplementary materials

### Installation

To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that a subfolder named "figures" is in the folder. Next, open `goh_stevens_2022_rcode.R` in [RStudio](https://rstudio.com) or another R interface and ensure that all packages mentioned at the top of the script are installed. Once all packages are installed, run the script in R using `source("goh_stevens_2022_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown document `goh_stevens_2022.Rmd.` Open this file in RStudio and ensure that you have packages [{knitr}](https://yihui.org/knitr/) and [{rmarkdown}](https://rmarkdown.rstudio.com/) installed. Once installed, use {knitr} to render the document (control-shift-K). Use the same process to render `goh_stevens_2022_SM.Rmd`.

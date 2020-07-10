## The 4C Initiative (Clinical Care for Cardiovascular disease in the COVID-19 pandemic) - monitoring the indirect impact of the coronavirus pandemic on services for cardiovascular diseases in the UK

## Introduction
The [CVD-COVID-UK consortium](https://www.hdruk.ac.uk/news/improving-the-nations-cardio-vascular-health-the-bhf-data-science-centre/) conducted this study to monitor the indirect impact of the coronavirus pandemic on services for cardiovascular diseases in the UK. This repository contains the scripts that run the web application at [hospitalactivity.com](http://hospitalactivity.com). In addition, analysis script for our manuscript is also available here. 

## Running the web application
If you would like to run the application locally, you need the following files and directories:
- app.R
- processing.R
- data
- submit
- www

All these should be stored within the same directory for the application to run succesfully. In addition, a file containing your own data in an excel spreadsheet (.xlsx) should be included in the same directory. The data template spreadsheet can be found within the **data** directory (filename = "4C data request.xlsx").

## Inspecting the analysis script
Accompanying our manuscript submission, our analysis script is available here:
- manuscript_4c.R

Because of data governance, we do not publicly provide hospital activity data submitted from participating hospitals. 


## Queries
Please feel free to contact [Michael Poon](https://twitter.com/MchaelPoon) about this web application - [mpoon@ed.ac.uk](mailto:mpoon@ed.ac.uk)
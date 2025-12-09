# Fleming Fund AMR surveillance system monitoring and analysis - Resource and code repository

## About
This repository contains the resources that allow the recreation of the AMR surveillance site monitoring and analysis systems used by the Fleming Fund and Mott Macdonald. 

By following the instructions in this file, you will be able to recreate the DHIS2 forms used to collect AMR site function data in your own DHIS2 system. 
This package can then also be used to produce analyses and graphs of collected data.

An example dataset using modified real-world data has been included in the '/DHIS2 exported files' folder. By using these as analysis input files, you can practise with semi-real data.

For more information on how the outputs of these analyses can be used, please review the corresponding [publication](PUBLICATION LINK HERE).

For any questions, please contact Dr Jacob Wildfire.

- Email: Jacob.Wildfire@Mottmac.com
- LinkedIn: [Jacob Wildfire](https://www.linkedin.com/in/jacob-wildfire)
- X: [@JacobWildfire](https://x.com/jacobwildfire)

## Requirements
For this tutorial, a basic knowledge of git:
- Access a tutorial [here](https://happygitwithr.com/)

To fully utilise this package requires:
- An existing healthcare or One Health DHIS2 system containing microbiology laboratories.
- A basic familiarity with R.
	- Ensure [R](https://cran.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/) are downloaded.

## Instructions 1-2: Preparing the repository **(GitHub/Windows)**
### 1. Downloading the repository
The most simple way to download and initialise the repository is to download it directly from GitHub. To do this,

1. Click '<> Code' on the 'FF_analysis' GitHub repository page.
2. Click 'Download ZIP', and download the .zip file locally to your machine.
	- This will initially download to your 'Downloads' folder. **Do not move it yet!**
3. Unzip `FF_analysis-main.zip` file.
	- Now you may choose where you would like the files to go.
	- Choose the exact folder you would like the files to go into. If this is the 'Downloads' folder, ensure you are extracting it into '\Downloads' and not '\Downloads\FF_analysis-main', as the latter will create two layers of folders with the same name. This is confusing and unnecessary.
	- I would recommend unzipping it into your 'Documents' folder. This should allow the R code to automatically run when it comes time to use it, provided you do not have custom RStudio settings.

You should now have access to the repository resources within a folder called 'FF_analysis-main/'.

### 2. Preparing your site masterlist
**If you would like to first practise with the example datasets, _start at step 5_**

Identify the microbiology laboratories from which you would like to capture and monitor AMR surveillance function data.

In the 'FF_analysis-main/Resource files/' folder, access the `Site masterlist template.xlsx` file. Within this excel are example sites. Replace these with your selected sites.
Instructions on how to fill the attributes of each site can be found in the 'READ ME' tab, and attribute headings.

## Instructions 3-4: Uploading forms and downloading data **(DHIS2)**
### 3. Uploading forms to DHIS2
The DHIS2 metadata files required to upload the data collection forms to DHIS2 are stored in 'FF_analysis-main/Resource files/DHIS2 form upload files/'.
To upload these to your system,

1. Navigate to the DHIS2 'Import/Export' app.
2. Select 'Metadata import'
3. Choose one of the following .json files:
	- `metadata HH site form.json`
	- `metadata AH site form.json`
	- `metadata HH samples form.json`
	- `metadata AH samples form.json`
4. Click 'Start dry run' and ensure that no errors appear.
	- The default options should be sufficient.
	- As such, ensure that the format is 'JSON', and the identifier is 'UID'.
5. If there are no errors, click 'Start import'.
6. Repeat this process for the remaining .json files.
7. Navigate to the 'Data set' section of the 'Maintenance' app, share human health forms with human health sites, and animal health forms with animal health+ sites.
8. Modify the 'Sharing settings' for relevant groups, changing 'Data' to 'Capture and view'.

These forms can now be used to capture data.

### 4. Downloading data
When you are ready to download your data from DHIS2,

1. Open your DHIS2 system.
2. Select the 'Import/Export' app.
3. Select 'Data export' from the left hand side menu.
4. Select the sites you would like to analyse.
	- Provided that 'Include descendants of selected organisation units' is selected, you can choose the hierarchical parent of your sites of interest. All sites below it will be included (e.g., 'Region X' will include all sites with its branch).
5. Select the exact form you would like to download the data from.
	- Select only one type of form at a time.
6. Set the date range from the first reporting date to the most recent reporting date.
	- The start and end date should map to the start of the first report date (e.g. for 22Q1, 01/01/2022) and end of the last report date (e.g. 25Q4 31/12/2025).
	- **Note**: the analysis graphs will allow you to choose the date range, so choosing a smaller data range is optional here.
7. Under data export format, select the 'CSV' option.
8. Under the compression mode, select the 'Uncompressed' option.
9. In the 'Advanced options', change:
	- The 'Data element ID scheme' to 'Name'.
	- The 'Organisation unit ID scheme' to 'Name'.
10. Click 'Export data'.
11. Modify each file with a recognisable name so that the file content is easily identified.
	- Examples of filenames can be found within the '~/FF_analysis-main/DHIS2 exported files/' folder.
12. Move to a dedicated folder within the repository folders.
	- Although it contains the example data, the '~/FF_analysis-main/DHIS2 exported files/' folder can also be used to store your data.
13. Repeat with the remaining form datasets.

When you are finished, if you are looking to download/analyse all forms, you should have separate HH and AH .csv datasets for the site and sample processing forms.

## Instructions 5 - Performing analyses **(RStudio)**
### 5. Analysing data
If you would like to practise data analysis, or have your own downloaded data to analyse, open RStudio. 

Within the 'FF_analysis-main/Code files/' folder, there are 7 files. These do the following:
- `0. Data Mastersheets Generation Code.R`
	- Produces human-readable excel versions of the DHIS2 site forms
- `1a. HH LSHTM Roadmap Code.R`
	- Analyses the functional status of each HH site, at each reporting period, according to the LSHTM Roadmap.
	- Stores this data in `1. HH LSHTM Roadmap status.xlsx`.
- `1b. AH LSHTM Roadmap Code.R`
	- Analyses the functional status of each AH site, at each reporting period, according to the Animal Health Roadmap.
	- Stores this data in `1. AH LSHTM Roadmap status.xlsx`.
- `2a. HH Roadmap Analysis Code.R`
	- Produces modifiable graphs showing the HH site functional status of the surveillance system.
	- Stores underlying data in `2. HH LSHTM Roadmap proportion masterlist.xlsx`.
- `2b. AH Roadmap Analysis Code.R`
	- Produces modifiable graphs showing the AH site functional status of the surveillance system.
	- Stores underlying data in `2. AH LSHTM Roadmap proportion masterlist.xlsx`.
- `3a. HH Sample Processing Code.R`
	- Produces modifiable graphs showing the HH samples processed over time.
	- Stores underlying data in `3. HH sample processing.xlsx`.
	- Produces `3a. Blood culture positivity percentage masterlist.xlsx`, an excel masterlist of the percentage of positive HH blood cultures over time.
- `3b. AH Sample Processing Code.R`
	- Produces modifiable graphs showing AH samples processed over time.
	- Stores underlying data in `3. AH sample processing.xlsx`.

To perform these analyses using the example data,

1. In each file, change the value of 'wd' to be where you have unzipped the 'FF_analysis-main/' folder.
	- If you unzipped it to your 'Documents' folder, you can keep it as '~/FF_analysis-main/'
	- Alternatively, set your working directory manually in RStudio. You can do this using the following settings:
		- Sessions → Set Working Directory → @Choose Directory
2. Run each R file in numerical order.
	- Whilst not totally necessary, 2a and 2b are dependent on the outputs of 1a and 1b, respectively.

Files will deposit excel their outputs into the 'FF_analysis-main/Output files/' folder.
Files 2a to 3b will also open a `shiny` app, allowing you to modify and save your plots as you see fit.

When you are ready to analyse your own downloaded data, in each code file under the 'Loading dataframes' section of code, simply change the read.csv argument to be your own data.

**You are now ready to analyse your own data.** For more information as to how, please see the corresponding [publication](PUBLICATION LINK HERE).

For more information as to what each level of function corresponds to (i.e. 'Core', 'Extended' and 'Advanced'), please see Table 2 of this additional [publication](https://wellcomeopenresearch.org/articles/2-92/v1).

**Note**: For code files 3a and 3b, changing the data range with the 'Cumulative' option selected will cause samples to be cumulatively counted from the beginning of the **new** date range.

## License

This work is distributed under the MIT license (see LICENSE file).

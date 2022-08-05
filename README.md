# microplastic-methods-metaanalysis
Data and code repository for the meta-analysis paper titled *Sampling method affects measured microplastic concentration: a meta-analysis*<br>

This README.md file was created on 2021-04-21 by Lisa Watkins.<br>

## General Information

**Title of Dataset**<br>

"microplastic-methods-metaanalysis"<br>

**Brief Dataset Description**

This repository was created to provide access to the data collected in the field and across the literature, as well as the analysis code, used for the paper by Watkins et al. titled, *Sampling method affects measured microplastic concentration: a meta-analysis* published in [Environmental Science & Technology](https://doi.org/10.1021/acs.est.1c03019).<br>
These Literature review data may contain errors. I encourage you to use the tables in `corestudies` and `pairedstudies` as references, and refer to the original papers, linked for each row, for reputable, error-free data.

**Dataset Contact Information**<br>
Name: Lisa Watkins<br>
Institution: Cornell University, Department of Biological & Environmental Engineering<br>
Address: 111 Wing Drive, Ithaca, New York, 14853, USA<br>
Email: ltw35 at cornell dot edu<br>
Role: author, maintainer<br>

**Date of Data Collection**<br>

Field data collection took place in Fall of 2016 and Fall of 2017. <br>
Literature review was conducted between June & October 2020, with a few additional papers added during winter 2020. <br>

**Geographic location of data collection**<br>

Field data was collected in perennial streams near Ithaca, New York.<br>
Literature review data were collected for studies of surface water microplastics worldwide. These data were collected via [Google Scholar](scholar.google.com)<br>

**Information about funding sources that supported the collection of the data**<br>

Lisa Watkins was supported by [National Science Foundation Graduate Research Fellowship Program](https://www.nsfgrfp.org/) grant No. 2017228528.<br>

## Sharing & Access Information ##

**Licenses/restrictions placed on the data**<br>

We ask that you please cite this work if you choose to use or share our data. (See [CITATION.md](https://github.com/ltw35/microplastic-methods-metaanalysis/blob/master/CITATION.md) for recommended citation). You may use/distribute freely. (See [Creative Commons - CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/) for a full explanation). <br>

**Links to publications that cite or use the data**<br>

As of 2021-04-21 there are no other publications that cite or use these data.<br>

**Links to other publicly accessible locations of the data**<br>

These data and the associated code are available at https://github.com/ltw35/microplastic-methods-metaanalysis. The associated publication is available in [Environmental Science and Technology](https://doi.org/10.1021/acs.est.1c03019). Please get in touch if you have trouble accessing the full-text. I am happy to share a copy directly.<br>

**Links/relationships to ancillary data**<br>

No ancillary data are associated with this paper.

**Data derived from another source**<br>

Field data was collected specifically for this study. Average values for it are included in the file `pairedstudies`. Site-specific data, including stream velocity, is included in `fieldsamples`. <br>
Literature review data were collected through Google Scholar web searches. The file `corestudies` contains doi references to each publication included in the analysis, as does `pairedstudies` for the few studies not otherwise included in the overall literature analysis.<br>


**Additional related data collected that was not included in the current data package**<br>

Some more specific data related to the field samples (including particle type, counts from individual counters, and particle color) are available by request.<br>

**Are there multiple versions of the dataset?**<br>

There are no other versions of the data associated with this paper.

**Recommended citation for the data**<br>

The recommended citation for these data and the code is available in [CITATION.md](https://github.com/ltw35/microplastic-methods-metaanalysis/blob/master/CITATION.md).

**Paper Availability**<br>

The paper associated with these data and the code is available from [Environmental Science and Technology](https://doi.org/10.1021/acs.est.1c03019). Please get in touch if you have trouble accessing the full-text. I am happy to share a copy directly.<br>

## Methodological Information ##

**Description of methods used for collection/generation of data:**<br>

1. Field methods: At a given time and location within the several streams in the Ithaca, New York area, a 1-4L grab sample and a 10 minute net sample were collected in sequence. Grab samples were subsequently rinsed through the same mesh (0.335mm) as made up the net (larger particles retained). Wet peroxide oxidataion, density separation, and visual identification were used to extract and identify particles within the sample. Multiple counters were used to validate counts for each sample. FTIR was used on a subset of particles to confirm the accuracy of visual counts. Blank samples composed of deionized water were run through all lab equipement. Additional blanks were composed of filter paper left out in the laboratory space for 24 hour periods. The average cumulative blank value was subtracted from all sample counts before concentrations were included in analysis.<br>
2. Literature review methods: Google Scholar searches were performed for the following words: “microplastic” + “surface water”, along with (individually) “net”, “pump”, “bulk”, “discrete” and “grab”. All resulting papers that sampled within the top 1m of a waterbody and reported volume sampled or a means of calculating it were included in the subsequent analysis. Additionally three additional studies that did not meet all above criteria, but compared pairs of differing-method samples and contained sufficient detail for a paired-sample analysis were identified and included in `pairedstudies`.<br>

**Methods for processing the data:**<br>

*What constitutes a row in `corestudies`:* For studies that sampled multiple waterbodies or used multiple methods, results were included for each unique combination of method and waterbody-type. For example, if multiple rivers in a region were sampled with the same method, those rivers' results were averaged, while the results of pumping and net methods on a single river were considered separate entries.<br>
*How values (e.g. average concentration or volume) were determined for `corestudies`:* Values in a study's main paper were taken over those included in its supplementary materials. If values were given for individual samples, they were averaged for `corestudies`. If multiple averages were given in the paper, but a given row was a combination of sample groups, the averages from the paper were averaged for  `corestudies`.<br>
*How complete is the dataset?* Only data that was freely available in the paper, supplementary materials, or a linked dataset were included (no authors were contacted for additional data). Additionally, if methods or results were not understandable, I've omitted them. If methods or results were slightly unclear, I've made a best guess. Because of the potential for errors in transcription, I encourage these datasets to be used as reference, and original papers (linked for each row in a given table) to be accessed for data validation to avoid any error propagation in future works.<br>



**Instrument- or software-specific information needed to interpret the data:**<br>

Data processing and analysis was done in R (version 1.3.1093) uing RStudio Desktop. All R packages used are listed at the top of the code file that utilizes them.

```
# To best use this data, run code as an *RStudio Project*. To do so, download this repository into a single file, including .R files and the .Rproj file. Open the .Rproj file, which will open RStudio and keep all related code and data connected.
```

**People involved with sample collection, processing, analysis and/or submission:**<br>

Lisa Watkins identified studies to include based on the forementioned criteria, extracted relevant data from the manuscripts, supplementary materials and any linked datasets, wrote the code to clean and analyze the data, curated the datasets and code for readability and sharing, and created all associated metadata.<br>
Field samples were collected, processed, and counted by Lisa Watkins, Susan McGrattan, Anna-Katharina von Krauland, Alexis Weaver, Gray Ryan, Emma Mosier, Whitney Denison, Elizabeth Dean, Leah Balkin, Jack Novak, and Xiaoman (Sharon) Zhang.

## Data & File Overview

### 1. main directory ###

**File List**

The main directory contains the following documentation files:

* `CITATION.md` - contains recommended citation for the data and code in this repository.
* `README.md` - (The one your reading now) contains metadata on the datasets, code, and overall repository, including methods used for dataset curation and how to cite this work.

The main directory contains the following R-related files:

* `microplastic-methods-metaanalysis.Rproj` - The RStudio project file that links code and data files.
* `what_affects_concentration.R` - This code cleans and analyzes `corestudies` data file.
* `paired_studies.R` - This code cleans and analyzes `pairedstudies` data file.

The main directory contains the following sub-directory:

* `data` - This directory contains the raw data used for the analysis performed by the code, as summarized in the associated manuscript.

**Relationship Between Files**<br>

The two code files are run independely, in any order. `what_affects_concentration.R` inputs `corestudies` data file. `paired_studies.R` inputs `pairedstudies` datafile.<br>
The other data file `data_fieldsamples` contains the paired samples collected by Lisa Watkins specifically for this study. The average values show up in both `corestudies` and `pairedstudies` as "This study" or "Watkins et al." `data_fieldsamples` does not get utilized in any of the R scripts.

### 2. data directory ###

**File List & Relationship Between Files**

The table in `corestudies` contains an entry for each study's unique combination of waterbody and method. Multiple samples are averaged in a given study's row. <br>
The table in `pairedstudies` contains an entry for each sample in a given study, with one row for one method and a second row with the same sample name for a second method. Some studies included in `pairedstudies` are also included (in an averaged form) in `corestudies`.

## Data-Specific Information For: `*corestudies`

**Number of columns/variables**

Number of columns: 32

**Number of rows**

Number of rows: 

**Variable list**<br>
* Omit: 0 if entry complete enough and used in analysis, x or ? if entry is incomplete or does not meet literature inclusion criteria for analysis
* Author_date: First author & date of study
* doi: DOI link to the row's study
* earliestyear_sampled: For studies that provide dates for when samples are collected, this column includes the year when first samples of the study were collected. For studies where sampling dates could not be found, this lack of data is designated by "nd".
* latestyear_sampled: For studies that provide dates for when samples are collected, this column includes the year when last samples were collected. For studies where sampling dates could not be found, this column includes date of paper submission.
* method: the method of microplastic collection (options: grab, net, pump). Samples that were collected by scooping water and then pouring it through a handnet are marked as grab samples here.
* method_specific: defining the exact method used, in an attempt to preserve some of the more specific language from the paper describing exact sampling apparatus used. (options for grab method rows: bucket+handnet, bulk; options for net method rows: conical, manta, rectangular; options for pump method rows: centrifugal pump, flow sampler, in situ pump, jet pump, large flow sampler, pump, PVC pump, submersible pump, Teflon pump).
* volume_measure: the method or apparatus used to measure or calculate volume sampled, according to the study (options: vessel *for grab method samples only*, GPS distance *for net method samples only*, flowmeter, mechanical flowmeter, digital flowmeter, water sampler *unknown method preserved from study language*, current meter, river velocity *for net method samples only*, time *for pump method samples only*, time x speed *for towed net method samples only*, not mentioned).
* river_ocean: the kind of waterbody sampled (options: marine [ocean/sea], wetland, lake, river, estuary, lagoon, reservoir, pond).
* lowestsize_mm: The smallest filtration size used to separate the sample contents from discarded particles, in units of mm. Depending on methods, this may be the size of filter paper openings, a sieve size, or a net mesh.
* lowest_mesh: Description to inform lowestsize_mm column. The type of filtration used to enforce the lowest size. (options: mesh [plastic], sieve [metal], or filter [paper or plastic]).
* vol_L: Sample volume, in units of L. For studies with inconsistent volumes across multiple samples, this is the averaged volume.
* vol_source: How vol_L value was determined for this table. (options: given [found in the text]; averaged [several volumes in the text were averaged to include in table]; calculated, distance [net area multiplied by tow distance]; calculated, speed [net area multiplied times tow time multiplied times average tow speed]).
* uppersize_mm: Microplastics are typically defined as plastics < 5mm. This column notes the upper size limit included in the concentrations and counts of the given study. For studies where largest included particle size was not found, this lack of data is designated by "nd".
* Cavg.L: Average reported concentration, in units of particles per L.
* Csd.L: Standard deviation of reported concentration, in units of particles per L. For studies where a standard deviation on average concentration could not be found or easily calculated, "nd" is used.
* num_samples: The number of samples collected. This value would indicate the number of values included in average concentration (Cavg.L) and average volume (vol_L). For some studies, the number of samples for different methods or locations is difficult to discern, but we have done our best to enter the best guess. "nd" is used for studies where number of samples was not found.
* total_particles_collected: The number of particles counted overall in the analyzed samples. For some studies, the number of particles for different methods or locations is difficult to discern, but we have done our best to enter the best guess. "nd" is used for studies where number of samples was not found.
* separation: Lists chemical or physical separation technique noted in paper's methods
* primary_id: type of microscope or method used for first past particle selection or identification
* secondary_id: The second (or most advanced) method or apparatus used to confirm a particle is plastic and should be included in the sample count, in addition to or in place of a visual inspection.
* highest_id: most advanced identification method used when choosing to include suspected particle in final counts
* no_particles_secondary: if noted in manuscript, the number of particles included in the secondary analysis method
* top_shape: The dominant particle type detected in the sample (options: fiber, fragment, bead [includes pellet-type], film, foam). "nd" indicates mention of most prominant particle type was not found in the paper. 
* excluded_particles: Notes whether a certain particle type was omited from all counts in the study.
* validation_rate: if noted in manuscript, the percent of particles assessed by secondary identification method that were confirmed to be plastic
* topsize_bottom_mm: The lower bound of the most common size range of particles, in units of mm.
* topsize_upper_mm: The upper bound of the most common size range of particles, in units of mm.
* top_polymer: The most common polymer type identified.
* second_polymer: The second most common polymer type identified.
* top_color: The most common particle color identified.
* second_color: The second most common particle color identified.
* location: General area where sample was collected.
* lat: The latitude of the general area where samples were collected. This should not be used for replicating sampling. It is merely intended to be used, on a global scale, to show the general region where samples were collected. 
* long: The longitude of the general area where samples were collected. This should not be used for replicating sampling. It is merely intended to be used, on a global scale, to show the general region where samples were collected. 
* waterblanks: The number of procedural blanks run for a given sample. 0 indicates no mention of blanks were found in the paper.
* airblanks: The number of blanks meant to measure potential contamination from lab air by leaving samples exposed. 0 indicates no mention of blanks were found in the paper.
* blanks_subtracted: An indication in the methods or results of whether reported concentrations are corrected by subtracting any blank sample measurements from the sample counts. (options: yes [concentrations corrected by blanks], no [concentrations not corrected by blanks], unknown [no mention of a correction], some [imprecise explanation of whether correction has been or should be made]).
* comment: Notes from Lisa Watkins about the methods, entries selected during data entry, or questions regarding the manuscript

## Data-Specific Information For: `*data_fieldsamples`

**Number of columns/variables**

Number of columns: 12

**Number of rows**

Number of rows: 8

**Variable list**<br>
* samplegroup: identification label for pair of samples collected at same location on same day. The label is 2-initial location code for the river, an underscore, and the month and day of collection.
* lat: latitude of sampling location
* long: longitude of sampling location
* sample_date: date of sampling
* grab_volumesampled_m3: water volume collected for grab sample, in cubic meters
* net_volumesampled_m3: water volume that passed through net sample, in cubic meters
* grab_conc_total.m3_minuscontam: Concentration of grab sample, after subtracting the average number of contaminating particles per sample (as measured in air and procedural blanks), in particles per cubic meter
* net_conc_total.m3_minuscontam: Concentration of net sample, after subtracting the average number of contaminating particles per sample (as measured in air and procedural blanks), in particles per cubic meter
* velocity_m.s: River velocity at mouth of net, in meters per second
* meshsize_mm: Net mesh size or filtration size used for grab samples, in millimeters
* mostcommon_particletype_grab: Most common type of particle found in grab sample. Options "fragment", "fiber", "film", "bead/pellet"
* mostcommon_particletype_net: Most common type of particle found in net sample. Options "fragment", "fiber", "film", "bead/pellet"

## Data-Specific Information For: `*pairedstudies`

**Number of columns/variables**

Number of columns: 15

**Number of rows**

Number of rows: 450

**Variable list**<br>
* omit: 0 if entry is complete and meets literature review criteria (set by Lisa Watkins). x or ? if it should be filtered out and not included in analysis.
* study: Author and date of study as published
* doi: DOI of associated paper
* location: type of waterbody sampled
* study_type: type of methods being paired in study. Options are "mesh compare" (two samples from same time/place, filtered through different sized mesh), "grab-grab" (two samples from same time/place, both sampled by grab to compare inter-sample variability), "grab-net" (one grab sample, one net sample taken from same time/place), "net-pump" (one net sample, one pumped sample, collected from same time/place).
* source: Table, page, or section number from DOI where Lisa Watkins transcribed the data from.
* sample_num: Sample identifier as listed in DOI study
* method: The sample collection method used to collect the sample described in that row
* mesh_mm: smallest mesh or filter used to separate the sample from water.
* vol_L: Sample volume collected, in liters
* Cavg.L: Plastic concentration reported from that sample, in particles per liter
* contam_numpersample_avg: Average number of particles of contamination found in all blank samples run in association with the given study or sample (whichever is more specific to the given sample), if reported.
* comment: Notes written by Lisa Watkins when transcribing this data, about data sources, methods, or other details. Not comprehensive or standardized.
* conc_L_samemeshsize: Plastic concentration, in particles per liter, of the paired sample, when accounting only for the same particle size range as available for the other sample in the pair, when available.


**Missing data codes**<br>

When no information could be found in the publication, supplementary materials, or linked data (though possibly just overlooked!), or was otherwise excluded because the entry was clearly not to end up in final analysis, "no data" is designated with a blank cell.

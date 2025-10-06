## Assessing Source Contributions to Air Quality and Noise in Unconventional Oil Shale Plays

### Data

The three subfolders of data are organized as follows:

- 1- minute-data: These data (.csv) include air pollutants and meteorology that were measured at 1-minute temporal resolution: CH4, CO2, CO, H2S, SO2, NO, NO2, NOx, O3, wind direction (wdr_deg), wind speed (wsp_ms), temperature (temp_f), relative humidity (relh_percent), incoming solar radiation (solr), atmospheric pressure (pressure_altcorr), precipitation (rain), water vapor (water_vapor_mr). Measurement units are in the second row of the datasets.

- VOC-sampling-window: These datasets include averaged data corresponding to the 10-minute VOC sampling period centered at the 5th minute of every hour. For all minute-level data, 4 timestamps before, the center timestamp, and the four timestamps after were averaged. For radioactivity measurements (rd) the nearest 10 minute sample was extracted. For this averaging, there was no minimum amount of data points required to compute; one data point was sufficient. Measurement units are in the second row of the datasets.

- merged-analytic: This dataset includes the merged VOC-sampling-window data, which was used for the non-negative matrix factorization source apportionment analysis. Merging code is also provided in this folder.



### Acknowledgement

The data were collected and research was conducted under contract to the Health Effects Institute (HEI), an organization jointly funded by the United States Environmental Protection Agency (EPA) (Contract No. 68HERC19D0010) and certain oil and natural gas companies. Although the research was produced with partial funding by EPA and industry, they have not been subject to their review, and therefore the research does not necessarily reflect the views of the Agency or the oil and natural gas industry, and no official endorsement by the Agency or the industry should be inferred.

Sacha Ruzzante
2023-09-01
sachawruzzante@gmail.com

https://pacificclimate.org/data/station-hydrologic-model-output


STATION HYDROLOGIC MODEL OUTPUT
The Station Hydrologic Model Output page provides access to simulated streamflow data for locations throughout British Columbia, Canada. The streamflow data were simulated using runoff and baseflow generated with an upgraded version of the Variable Infiltration Capacity (VIC-GL) model that is coupled to a glacier model (Schnorbus, in prep) and routed with RVIC (Lohmann et al., 1998, 1996; Hamman et al., 2016).


ABOUT THIS DATASET
The simulated data includes daily streamflow time series for 190 locations corresponding to a combination of Water Survey of Canada and US Geological Survey hydrometric gauges and dam sites in the Peace, Fraser and Columbia watersheds. For each site, streamflow for the historical run, as driven by PCIC's gridded meteorological data for northwest North America (PNWNAmet), is available from 1945 to 2012. Additionally, streamflow for 12 scenarios, as driven by six GCMs from the Coupled Model Intercomparison Project Phase 5 (CMIP5; Taylor et al., 2011) run under two Representative Concentration Pathways (RCPs), 4.5 and 8.5, statistically downscaled with the Bias Correction/Constructed. Analogues with Quantile mapping reordering (BCCAQ) method  using PNWNAmet as the target, is available from 1945 to 2099. See Gridded Hydrologic Model Output for more details on Global Climate Model (GCM) selection and downscaling. The simulated data represents naturalized flow conditions (i.e. with effects of upstream regulation removed) for those sites affected by storage regulation.

Simulated data includes routed Daily Streamflow (m3s-1).

USAGE NOTES
One CSV file that contains streamflow for the PNWNAmet historical run and 12 CMIP5 scenarios is provided for each station. The first column is the date, followed by PNWNAmet-driven streamflow and then a column each for the 12 CMIP5 scenarios listed by GCM/RCP/run (i.e. ACCESS1-0_rcp85_r1i1p1). Streamflow is provided in cubic metres per second (m3 s-1).

To test the ability of the VIC-GL RVIC model to replicate streamflow in your watershed of interest, compare PNWNAmet values to observed streamflow. To look at the impact of climate change on streamflow, it is suggested that one compare the future to the past within the same GCM/RCP/run, such as mean daily streamflow from 2041-2070 versus 1971-2000 for ACCESS1-0_rcp85_r1i1p1. 

The user interface features an interactive map of northwestern North America that allows users to zoom, pan and select their region of interest using a rectangular selection tool. Alternatively, stations can be selected by Water Survey of Canada station name or ID, US Geological Survey station name or ID, dam or project site.

See the User Docs for more details. See below for notes on Data Citation, Terms of Use, No Warranty and References.

DATA CITATION
When referring to the Station Hydrologic Model Output data retrieved from the website or found otherwise, the source must be clearly stated:
Pacific Climate Impacts Consortium, University of Victoria, (February 2020). VIC-GL BCCAQ CMIP5 RVIC: Station Hydrologic Model Output. Downloaded from <Permalink> on <Date>.

TERMS OF USE
The data is subject to PCIC's terms of use.

NO WARRANTY
This data product is provided by the Pacific Climate Impacts Consortium with an open license on an “AS IS” basis without any warranty or representation, express or implied, as to its accuracy or completeness. Any reliance you place upon the information contained here is your sole responsibility and strictly at your own risk. In no event will the Pacific Climate Impacts Consortium be liable for any loss or damage whatsoever, including without limitation, indirect or consequential loss or damage, arising from reliance upon the data or derived information.

REFERENCES

Hamman, J., Nijssen, B., Brunke, M., Cassano, J., Craig, A., DuVivier, A., Hughes, M., Lettenmaier, D.P., Maslowski, W., Osinski, R., Roberts, A., Zeng, X., 2016. Land Surface Climate in the Regional Arctic System Model. J. Clim. 29, 6543–6562. https://doi.org/10.1175/JCLI-D-15-0415.1(link is external).

Lohmann, D., Nolte-Holube, R., Raschke, E., 1996. A large-scale horizontal routing model to be coupled to land surface parametrization schemes. Tellus A 48, 708–721. https://doi.org/10.3402/tellusa.v48i5.12200(link is external).

Lohmann, D., Raschke, E., Nijssen, B., Lettenmaier, D.P., 1998. Regional scale hydrology: I. Formulation of the VIC-2L model coupled to a routing model. Hydrol. Sci. J. 43, 131–141. https://doi.org/10.1080/02626669809492107(link is external).

Schnorbus, M.A., in prep. Modelling glacier surface mass and energy balance with the VIC model. Dev.
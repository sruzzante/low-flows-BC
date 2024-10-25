# low-flows-BC
Analysis of low flows and streamflow drought in British Columbia, Canada. Historical trends &amp; reconstruction 


There are four folders:
1.code: All R codes used for the analysis in Ruzzante and Gleeson (submitted 2024):

	1.prepareCatchmentPolygons.R: merge and save the catchment polygons
	2.prepareData.R: extract and save the hydrometric (streamgauge) data
	3.xxx: These codes prepare the covariate data (temperature, precipitation, snow, water use, and forestry)
	4.Regime_Map_Fig1.R: create figure 1 in the paper
	5.makeTrendMapBC_Fig2.R: conduct the trend analysis and create figure 2 in the paper
	6.1ClimateSensitivityAnalysis.R: Conduct the sensitivity analysis (Section 3.2 of paper, and appendix D)
	6.2ClimateSensitivityAnalyis.stationarity.R: Conduct the stationarity analyis, section 3.2.2)
	7.1regressionStep1_optimization.R: Conduct 10x 5-fold cross-validation procedure using all-subsets regression to pick the best regression model for each catchment.
	7.2regressionStep2_evaluation.R: Repeat the cross-validation with new random train-test splits to evaluate the best model
	7.3regression-step3_fitting.R: Fit the best model structure to the full dataset to generate the final model.
	7.4regressionPerformance.R: create Table 2, summarizing the performance data
	7.5regressionPerformance_vs_PCIC.R: compare the performance against PCIC's VIC-GL models
	7.6regression_autocorrelation.R: Evaluation residual autocorrelation of the models
	7.7regressionModelFigure_Fig4.R: create Figure 4 in the paper
	7.8regressionHindcasting.R: simulate low flows from 1901-2022, and create Figures 5 and 6.
	./forestry.Analysis/ : codes used to conduct analysis of forestry effects on low flows (Appendix E)
	
2.data: Data used and produced in this study. Many files are not uploaded due to space limitations - see readmes. However, all files for running codes 4.RegimeMap_Fig4.R and higher are available.
	1.original: Data in its original format, as downloaded from the  internet. See individual readmes for source details. All data are open-source except the BC Assessment folio data
	2.working: Working data files that are used in the Analysis
	
3.Figures: folder to store figures from Analysis


4.output: folder to store some output (results of analyis, tables, etc)


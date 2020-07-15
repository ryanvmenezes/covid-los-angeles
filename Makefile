update:
	Rscript 01_get-and-create-daily-data.R
	Rscript 02_plot.R
	
rebuildgis:
	Rscript 00_make-mapla-csa-crosswalk.R
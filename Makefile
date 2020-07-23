update:
	Rscript 01_get-and-create-daily-data.R
	Rscript 02_plot.R
	Rscript poverty.R
	Rscript crowding.R

gis:
	Rscript 00_make-mapla-csa-crosswalk.R
	
census:
	Rscript gis-census/csa-to-tract-xwalk.R
	Rscript gis-census/csa-census-calcs.R
	
demographics:
	Rscript crowding.R
	Rscript poverty.R

maps:
	Rscript maps.R
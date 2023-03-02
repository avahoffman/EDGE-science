## Data

```
├── EDGE_AllSites_long.csv
├── EDGE_North_South_PPT_ANPP.csv
├── EDGE_biomass_DROPPED.csv
├── EDGE_biomass_outliers_removed.csv
├── README.md
├── Taxa_Info.csv
├── genetic
│	├── genind_all.R
│	└── gracilis_traits.csv
├── huxman_2004.csv
├── precip.csv
└── shapefiles
    ├── chihuahuan_desert.cpg
    ├── chihuahuan_desert.dbf
    ├── chihuahuan_desert.prj
    ├── chihuahuan_desert.qpj
    ├── chihuahuan_desert.shp
    ├── chihuahuan_desert.shx
    ├── nm_mtns.cpg
    ├── nm_mtns.dbf
    ├── nm_mtns.prj
    ├── nm_mtns.qpj
    ├── nm_mtns.shp
    ├── nm_mtns.shx
    ├── northern_steppe.cpg
    ├── northern_steppe.dbf
    ├── northern_steppe.prj
    ├── northern_steppe.qpj
    ├── northern_steppe.shp
    ├── northern_steppe.shx
    ├── shortgrass_prairie.cpg
    ├── shortgrass_prairie.dbf
    ├── shortgrass_prairie.prj
    ├── shortgrass_prairie.qpj
    ├── shortgrass_prairie.shp
    └── shortgrass_prairie.shx
```

`EDGE_AllSites_long.csv` - Raw Data

`EDGE_North_South_PPT_ANPP.csv` - Precipitation and total ANPP data at EDGE sites. Corrected by Ingrid Slette et al. October 2022. Most current.

`EDGE_biomass_DROPPED.csv` - Values _removed_ by the outlier analysis. See `src/outlier_analysis.R`.

`EDGE_biomass_outliers_removed.csv` - Values _kept_ after the outlier analysis. This is the main data going forward.

`Taxa_Info.csv` - Key for the plant abbreviations (scientific name, pathway, functional group)

`genetic` 

- `genind_all.R` - genind data object (SNP data)
- `gracilis_traits.csv` - biomass from different ecotypes of _Bouteloua gracilis_ (blue grama)

`huxman_2004.csv` - Data from Huxman 2004 paper. Depricated. 

`precip.csv` - Precipitation and total ANPP data at EDGE sites. Depricated. 

`shapefiles` - File folder for spatial files used for mapping. See `src/site_map.R`.

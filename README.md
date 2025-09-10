# MAGShiny

An R Shiny app for visualizing metagenome-assembled genome (MAG) quality metrics, inspired by the 2024 MAGFlow/BIgMAG publication. Upload a TSV file with MAG metrics (e.g., Completeness, Contamination, N50) to explore interactive scatterplots, boxplots, heatmaps, and tables.

## Features
- Interactive dashboard with plotly and DT for dynamic exploration.
- Supports metrics from tools like CheckM2, BUSCO, GUNC, and QUAST.
- Statistical tests (e.g., Kruskal-Wallis) for comparing samples.
- Built with reproducible `renv` environment.

## Installation
1. Clone the repo: `git clone https://github.com/jlog3/MAGShiny.git`
2. Install R and required packages: `renv::restore()`
3. Run the app: `shiny::runApp('app.R')`

## Sample Data
- See `data/sample_MAG_data.tsv` for example input format.
- Source real data from [MGnify](https://www.ebi.ac.uk/metagenomics/).

## Demo
[Link to shinyapps.io demo, once deployed]

## Citation
Inspired by: MAGFlow/BIgMAG (2024). Available at: https://pmc.ncbi.nlm.nih.gov/articles/PMC11445639/
Original MAGFlow Git Repo: https://github.com/jeffe107/MAGFlow
Original BIgMAG Git Repo: https://github.com/jeffe107/BIgMAG

## License
MIT License

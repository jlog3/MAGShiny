# MAGShiny

**MAGShiny** is an interactive Shiny application for analyzing and visualizing the quality of metagenomic assembled genomes (MAGs). Inspired by BigMAG, it offers two workflows: uploading pre-processed quality assessment data (TSV/CSV) or analyzing raw MAG FASTA files using the MAGFlow pipeline (via Nextflow and Docker). The dashboard provides intuitive visualizations, including scatterplots, boxplots, taxonomy presence/absence matrices, and clustering heatmaps, to explore MAG quality metrics such as completeness, contamination, N50, and more.

## Features

- **Two Workflows**:
  - **Upload Processed Data**: Visualize pre-processed MAG quality data from tools like CheckM2, BUSCO, QUAST, and GUNC.
  - **Analyze Raw MAGs**: Run the MAGFlow pipeline on raw FASTA files using local Docker or cloud execution.
- **Interactive Visualizations**:
  - **Summary**: Bar plots displaying the distribution of high-, medium-, and low-quality MAGs.
  - **Scatterplots**: Completeness vs. contamination and BUSCO complete single-copy orthologs (SCO) vs. contamination.
  - **Boxplots**: Assembly statistics (N50) and GUNC contamination scores (CSS) by sample.
  - **Taxonomy Matrix**: Presence/absence of taxonomic groups across samples.
  - **Cluster Heatmap**: Sample clustering based on average quality metrics.
  - **Raw Data Table**: Interactive table with filtering and download capabilities.
- **Statistical Insights**: Kruskal-Wallis and Welch ANOVA tests for comparing completeness and N50 metrics.
- **Customizable Analysis**: Filter by completeness threshold and taxonomy level; color scatterplots by user-defined variables.
- **Downloadable Results**: Export processed data as TSV files.

## Prerequisites

### For Data Upload Workflow
- R (version 4.0 or higher)
- R packages: `shiny`, `shinydashboard`, `dplyr`, `tidyr`, `ggplot2`, `plotly`, `DT`, `stats`, `heatmaply`, `processx`, `readr`

### For MAG Analysis Workflow
- [Nextflow](https://www.nextflow.io/) installed and available in PATH
- [Docker](https://www.docker.com/) installed and running
- Internet connection for downloading the MAGFlow pipeline and containers
- MAGFlow pipeline (available at [jeffe107/MAGFlow](https://github.com/jeffe107/MAGFlow))

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/jlog3/MAGShiny.git
   ```
   
2. Install R and required packages:
   ```R
   install.packages("renv")
   renv::restore()
   ```

3. Run the application:
   ```R
   shiny::runApp("app.R")
   ```

Optional: Set Up MAGFlow for Raw MAG AnalysisInstall Nextflow: Follow instructions at nextflow.io.
Install Docker: Follow instructions at docker.com.
Ensure the MAGFlow pipeline is accessible (see jeffe107/MAGFlow).

## Usage

### Sample Data
- Example input format: `data/sample_MAG_data.tsv`
- Source real data from [MGnify](https://www.ebi.ac.uk/metagenomics/)

### Demo
[Link to shinyapps.io demo, once deployed]

### Workflows

#### 1. Upload Processed Data
- Select "Upload processed data (TSV/CSV)" in the sidebar.
- Upload a TSV or CSV file with the required columns (see below).
- Click "Load & Analyze Data" to generate visualizations.

#### 2. Analyze Raw MAG Files
- Select "Analyze raw MAG files (via MAGFlow)" in the sidebar.
- Upload one or more FASTA files.
- Choose execution mode (Local Docker or Cloud).
- Click "Run MAGFlow Analysis" to process the files.

### Required Data Columns (for TSV/CSV Upload)
| Column           | Description                                      |
|------------------|--------------------------------------------------|
| `Sample_ID`      | Unique identifier for each sample                |
| `Taxonomy_Level` | Taxonomic classification (e.g., genus, species) |
| `Completeness`   | MAG completeness percentage (from CheckM2)       |
| `Contamination`  | MAG contamination percentage (from CheckM2)     |
| `CSS`            | Contamination score from GUNC                   |
| `N50`            | Assembly N50 statistic (from QUAST)             |
| `Complete_SCO`   | Complete single-copy orthologs (from BUSCO)     |

### Exploring Results
- Navigate tabs (Summary, Scatterplots, Boxplots, Taxonomy Matrix, Cluster Heatmap, Raw Data) to explore results.
- Adjust settings like completeness threshold or taxonomy level in the sidebar.
- Export results using the "Download Processed Data" button in the Raw Data tab.

### Example Data Format
| Sample_ID | Taxonomy_Level | Completeness | Contamination | CSS | N50   | Complete_SCO |
|-----------|----------------|--------------|---------------|-----|-------|--------------|
| Sample1   | Bacteria       | 95.2         | 2.1           | 0.3 | 50000 | 90           |
| Sample2   | Archaea        | 80.5         | 5.0           | 0.5 | 30000 | 85           |

## Notes
- **Cloud Execution**: Requires manual setup of a cloud backend for Nextflow. Refer to the [MAGFlow documentation](https://github.com/jeffe107/MAGFlow) for details.
- **Performance**: The MAG analysis workflow duration depends on the number of FASTA files and system resources.
- **Troubleshooting**: Ensure Docker is running and Nextflow is configured correctly. Check the console for error messages if the analysis fails.

## Contributing
Contributions are welcome! To contribute:
1. Fork the repository.
2. Create a new branch:
   ```bash
   git checkout -b feature-name
   ```
3. Commit your changes:
   ```bash
   git commit -m 'Add feature'
   ```
4. Push to the branch:
   ```bash
   git push origin feature-name
   ```
5. Submit a pull request.

Please open an issue to report bugs or suggest improvements.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments
- Inspired by [BigMAG](https://github.com/jeffe107/BIgMAG).
- Built with [Shiny](https://shiny.rstudio.com/) and [MAGFlow](https://github.com/jeffe107/MAGFlow).
- Thanks to the open-source bioinformatics community for tools like CheckM2, BUSCO, QUAST, and GUNC.

## Citation
- Inspired by: MAGFlow/BIgMAG (2024). Available at: [PMC11445639](https://pmc.ncbi.nlm.nih.gov/articles/PMC11445639/)
- Original MAGFlow Repository: [jeffe107/MAGFlow](https://github.com/jeffe107/MAGFlow)
- Original BigMAG Repository: [jeffe107/BIgMAG](https://github.com/jeffe107/BIgMAG)
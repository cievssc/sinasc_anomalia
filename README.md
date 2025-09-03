# Painel Sinasc / Anomalia Congênita

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1FA8E0?style=for-the-badge&logo=r&logoColor=white)
![SUS](https://img.shields.io/badge/SUS-blue?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)

Painel dos dados elaborado para explorar e sintetizar os dados do Sistema de Informações sobre Nascidos Vivos (SINASC), tendo 

## 🌟 Visão geral

A aplicação 

## ✨ Features

- **Data Upload**: Support for CSV, Excel, and TSV files
- **Interactive Filtering**: Dynamic controls for subsetting data
- **Multiple Visualization Types**: 
  - Scatter plots
  - Bar charts
  - Histograms
  - Box plots
  - Heatmaps
- **Real-time Updates**: All visualizations update immediately as parameters change
- **Data Export**: Download filtered data or visualizations in multiple formats
- **Responsive Design**: Adapts to different screen sizes
- **Theme Options**: Light and dark mode support

## 🚀 Installation

### Prerequisites

- R (version 4.0.0 or higher)
- RStudio (recommended)

### Step-by-Step Setup

1. Clone this repository:
```bash
git clone https://github.com/yourusername/shiny-data-explorer.git
cd shiny-data-explorer
```

2. Install required R packages:
```r
# Run in R console
install.packages(c("shiny", "ggplot2", "dplyr", "readxl", "DT", "shinythemes", "plotly"))
```

3. Launch the application:
```r
# Run in R console
shiny::runApp()
```

Alternatively, open the `app.R` file in RStudio and click "Run App".

## 📖 Usage

1. **Data Input**: Start by uploading your dataset using the file input control or select one of the provided sample datasets.

2. **Data Filtering**: Use the sidebar controls to filter your data based on variable values, ranges, or categories.

3. **Visualization**: Choose a plot type from the dropdown and customize its appearance using the options provided.

4. **Export Results**: Download the filtered dataset as a CSV file or save visualizations as PNG/PDF files.

## 📁 Project Structure

```
sinasc_app/
│
├── app.R                 # Main application file
├── global.R              # Global variables and functions
├── server.R              # Server logic
├── ui.R                  # User interface definition
├── www/                  # Web resources
│   ├── style.css         # Custom CSS styles
│   └── logo.png          # Application logo
├── data/                 # Sample data directory
│   ├── sample_data.csv
│   └── example_data.xlsx
├── modules/              # Shiny modules
│   ├── data_input.R
│   ├── filters.R
│   └── plots.R
├── tests/                # Test scripts
│   ├── test-server.R
│   └── test-modules.R
└── README.md
```

## ⚙️ Configuration

The application can be customized through several options:

### Theme Selection
Modify the UI theme by changing the `theme` parameter in `ui.R`:

```r
shinytheme("cerulean")  # Options: cerulean, darkly, flatly, etc.
```

### Data Limits
Adjust maximum upload size in `server.R`:
```r
options(shiny.maxRequestSize = 30*1024^2)  # 30MB limit
```

### Preloaded Datasets
Add sample datasets by placing them in the `data/` directory and updating the dataset selection dropdown in `ui.R`.

## 🤝 Contributing

We welcome contributions! Please follow these steps:

1. Fork the project
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


---

⭐ Star this repo if you found it helpful!

For questions or support, please open an issue or contact [your-email@example.com].
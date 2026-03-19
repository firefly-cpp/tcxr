<h1 align="center">
    tcxr
</h1>

<h2 align="center">
    A parser for TCX files written in R
</h2>

<p align="center">
  <a href="https://github.com/firefly-cpp/tcxr/actions?workflow=R-CMD-check">
    <img alt="R build status" src="https://github.com/firefly-cpp/tcxr/workflows/R-CMD-check/badge.svg">
  </a>
  <a href="https://CRAN.R-project.org/package=tcxr">
    <img alt="CRAN version" src="https://www.r-pkg.org/badges/version/tcxr">
  </a>
  <a href="https://cran.r-project.org/package=tcxr">
    <img alt="CRAN downloads" src="https://cranlogs.r-pkg.org/badges/grand-total/tcxr?color=blue">
  </a>
  <a href="https://doi.org/10.32614/CRAN.package.tcxr">
    <img alt="DOI" src="https://img.shields.io/badge/DOI-10.32614/CRAN.package.tcxr-blue">
  </a>
</p>

<p align="center">
    <a href="#-installation">📦 Installation</a> •
    <a href="#-features">✨ Features</a> •
    <a href="#-usage">🚀 Usage</a> •
    <a href="#-datasets">💾 Datasets</a> •
    <a href="#-further-read">📖 Further read</a> •
    <a href="#-related-packagesframeworks">🔗 Related packages/frameworks</a> •
    <a href="#-license">🔑 License</a>
</p>

tcxr is an R package designed to parse Training Center XML (TCX) files and extract key activity metrics. This package helps analyze workout and training data from devices that export TCX format, providing insights such as total distance, duration, calories burned, altitude, and power values. The Ruby package [tcxread](https://github.com/firefly-cpp/tcxread) was the inspiration for this R package.

## 📦 Installation


Install CRAN release version:

```R
install.packages("tcxr")
```

## ✨ Features

- Parse TCX files to extract activity metrics
- Compute total distance, duration, and calories burned
- Analyze maximum altitude and power values
- Works with TCX data exported from Garmin and similar devices

## 🚀 Usage

### Load the package

```r
library(tcxr)
```

### Read a TCX file

```r
# Provide the path to a TCX file
tcx <- TCXRead("activity.tcx")

tcx$summary      # aggregated activity metrics
tcx$raw_data     # full trackpoint dataframe
```

## 💾 Datasets

Datasets available and used in the examples on the following links: [DATASET1](https://iztok-jr-fister.eu/static/publications/Sport5.zip), [DATASET2](https://iztok-jr-fister.eu/static/css/datasets/Sport.zip), [DATASET3](https://github.com/firefly-cpp/tcx-test-files).

## 📖 Further read

[1] [Awesome Computational Intelligence in Sports](https://github.com/firefly-cpp/awesome-computational-intelligence-in-sports)

## 🔗 Related packages/frameworks

[1] [tcxreader: Python reader/parser for Garmin's TCX file format.](https://github.com/alenrajsp/tcxreader)

[2] [sport-activities-features: A minimalistic toolbox for extracting features from sports activity files written in Python](https://github.com/firefly-cpp/sport-activities-features)

[3] [TCXReader.jl: Julia package designed for parsing TCX files](https://github.com/firefly-cpp/TCXReader.jl)

[4] [TCXWriter: A Tiny Library for writing/creating TCX files on Arduino](https://github.com/firefly-cpp/tcxwriter)

## 🔑 License

This package is distributed under the MIT License. This license can be found online at <http://www.opensource.org/licenses/MIT>.

## Disclaimer

This framework is provided as-is, and there are no guarantees that it fits your purposes or that it is bug-free. Use it at your own risk!

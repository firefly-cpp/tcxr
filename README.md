# tcxr

## Overview

tcxr is an R package designed to parse Training Center XML (TCX) files and extract key activity metrics. This package helps analyze workout and training data from devices that export TCX format, providing insights such as total distance, duration, calories burned, altitude, and power values.

## Installation

```r
# TODO
```

## Features

- Parse TCX files to extract activity metrics
- Compute total distance, duration, and calories burned
- Analyze maximum altitude and power values
- Works with TCX data exported from Garmin and similar devices

## Usage

### Load the package

```r
library(tcxr)
```

### Read a TCX file

```r
# Provide the path to a TCX file
example_tcx_file <- "example.tcx"

# Parse the TCX file and extract metrics
result <- TCXRead(example_tcx_file)

# Print the extracted data
print(result)
```
## 💾 Datasets

Datasets available and used in the examples on the following links: [DATASET1](http://iztok-jr-fister.eu/static/publications/Sport5.zip), [DATASET2](http://iztok-jr-fister.eu/static/css/datasets/Sport.zip), [DATASET3](https://github.com/firefly-cpp/tcx-test-files).

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

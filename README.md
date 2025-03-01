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



% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcxr.R
\name{parse_trackpoint}
\alias{parse_trackpoint}
\title{Parse a Trackpoint from a TCX File}
\usage{
parse_trackpoint(trackpoint)
}
\arguments{
\item{trackpoint}{An XML node representing a trackpoint.}
}
\value{
A list of parsed trackpoint metrics.
}
\description{
Extracts data from a trackpoint, including altitude, distance, speed, power, cadence, and heart rate.
}

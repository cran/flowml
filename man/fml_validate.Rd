% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fml_validate.R
\name{fml_validate}
\alias{fml_validate}
\title{fml_validate}
\usage{
fml_validate(parser_inst)
}
\arguments{
\item{parser_inst}{instance of fml_parser class that comprises command line arguments.}
}
\value{
none
}
\description{
Pipeline function that performs a validation experiment on a
a caret train object based on test samples.
}
\examples{
\dontrun{
parser_inst <-  flowml::create_parser()

parser_inst$pipeline_segment <- "validate"
parser_inst$config <- flowml::fml_example(file = "reg_config.json")
parser_inst$data <- flowml::fml_example(file = "reg_data.csv")
parser_inst$samples_train <- flowml::fml_example(file = "reg_samples_train.txt")
parser_inst$samples_test <- flowml::fml_example(file = "reg_samples_test.txt")
parser_inst$features <- flowml::fml_example(file = "reg_features.txt")
parser_inst$extended_features <- flowml::fml_example(file = "reg_features_extended.txt")
parser_inst$trained <- flowml::fml_example(file = "reg_fit.rds")
parser_inst$permutation <- "none"
parser_inst$result_dir <- tempdir()

flowml::fml_validate(parser_inst = parser_inst)
}

}
\author{
Kolja Becker
}

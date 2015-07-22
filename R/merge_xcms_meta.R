#' Merge a XCMS output file with meta data
#' @description This functions combines meta data with a XCMS output file. The
#'   merge is done based on the sample name.
#' @param XcmsFile the XCMS output file. This is a comma seperated file.
#' @param MetaFile the meta data file. This file should be comma seperated.
#' @return a data frame with the meta data and the XCMS data combined.
#' @details This functions is based on our data analysis protocol. The XCMS peak
#'   picking is done on the QCpool and actual samples. Sample names should start
#'   with \code{QCpool_} or \code{Sample_}. This is used within the function to find where the
#'   actual data starts. The MetaFile should contain a column with the name
#'   \code{SampleName}. Merging of both files is based on the sample name.
#' @author Rico Derks
merge_xcms_meta <- function(XcmsFile, MetaFile) {
  # read the meta data
  MetaData <- read.table(file=MetaFile, header=TRUE, sep=",");

  # read the xcms output file
  XCMSData <- read.table(file=XcmsFile, header=TRUE, sep=",");
  FeatureNames <- XCMSData[, "name"];
  DataStart <- grep(x=colnames(XCMSData), pattern="QCpool_")[1];
  # only get the feature data and transpose the matrix (samples on rows and features as columns)
  PeakData <- t(XCMSData[, DataStart:ncol(XCMSData)]);
  colnames(PeakData) <- FeatureNames;
  SampleNames <- rownames(PeakData);

  # combine metadata with the peakdata
  # by.y=0 -> 0 means by rownames
  merge_xcms_meta <- merge(MetaData, PeakData, by.x="SampleName", by.y=0);
}

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

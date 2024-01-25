#' @export
createVideo <- function(
  imageDirPath,
  outDirPath = imageDirPath,
  frameRate = 5,
  outFormat = c("mpeg4", "h264", "gif"),
  keepInfoTxtFile = FALSE,
  nBatches = 1,
  batchIndex = 1
) {
  cat("Creating videos from images in", imageDirPath, "...\n")

  outFormat <- match.arg(outFormat)
  videoCodecSpec <- switch(outFormat,
    "mpeg4" = '-c:v mpeg4 -q:v 1 -vf "fps=25,format=yuv420p"',
    "h264" = '-c:v libx264 -q:v 1 -vf "fps=25"',
    "gif" = '-filter_complex "[0:v] split [a][b];[a] palettegen [p];[b][p] paletteuse"',
    stop("Unknown format: ", outFormat))
  outFileEnding <- switch(outFormat,
    "mpeg4" = ".mp4",
    "h264" = ".mp4",
    "gif" = ".gif",
    stop("Unknown format: ", outFormat))

  fileNames <- list.files(path = imageDirPath, pattern = ".+_[+-]?\\d+(\\.\\d*)?\\.png$")
  matches <- str_match(fileNames, "(.+)_([+-]?\\d+(\\.\\d*)?)\\.png$")[,1:3]
  colnames(matches) <- c("fileName", "prefix", "number")
  tbl <-
    matches |>
    as_tibble() |>
    mutate(
      number = as.numeric(.data$number),
      filePath = normalizePath(file.path(imageDirPath, .data$fileName)))
  uniquePrefixes <- unique(tbl$prefix)
  cat(sprintf("Found %d files with %d unique prefixes.\n", nrow(tbl), length(uniquePrefixes)))

  batch <- splitAndGetOneBatch("prefix", uniquePrefixes, nBatches, batchIndex)

  for (prefix in batch) {
    createVideoForPrefix(prefix, tbl, outDirPath, frameRate, videoCodecSpec, outFileEnding, keepInfoTxtFile)
  }
}

createVideoForPrefix <- function(prefix, tbl, outDirPath, frameRate, videoCodecSpec, outFileEnding, keepInfoTxtFile = FALSE) {
  cat("Processing", prefix, "images...\n")
  outFilePath <- file.path(outDirPath, paste0(prefix, outFileEnding))
  if (file.exists(outFilePath)) {
    cat("File", outFilePath, "already exists. Skipping.\n")
    return(invisible())
  }
  pt <- proc.time()[3]
  tbl <-
    tbl |>
    filter(.data$prefix == .env$prefix) |>
    arrange(.data$number)
  invalidFiles <- file.size(tbl$filePath) == 0
  if (any(invalidFiles)) {
    cat(
      "WARNING: Some files are invalidFiles:\n",
      paste(tbl$filePath[invalidFiles], collapse=",\n"),
      "\nIgnoring those.\n")
    tbl <- tbl[!invalidFiles,]
  }
  cat("Use", nrow(tbl), "images to create video", prefix, ".\n")

  lines <- c(
    paste0("file '", tbl$filePath, "'"),
    paste0("file '", last(tbl$filePath), "'"))
  if (keepInfoTxtFile) {
    infoFilePath <- file.path(
      outDirPath,
      paste0(
        prefix,
        "_imageToVideoInfo_",
        Sys.time() |> format("%Y%m%d_%H%M%S"),
        ".txt"))
  } else {
    infoFilePath <- tempfile(fileext = ".txt")
  }
  cat("Writing info file", infoFilePath, "...\n")
  writeLines(lines, infoFilePath)
  cmdText <- sprintf(
    'ffmpeg -r %f -f concat -safe 0 -i "%s" %s "%s"',
    frameRate,
    normalizePath(infoFilePath, mustWork=TRUE),
    videoCodecSpec,
    normalizePath(outFilePath, mustWork=FALSE)
  )
  cat("run command:\n")
  cat(cmdText, "\n")
  system(cmdText)
  if (!keepInfoTxtFile) file.remove(infoFilePath)
  cat("Done after", proc.time()[3] - pt, "s.\n")
}

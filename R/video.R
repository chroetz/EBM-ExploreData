#' @export
createVideo <- function(
  imageDirPath,
  outDirPath = imageDirPath,
  frameRate = 5,
  nBatches = 1,
  batchIndex = 1
) {

  cat("Creating videos from images in", imageDirPath, "...\n")

  fileNames <- list.files(path = imageDirPath, pattern = ".+_[+-]?\\d+(\\.\\d*)?\\.png$")
  matches <- str_match(fileNames, "(.+)_([+-]?\\d+(\\.\\d*)?)\\.png$")[,1:3]
  colnames(matches) <- c("fileName", "prefix", "number")
  tbl <-
    matches |>
    as_tibble() |>
    mutate(
      number = as.numeric(number),
      filePath = normalizePath(file.path(imageDirPath, fileName)))
  uniquePrefixes <- unique(tbl$prefix)
  cat(sprintf("Found %d files with %d unique prefixes.\n", nrow(tbl), length(uniquePrefixes)))

  batch <- splitAndGetOneBatch("prefix", uniquePrefixes, nBatches, batchIndex)

  for (prefix in batch) {
    createVideoForPrefix(prefix, tbl, outDirPath, frameRate)
  }
}

createVideoForPrefix <- function(prefix, tbl, outDirPath, frameRate) {
  cat("Processing", prefix, "images...\n")
  outFilePath <- file.path(outDirPath, paste0(prefix, ".mp4"))
  if (file.exists(outFilePath)) {
    cat("File", outFilePath, "already exists. Skipping.\n")
    return(invisible())
  }
  pt <- proc.time()[3]
  tbl <- filter(tbl, .data$prefix == .env$prefix) |> arrange(number)
  lines <- c(
    paste0("file '", tbl$filePath, "'"),
    paste0("file '", last(tbl$filePath), "'"))
  infoFilePath <- file.path(outDirPath, paste0(prefix, "_imageToVideoInfo.txt"))
  cat("Writing info file", infoFilePath, "...\n")
  writeLines(lines, infoFilePath)
  cmdText <- sprintf(
    'ffmpeg -r %f -f concat -safe 0 -i "%s" -c:v libx264 -vf "fps=25,format=yuv420p" "%s"',
    frameRate,
    normalizePath(infoFilePath, mustWork=TRUE),
    normalizePath(outFilePath, mustWork=FALSE)
  )
  cat("run command:\n")
  cat(cmdText, "\n")
  system(cmdText)
  file.remove(infoFilePath)
  cat("Done after", proc.time()[3] - pt, "s.\n")
}

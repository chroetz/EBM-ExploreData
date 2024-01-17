#' @export
createMaps <- function(
  dataFilePath,
  dataVariableName,
  dataRegionName = "GID_1",
  dataTimeName = "year",
  variableTransformationText = NULL,
  shapeFilePath,
  shapeRegionName = "GID_1",
  outDir,
  widthInPx = 1920,
  heightInPx = 1080,
  dpi = 150,
  nBatches = 1,
  batchIndex = 1,
  timeFilter = NULL,
  regionFilter = NULL
) {

  data <- read_csv(dataFilePath, col_types = cols())
  if (hasValue(timeFilter)) {
    data <- filter(data, .data[[dataTimeName]] %in% timeFilter)
  }
  if (hasValue(regionFilter)) {
    data <- filter(data, .data[[dataRegionName]] %in% regionFilter)
  }

  if (hasValueString(variableTransformationText)) {
    transformation <- eval(parse(text = variableTransformationText))
    data <-
      data |>
      mutate(across(all_of(dataVariableName), transformation))
  }

  cat("read geodata file...")
  pt <- proc.time()[3]
  shape <-
    shapeFilePath |>
    read_sf() |>
    select(all_of(shapeRegionName), .data$geom)
  if (hasValue(regionFilter)) {
    shape <- filter(shape, .data[[shapeRegionName]] %in% regionFilter)
  }
  cat(" done after", proc.time()[3] - pt, "s\n")

  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }

  dataTimeValues <- unique(data[[dataTimeName]])
  cat("Split", dataTimeName, "into", nBatches, "batches.\n")
  batches <- setupBatches(dataTimeValues, nBatches)
  batch <- batches[[batchIndex]]
  if (length(batch) == 0) {
    cat("Batch", batchIndex, "is empty. Nothing to do.\n")
    return(invisible())
  }
  cat("Process batch", batchIndex, "with", length(batch), dataTimeName, "values.\n")

  for (dataTimeValue in batch) {
    cat("processing", dataTimeName, dataTimeValue, "... ")
    pt <- proc.time()[3]

    title <- paste(dataVariableName, dataTimeValue, sep = "_")

    dataWithShape <-
      shape |>
      left_join(
        data |>
          filter(.data[[dataTimeName]] == .env$dataTimeValue) |>
          select(all_of(dataRegionName), all_of(dataVariableName)),
        join_by(!!shapeRegionName == !!dataRegionName))

    plt <-
      dataWithShape |>
      ggplot(aes(geometry = .data$geom, fill = .data[[dataVariableName]])) +
        geom_sf() +
        ggtitle(title) +
        scale_fill_viridis_c(option = "C", limits = range(data[[dataVariableName]])) +
        theme(legend.position="bottom")

    ggplot2::ggsave(
      file.path(outDir, paste0(title, ".png")),
      plot = plt,
      width = widthInPx, height = heightInPx, units = "px", dpi = dpi)
    cat(" done after", proc.time()[3] - pt, "s\n")
  }
}

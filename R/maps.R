#' @export
createMaps <- function(
  dataFilePath,
  dataSetName = cerUtility::removeFileNameEnding(basename(dataFilePath)),
  dataVariableName,
  dataRegionName = "GID_1",
  dataTimeName = "year",
  variableTrans = "identity",
  limits = NULL,
  outOfBoundsHandling = NULL,
  shapeFilePath,
  shapeRegionName = "GID_1",
  outDir,
  outFileGlue = "{dataSetName}_{dataVariableName}_{variableTrans}_{dataTimeValue}.png",
  plotTitleGlue = "{dataSetName} {dataTimeValue}",
  widthInPx = 1920,
  heightInPx = 1080,
  dpi = 150,
  nBatches = 1,
  batchIndex = 1,
  timeFilter = NULL,
  regionRegex = NULL
) {

  # load and filter data
  data <- read_csv(dataFilePath, col_types = cols())
  if (hasValue(timeFilter)) {
    data <- filter(data, .data[[dataTimeName]] %in% timeFilter)
  }
  if (hasValue(regionRegex)) {
    data <- filter(data, stringr::str_detect(.data[[dataRegionName]], regionRegex))
  }

  # limits and out of bounds
  v <- data[[dataVariableName]]
  vRange <- range(v[is.finite(v)])
  if (length(limits) != 2) {
    limits <- vRange
  }
  if (is.na(limits[1])) limits[1] <- vRange[1]
  if (is.na(limits[2])) limits[2] <- vRange[2]
  if (!hasValueString(outOfBoundsHandling)) {
    outOfBoundsHandling <- "censor"
  }
  oob <- switch(
    outOfBoundsHandling,
    "squish" = scales::squish,
    "squish_infinite" = scales::squish_infinite,
    "censor" = scales::censor,
    scales::censor)
  labelsFunction <- if (startsWith(outOfBoundsHandling, "squish")) {
      \(x) {
        labels <- scales::label_number_auto()(x)
        if (length(labels) >= 2) {
          if (limits[1] > vRange[1]) labels[1] <- paste0("\u2264", labels[1])
          if (limits[2] < vRange[2]) labels[length(labels)] <- paste0("\u2265", labels[length(labels)])
        }
        return(labels)
    }
  } else {
    scales::label_number_auto()
  }
  transform <- scales::as.transform(variableTrans)
  if (!is.finite(transform$transform(0)) && limits[1] == 0) {
    limits[1] <- pmax(.Machine$double.xmin, min(v[v>0], na.rm = TRUE))
    cat("Transformed 0 is NaN. Setting lower limit to", limits[1], "\n")
  }

  cat("read geodata file...")
  pt <- proc.time()[3]
  shape <-
    shapeFilePath |>
    read_sf() |>
    select(all_of(shapeRegionName), .data$geom)
  if (hasValue(regionRegex)) {
    shape <- filter(shape, str_detect(.data[[dataRegionName]], regionRegex))
  }
  cat(" done after", proc.time()[3] - pt, "s\n")

  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }

  dataTimeValues <- unique(data[[dataTimeName]])
  batch <- splitAndGetOneBatch(dataTimeName, dataTimeValues, nBatches, batchIndex)

  for (dataTimeValue in batch) {
    cat("processing", dataTimeName, dataTimeValue, "... ")
    pt <- proc.time()[3]

    plotTitle <- str_glue(plotTitleGlue)

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
        ggtitle(plotTitle) +
        scale_fill_viridis_c(
          option = "C",
          limits = limits,
          labels = labelsFunction,
          oob = oob,
          trans = variableTrans,
          guide = ggplot2::guide_colorbar(
            title.position = "top",
            barwidth = grid::unit(0.9, "npc"))
        ) +
        theme(legend.position="bottom")

    ggplot2::ggsave(
      file.path(outDir, str_glue(outFileGlue)),
      plot = plt,
      width = widthInPx, height = heightInPx, units = "px", dpi = dpi)
    cat(" done after", proc.time()[3] - pt, "s\n")
  }
}

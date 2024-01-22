#' @export
createMaps <- function(
  dataFilePath,
  dataSetName = EbmUtility::removeFileNameEnding(basename(dataFilePath)),
  dataVariableName,
  dataRegionName = "GID_1",
  dataTimeName = "year",
  variableTrans = "identity",
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

  data <- read_csv(dataFilePath, col_types = cols())
  if (hasValue(timeFilter)) {
    data <- filter(data, .data[[dataTimeName]] %in% timeFilter)
  }
  if (hasValue(regionRegex)) {
    data <- filter(data, stringr::str_detect(.data[[dataRegionName]], regionRegex))
  }
  v <- data[[dataVariableName]]
  limits <- range(v[is.finite(v)])
  transform <- scales::as.transform(variableTrans)
  if (!is.finite(transform$transform(0)) && limits[1] == 0) {
    limits[1] <- pmax(.Machine$double.xmin, min(v[v>0]))
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

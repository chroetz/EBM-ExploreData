createMaps <- function(
  year,
  variable,
  outDir,
  dataFilePath,
  shapefilePath,
  sfRegionName = "GID_1",
  variableRegionName = "GID_1",
  transformation = \(x) log10(x+1)
) {

  data <-
    read_csv(dataFilePath, col_types = cols()) |>
    mutate(across(all_of(variable), transformation))

  cat("read geodata file...")
  pt <- proc.time()[3]
  sf <-
    shapefilePath |>
    read_sf() |>
    select(all_of(sfRegionName), geom)
  cat(" done after", proc.time()[3] - pt, "s\n")

  title <- paste(variable, year, sep = "_")

  dataWithShape <-
    sf |>
    left_join(
      data |>
        filter(.data$year == .env$year) |>
        select(all_of(variableRegionName), all_of(variable)),
      join_by(!!sfRegionName == !!variableRegionName))

  plt <-
    dataWithShape |>
    ggplot(aes(geometry = geom, fill = .data[[variable]])) +
      geom_sf() +
      ggtitle(title) +
      scale_fill_viridis_c(option = "B", limits = range(data[[variable]])) +
      theme(legend.position="bottom")

  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }

  cat("creating and saving plot", title, "... ")
  pt <- proc.time()[3]
  ggplot2::ggsave(
    file.path(outDir, paste0(title, ".png")),
    plot = plt,
    width = 1920, height = 1080, units = "px", dpi = 150)
  cat(" done after", proc.time()[3] - pt, "s\n")

}

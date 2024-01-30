#' @export
showTable <- function(data, caption, digits = 3) {
  if (knitr::is_latex_output()) {
    caption <- knitrEscapeLatex(caption)
  }
  data |>
    kableExtra::kbl(
      caption = caption,
      digits = digits,
      booktabs = TRUE,
      align = c("l", rep("r", ncol(data)-1))
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("condensed", "striped"),
      latex_options = c("HOLD_position"),
      full_width = FALSE)
}

knitrEscapeLatex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("\\\\textbackslash", "\\\\textbackslash{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  return(x)
}

#' @export
showStandardSummary <- function(d, params, shapeRegion = NULL, shapeCountry = NULL) {
  variableNamePattern <- params$variableName
  cat("\n\n## Summary Table\n\n")
  summaryShowSummaryTable(d, variableNamePattern)
  cat("\n\n## Histograms\n\n")
  summaryShowHistograms(d, params$transformations, variableNamePattern)
  cat("\n\n## Tables of Extremes\n\n")
  summaryShowRanked(d, variableNamePattern)
  if (params$timeName %in% colnames(d)) {
    cat("\n\n## Plots by Time\n\n")
    summaryShowPlotByTime(d, params$timeName, params$transformations, variableNamePattern)
  }
  if (params$createMaps) {
    isRegionDistinct <- length(unique(d[[params$regionName]])) == nrow(d)
    if (isRegionDistinct && !is.null(shapeRegion)) {
      cat("\n\n## Region Maps\n\n")
      summaryShowMaps(
        d, shapeRegion,
        params$transformations,
        params$variableName,
        shapeBy = params$shapeRegionName,
        dataBy = params$regionName)
    }
    isCountryDistinct <- length(unique(d[[params$countryName]])) == nrow(d)
    if (isCountryDistinct && !is.null(shapeCountry)) {
      cat("\n\n## Country Maps\n\n")
      summaryShowMaps(
        d, shapeCountry,
        params$transformations,
        params$variableName,
        shapeBy = params$shapeCountryName,
        dataBy = params$countryName)
    }
  }
}


#' @export
summarizeDataBy <- function(data, byVariableName, valueVariableName, aggregateFunctions) {
  d <-
    data |>
    summarize(
      across(
        .cols = !!valueVariableName,
        .fns = aggregateFunctions,
        .names = "{.fn}_{valueVariableName}"),
      .by = !!byVariableName)
  return(d)
}




summaryShowSummaryTable <- function(d, variableNamePattern) {
  d0 <- d |> select(matches(variableNamePattern))
  x <- tibble(
    name = colnames(d0),
    n = sapply(d0, length),
    nNA = sapply(d0, \(x) sum(is.na(x))),
    mean = sapply(d0, mean, na.rm = TRUE),
    sd = sapply(d0, stats::sd, na.rm = TRUE),
    min = sapply(d0, min, na.rm = TRUE),
    quantile_25 = sapply(d0, stats::quantile, probs = 0.25, na.rm = TRUE),
    median = sapply(d0, stats::median, na.rm = TRUE),
    quantile_75 = sapply(d0, stats::quantile, probs = 0.75, na.rm = TRUE),
    max = sapply(d0, max, na.rm = TRUE)
  )
  cat("\n\n")
  x |>
    showTable(paste0("Summary statistics of ", variableNamePattern)) |>
    cat()
  cat("\n\n")
}


summaryGetPrefix <- function(variableNames, transformations) {
  prefix <- str_extract(variableNames, "^[^_]+(?=_)")
  # first transformation name is default
  prefix[is.na(prefix)] <- names(transformations)[1]
  prefix[!prefix %in% names(transformations)] <- names(transformations)[1]
  return(prefix)
}


summaryShowHistograms <- function(d, transformations, variableNamePattern) {
  variableNames <- str_subset(colnames(d), variableNamePattern)
  prefix <- summaryGetPrefix(variableNames, transformations)
  for (i in seq_along(variableNames)) {
    variableName  <- variableNames[i]
    for (trans in transformations[[prefix[i]]]) {
      transformer <- scales::as.transform(trans)
      graphics::hist(
        transformer$transform(d[[variableName]]),
        xlab=variableName,
        main=paste0("Histogram of ", variableName, " (", trans, ")"))
    }
  }
}


summaryShowRanked <- function(d, variableNamePattern) {
  variableNames <- str_subset(colnames(d), variableNamePattern)
  for (variableName in variableNames) {
    dRanked <-
      d |>
      drop_na(!!variableName) |>
      mutate(rank = rank(.data[[variableName]], ties.method = "first"))
    cat("\n\n")
    dRanked |>
      filter(rank <= 10) |>
      arrange(rank) |>
      showTable(paste0("Smallest 10 Values of ", variableName)) |>
      cat()
    cat("\n\n")
    dRanked |>
      filter(rank >= max(rank)-10) |>
      arrange(desc(rank)) |>
      showTable(paste0("Largest 10 Values of ", variableName)) |>
      cat()
    cat("\n\n")
  }
}


summaryShowPlotByTime <- function(d, timeName, transformations, variableNamePattern) {
  isTimeDistinct <- length(unique(d[[timeName]])) == nrow(d)
  variableNames <- str_subset(colnames(d), variableNamePattern)
  prefix <- summaryGetPrefix(variableNames, transformations)
  for (i in seq_along(variableNames)) {
    variableName  <- variableNames[i]
    for (trans in transformations[[prefix[i]]]) {
      if (isTimeDistinct) {
        plt <-
          d |>
          ggplot(aes(x = .data[[timeName]], y = .data[[variableName]])) +
          geom_line(na.rm = TRUE) +
          geom_point(na.rm = TRUE) +
          ggtitle(paste0("Plot of ", variableName, " (", trans, ")")) +
          scale_y_continuous(trans = trans)
        plot(plt)
      } else {
        plt <-
          d |>
          ggplot(aes(x = .data[[timeName]], y = .data[[variableName]])) +
          geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), na.rm = TRUE) +
          geom_point(alpha = 0.1, na.rm = TRUE) +
          ggtitle(paste0("Smoothed plot of ", variableName, " (", trans, ")")) +
          scale_y_continuous(trans = trans)
        plot(plt)
      }
    }
  }
}





summaryShowMaps <- function(d, shape, transformations, variableNamePattern, shapeBy, dataBy) {
  dataWithShape <-
    shape |>
    left_join(
      d,
      join_by(!!shapeBy == !!dataBy))
  variableNames <- str_subset(colnames(d), variableNamePattern)
  prefix <- summaryGetPrefix(variableNames, transformations)
  for (i in seq_along(variableNames)) {
    variableName  <- variableNames[i]
    for (trans in transformations[[prefix[i]]]) {
      plt <- summaryCreateMapForShowMaps(dataWithShape, variableName, trans)
      plot(plt)
    }
  }
}


summaryCreateMapForShowMaps <- function(dataWithShape, variableName, trans) {
  dataWithShape |>
  ggplot(aes(geometry = .data$geom, fill = .data[[variableName]])) +
    geom_sf() +
    ggtitle(paste0("Map of ", variableName, " (", trans, ")")) +
    scale_fill_viridis_c(
      option = "C",
      trans = trans,
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth = grid::unit(0.9, "npc"))
    ) +
    theme(legend.position="bottom")
}

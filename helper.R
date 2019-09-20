file_ext2 <- function(x)
{
  pos <- regexpr("\\.([[:alnum:]]+)(\\.(gz|bz2|xz|zip))*$", x)
  if(pos > -1L) substring(x, pos + 1L) else ""
}

non_num <- function(x) !is.numeric(x)

zipped <- function(...)
{
  unlist(lapply(list(...), paste0, c("", ".gz", ".bz2", ".xz", ".zip")))
}

read_my_file <- function(fp)
{
  ext <- tools::file_ext(fp)
  if(ext %in% zipped("csv"))
  {
    out <- read_csv(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% zipped("txt"))
  {
    out <- read_table2(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% zipped("tab", "tsv"))
  {
    out <- read_tsv(fp, col_names = TRUE, col_types = cols())
  } else if(ext %in% "sas7bdat")
  {
    out <- haven::read_sas(fp)
  } else if(ext %in% c("xlsx", "xls"))
  {
    out <- readxl::read_excel(fp)
  }
  attr(out, "extension") <- ext
  out
}

#################################################################################################################

count_unique <- function(vars, dat)
{
  vars <- vars[vars %in% colnames(dat)]
  map_int(dat[vars], function(x) length(unique(x)))
}

do_the_tableby <- function(y, x, strat, dat)
{
  x <- x[x != " "]
  validate(
    need(!is.null(dat) && length(y) * length(x) * nrow(dat) > 0, "Please select x-variable(s) and (optionally) a by-variable."),
    need(y == " " || count_unique(y, dat) <= 20, "This tab only supports by-variables with <= 20 unique levels."),
    need(!identical(y, x), "Sorry, the x-variables and by-variable can't be identical.")
  )
  Call <- call("tableby", formula = quote(formulize(y, x)), data = quote(dat))
  if(strat != " ") Call$strata <- as.name(strat)
  eval(Call, environment())
}

#################################################################################################################

PLOTTYPES <- c("Scatter Plot" = "geom_point",
               "Histogram" = "geom_histogram",
               "Boxplot" = "geom_boxplot",
               "Line Plot" = "geom_line")

SCALETYPES <- function(a)
{
  out <- paste0("scale_", a, "_", c("log10", "sqrt", "reverse"))
  names(out) <- c("Log10", "Square Root", "Reverse")
  c("(No Transformation)" = " ", out)
}

do_the_ggplot <- function(..., facet, type, scale_y, scale_x, dat)
{
  args <- list(...)
  FUN <- match.fun(type)

  validate(
    need((args$y != " " || type == "geom_histogram") && args$x != " ", "Please select x- and y-variables."),
    need(type != "geom_histogram" || !non_num(dat[[args$x]]), "Histograms require a continuous x-variable!"),
    need(scale_y == " " || !non_num(dat[[args$y]]), "Scale transformations can't be used on non-numeric data!"),
    need(scale_x == " " || !non_num(dat[[args$x]]), "Scale transformations can't be used on non-numeric data!")
  )

  if(type == "geom_histogram") args$y <- NULL

  args <- args[map_lgl(args, function(x) x != " ")]

  p <- ggplot(dat, do.call(aes_string, args)) +
    FUN()
  if(facet != " ") p <- p + facet_wrap(formulize("", facet))
  if(scale_x != " ") p <- p + (match.fun(scale_x))()

  p
}

#################################################################################################################

do_the_survplot <- function(time, event, x, dat)
{
  validate(
    need(!is.null(time) && time != " ", "Please select a time-to-event."),
    need(!non_num(dat[[time]]), "Time variable is not numeric!"),
    need(is.null(event) || event == " " || all(dat[[event]] %in% c(NA, TRUE, FALSE)) ||
           all(dat[[event]] %in% c(NA, 0:1)) || all(dat[[event]] %in% c(NA, 1:2)),
         "Make sure event variable is T/F, 0/1, or 1/2"),
    need(is.null(x) || length(x) == 0 || x == " " || count_unique(x, dat) <= 20,
         "This tab only supports x-variables with <= 20 unique levels.")
  )

  lhs <- paste0("Surv(", time, if(!is.null(event) && event != " ") paste0(", ", event), ")")

  x <- x[x != " "]

  if(is.null(x) || length(x) == 0)
  {
    rhs <- "1"
  } else if(is.numeric(dat[[x]]))
  {
    dat[[x]] <- factor(dat[[x]])
    rhs <- x
  } else rhs <- x

  autoplot(survfit(formulize(lhs, rhs), data = dat))
}

#################################################################################################################

documentation <- "R/documentation.md" %>%
  readLines() %>%
  gsub("`(.*?)`", "<code>\\1</code>", x = .) %>%
  gsub("^## (.*)", "<h2>\\1</h2>", x = .) %>%
  gsub("^([^< ].*)", "<p>\\1</p>", x = .) %>%
  gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", x = .) %>%
  paste0(collapse = "")

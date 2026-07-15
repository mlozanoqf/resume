#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

usage <- paste(
  "Usage:",
  "  Rscript scripts/verify_cv_content.R write <html> <manifest>",
  "  Rscript scripts/verify_cv_content.R check <html> <manifest>",
  sep = "\n"
)

if (length(args) != 3 || !args[[1]] %in% c("write", "check")) {
  stop(usage, call. = FALSE)
}

if (!requireNamespace("xml2", quietly = TRUE) ||
    !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Packages 'xml2' and 'jsonlite' are required.", call. = FALSE)
}

mode <- args[[1]]
html_path <- args[[2]]
manifest_path <- args[[3]]

normalize_text <- function(value) {
  value <- gsub("\\u00a0", " ", value, fixed = TRUE)
  value <- gsub("[[:space:]]+", " ", value)
  trimws(value)
}

node_classes <- function(node) {
  classes <- xml2::xml_attr(node, "class")
  ifelse(is.na(classes), "", classes)
}

drop_presentation_nodes <- function(node) {
  clone_doc <- xml2::read_html(
    paste0("<!doctype html><html><body>", as.character(node), "</body></html>"),
    encoding = "UTF-8"
  )
  clone <- xml2::xml_find_first(clone_doc, "//body/*[1]")
  unwanted <- xml2::xml_find_all(
    clone,
    paste0(
      ".//*[",
      "self::script or self::style or self::svg or ",
      "self::p[starts-with(normalize-space(.), 'This document took ')] or ",
      "contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ') or ",
      "contains(concat(' ', normalize-space(@class), ' '), ' anchorjs-link ') or ",
      "contains(concat(' ', normalize-space(@class), ' '), ' cv-icon ')",
      "]"
    )
  )
  xml2::xml_remove(unwanted)
  clone
}

extract_links <- function(node) {
  links <- xml2::xml_attr(xml2::xml_find_all(node, ".//a[@href]"), "href")
  links <- links[!is.na(links)]
  links <- links[!grepl("^#", links)]
  sort(unique(links))
}

extract_manifest <- function(path) {
  doc <- xml2::read_html(path, encoding = "UTF-8")
  main <- xml2::xml_find_first(doc, "//main[@id='quarto-document-content']")
  if (inherits(main, "xml_missing")) {
    stop("Could not find the Quarto document body in ", path, call. = FALSE)
  }

  sections <- xml2::xml_find_all(
    main,
    "./section[contains(concat(' ', normalize-space(@class), ' '), ' level1 ')]"
  )

  section_data <- lapply(sections, function(section) {
    clean <- drop_presentation_nodes(section)
    heading <- xml2::xml_find_first(clean, "./h1")
    list_items <- xml2::xml_find_all(clean, ".//li")
    paragraphs <- xml2::xml_find_all(clean, ".//p")

    list(
      id = xml2::xml_attr(section, "id"),
      heading = normalize_text(xml2::xml_text(heading)),
      text = normalize_text(xml2::xml_text(clean)),
      list_items = length(list_items),
      paragraphs = length(paragraphs),
      links = extract_links(clean)
    )
  })

  first_section <- if (length(sections)) sections[[1]] else NULL
  pre_section_nodes <- if (is.null(first_section)) {
    xml2::xml_children(main)
  } else {
    children <- xml2::xml_children(main)
    first_index <- which(vapply(children, identical, logical(1), first_section))[[1]]
    if (first_index > 1) children[seq_len(first_index - 1)] else children[0]
  }

  contact_links <- unlist(lapply(pre_section_nodes, extract_links), use.names = FALSE)
  contact_links <- sort(unique(contact_links))
  emails <- sort(unique(contact_links[grepl("^mailto:", contact_links)]))
  profiles <- sort(unique(contact_links[!grepl("^mailto:", contact_links)]))

  list(
    title = normalize_text(xml2::xml_text(xml2::xml_find_first(main, ".//h1[contains(concat(' ', normalize-space(@class), ' '), ' title ')]"))),
    subtitle = normalize_text(xml2::xml_text(xml2::xml_find_first(main, ".//*[contains(concat(' ', normalize-space(@class), ' '), ' subtitle ')]"))),
    contact_emails = emails,
    profile_links = profiles,
    section_count = length(section_data),
    sections = section_data
  )
}

current <- extract_manifest(html_path)

if (mode == "write") {
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    current,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  message("Wrote CV content manifest: ", manifest_path)
  quit(status = 0)
}

if (!file.exists(manifest_path)) {
  stop("Manifest does not exist: ", manifest_path, call. = FALSE)
}

current_file <- tempfile("cv-current-", fileext = ".json")
jsonlite::write_json(
  current,
  current_file,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
expected_raw <- readBin(manifest_path, what = "raw", n = file.info(manifest_path)$size)
current_raw <- readBin(current_file, what = "raw", n = file.info(current_file)$size)

if (!identical(current_raw, expected_raw)) {
  expected_file <- tempfile("cv-expected-", fileext = ".json")
  file.copy(manifest_path, expected_file, overwrite = TRUE)
  message("CV content verification failed.")
  message("Expected manifest: ", expected_file)
  message("Current manifest:  ", current_file)
  quit(status = 1)
}

message(
  "CV content verified: ",
  current$section_count,
  " sections; all protected text, entries, emails, and links match."
)

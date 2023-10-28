#' @title gh_stars
#'
gh_stars <- function(n = 100) {
  res <- gh::gh(
    "/user/starred",
    .limit = n
  )

  parsed_res <- res |>
    purrr::map(
      \(x) tibble::tibble(
        id = x$id,
        name = x$name,
        repo = x$full_name,
        owner = x$owner$login,
        language = x$language,
        updated_at = x$updated_at |>
          lubridate::ymd_hms(),
        description = x$description,
        url = x$html_url,
        n_stargazers = x$stargazers_count,
        archived = x$archived,
        disabled = x$disabled
      )
    ) |>
    dplyr::bind_rows()

  output <- parsed_res |>
    dplyr::filter(
      !archived,
      !disabled
    ) |>
    dplyr::select(
      -id,
      -repo,
      -archived,
      -disabled
    )

  return(output)
}

#' @title gh_starred_r_packages
#' 
gh_starred_r_packages <- function(n) {
  gh_stars(n) |>
    dplyr::filter(
      language == "R"
    )
}

#' @title gh_description_file
#' 
gh_description_file <- function(owner, name) {
  res <- gh::gh(
    "/repos/:owner/:name/contents/:file",
    owner = owner,
    name = name,
    file = "DESCRIPTION"
  )

  content <- res$content |>
    base64enc::base64decode() |>
    readr::read_lines()

  temp_file <- fs::file_temp()

  readr::write_lines(
    content,
    file = temp_file
  )

  description <- desc::desc(file = temp_file)

  fs::file_delete(temp_file)

  return(description)
}
#' Update Review Records
#'
#' Updates the review records data frame when a datatable checkbox is clicked.
#'
#' @param review_records The review records data frame to update.
#' @param review_selection The review selection data frame input from the
#'   datatable.
#' @param active_data The active review data frame.
#'
#' @details Three main steps are performed: UPSERT, SUBSET, and ANTI-JOIN The
#'   UPSERT takes the review selection data frame and upserts it into the review
#'   records data frame. (An upsert will insert a record if the unique
#'   identifier is not yet present and update a record based on the unique
#'   identifier if it already exists.) The SUBSET step removes an empty reviews
#'   (partially review rows) and any records not part of the active review (as a
#'   precautionary measure). The ANTI-JOIN step removes any records that match
#'   the active review (records that will not be changing review status based on
#'   user inputs).
#' 
#' @noRd
update_review_records <- function(review_records, review_selection, active_data) {
  if (is.null(review_records))
    review_records <- data.frame(id = integer(), reviewed = character())
  review_records |> 
    dplyr::rows_upsert(
      review_selection,
      by = "id"
    ) |> 
    # Remove empty reviews and inactive data IDs
    subset(!is.na(reviewed) | !id %in% active_data$id) |> 
    # Only update records where the review status is being changed
    dplyr::anti_join(
      active_data,
      by = c("id", "reviewed")
    ) |> 
    dplyr::arrange(id)
}

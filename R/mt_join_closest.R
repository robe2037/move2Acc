#' Join values onto a move2 object by closest timestamp and ID
#'
#' @description
#' Joins a `move2` object with a data.frame, matching on track ID and timestamp.
#' Values can be matched with the closest timestamp before each given
#' timestamp in the input `move2`, the closest timestamp after each given
#' timestamp in the input `move2`, or by an exact match (a standard join).
#' 
#' The join will always use the [move2::mt_track_id_column()] and
#' [move2::mt_time_column()] of the input `move2` as the track ID and timestamp,
#' respectively. To join on different columns, set them beforehand with
#' [move2::mt_set_track_id_column()] and/or [move2::mt_set_time_column()].
#'
#' @param x A `move2` object.
#' @param y A data.frame containing columns with track ID and timestamp
#'   information.
#' @param ... Additional expressions passed to [dplyr::join_by()].
#' @param method How to match timestamps during the join. One of:
#'   - `"closest_before"` (default): for each record in `x`, match to the record in
#'     `y` whose timestamp is closest to but not later than the given timestamp in `x`.
#'   - `"closest_after"`: for each record in `x`, match to the record in `y` whose
#'     timestamp is closest to but not earlier than the given timestamp in `x`.
#'   - `"exact"`: match records by exact timestamp.
#' @param track_id_column_y Name of the column containing the track IDs in `y`.
#'   Defaults to the [move2::mt_track_id_column()] of `x`.
#' @param time_column_y Name of the column containing the timestamps in `y`.
#'   Defaults to the [move2::mt_time_column()] of `x`.
#' @param suffix If there are non-joined duplicate variables in x and y, these 
#'   suffixes will be added to the output to disambiguate them. Should be a 
#'   character vector of length 2. 
#'   
#'   Note that if the first element of the suffix
#'   is not `""`, the timestamp column name of `x` may be changed during the 
#'   join, in which case an `sf` object will be returned, not a `move2` object.
#' 
#' @returns `x` with additional columns from `y` attached
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(move2)
#' library(dplyr)
#' 
#' alb <- albatrosses()
#' 
#' vedbas <- alb |> 
#'   mutate(v = vedba(as_acc(alb, drop = FALSE))) |> 
#'   select(v) |> 
#'   sf::st_drop_geometry() |> 
#'   filter(!is.na(v))
#'
#' alb_gps <- alb[!sf::st_is_empty(alb), ]
#' 
#' # Match based on closest timestamp in y prior to each timestamp in x
#' alb_joined <- mt_join_closest(alb_gps, vedbas)
#' 
#' alb_joined |> select(v, timestamp.y)
#' 
#' # Match based on closest timestamp in y following each timestamp in x
#' alb_joined <- mt_join_closest(alb_gps, vedbas, method = "closest_after")
#' 
#' alb_joined |> select(v, timestamp.y)
#' 
#' # Provide additional conditions to match records on, for instance to match
#' # closest prior timestamp only for GPS records (with sensor type 653):
#' vedbas$sensor <- 653
#' alb$sensor_type_id <- as.numeric(alb$sensor_type_id)
#' 
#' mt_join_closest(alb, vedbas, sensor_type_id == sensor) |>
#'   select(v, timestamp.y, sensor_type_id)
#' }
mt_join_closest <- function(x,
                            y,
                            ...,
                            method = "closest_before",
                            track_id_column_y = NULL,
                            time_column_y = NULL,
                            suffix = c("", ".y")) {
  time_column_x <- move2::mt_time_column(x)
  track_id_column_x <- move2::mt_track_id_column(x)
  track_id_column_y <- track_id_column_y %||% track_id_column_x
  time_column_y <- time_column_y %||% time_column_x
  
  method <- rlang::arg_match(method, c("closest_before", "closest_after", "exact"))
  
  track_x_sym <- rlang::sym(track_id_column_x)
  track_y_sym <- rlang::sym(track_id_column_y)
  time_x_sym <- rlang::sym(time_column_x)
  time_y_sym <- rlang::sym(time_column_y)
  
  temporal <- switch(
    method,
    closest_before = rlang::expr(closest(!!time_x_sym >= !!time_y_sym)),
    closest_after = rlang::expr(closest(!!time_x_sym <= !!time_y_sym)),
    exact = rlang::expr(!!time_x_sym == !!time_y_sym)
  )
  
  dots <- rlang::enexprs(...)
  
  join_spec <- rlang::inject(
    dplyr::join_by(
      !!track_x_sym == !!track_y_sym,
      !!temporal,
      !!!dots
    )
  )
  
  dplyr::left_join(x, y, by = join_spec, suffix = suffix)
}

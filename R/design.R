
distance_from_ideal <- function(round, segment_id, x_coordinate, y_coordinate) {
  data.table(round = round, segment_id = segment_id) %>%
    merge(ideal_position,
          by = c("round","segment_id"),
          all.x = TRUE) %>%
    .[j = sqrt((performance - x_coordinate)^2 + (size - y_coordinate)^2)]
}

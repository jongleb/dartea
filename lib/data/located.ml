type position = { row : int; col : int }
type region = { start_pos : position; end_pos : position }
type 'a t = { region : region; thing : 'a }

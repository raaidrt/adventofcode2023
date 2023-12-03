type day = int
type version = int

type mode = Practice of day * version | Final of day

val get_lines : mode -> string list
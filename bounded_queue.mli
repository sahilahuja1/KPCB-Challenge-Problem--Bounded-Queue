module type QUEUE =
  sig
    type 'a queue
    val construct : int -> int queue
    val enqueue : int -> int queue -> unit
    val dequeue : int queue -> int
    val length : int queue -> int
  end
module Queue : QUEUE
val test : unit -> bool

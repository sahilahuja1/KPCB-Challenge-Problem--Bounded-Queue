;; open Assert

module type QUEUE = sig
  
  type 'a queue

  val construct : int -> int queue

  val enqueue : int -> int queue -> unit

  val dequeue : int queue -> int
  
  val length : int queue -> int
  
end


module Queue : QUEUE = struct

  type 'a qnode = {
    v : 'a;
    mutable next : 'a qnode option;
  }
  
  type 'a queue = {
    mutable head : 'a qnode option;
    mutable tail : 'a qnode option;
    size : int;
  }
  
  let construct (length : int) = {
    head = None;
    tail = None;
    size = length;
  }
    
  let length (q: int queue) : int =
    let rec loop (qn : 'a qnode option) (len:int) : int = 
      begin match qn with
      | None -> len
      | Some n -> loop n.next (1 + len)
      end
    in
    loop q.head 0

  let enqueue (x : int) (q : int queue) : unit =
    if length q < q.size then
      (let newnode = Some { v = x; next = None } in
        begin match q.tail with
        | None -> 
           q.head <- newnode
        | Some n -> 
           n.next <- newnode
        end;
      q.tail <- newnode)
    
  let dequeue (q : int queue) : int =
    begin match q.head with
    | None -> failwith "no element"
    | Some n -> 
       q.head <- n.next;
       if q.head = None then q.tail <- None;
       n.v   
    end
 
    
(************************************)
(***********TEST CASES***************)
(************************************)

  let test () : bool = 
    let q : int queue = construct (5) in 
    length q = 0
;; run_test "length of empty q" test

  let test () : bool = 
    let q : int queue = construct(3) in 
    enqueue 1 q;
    enqueue 2 q;
    enqueue 3 q;
    enqueue 4 q;
    length q = 3
;; run_test "length of 3 element q" test

end

;; open Queue

let test () : bool =
  let q1 = construct 1 in
  let q2 = construct 1 in
  not (q1 == q2)
;; run_test "construct distinct queues" test

let test () : bool =
  let q : int queue = construct 3 in
  enqueue 1 q;
  enqueue 2 q;
  1 = dequeue q
;; run_test "deq first" test

let test () : bool =
  let q : int queue = construct 3 in
  enqueue 1 q;
  enqueue 2 q;
  let _ = dequeue q in
  2 = dequeue q
;; run_test "deq second" test

let test () : bool =
  let q : int queue = construct 3 in
  enqueue 1 q;
  let _ = dequeue q in
  enqueue 2 q;
  2 = dequeue q
;; run_test "enq after deq" test

let test () : bool =
  let q : int queue = construct 4 in
  let _ = dequeue q in
  false
;; run_failing_test "deq of empty should fail" test
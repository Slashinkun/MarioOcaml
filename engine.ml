open Format

module String = struct
  include String
  (** Some compatibility functions*)

  let fold_left f a s =
    let n = String.length s in
    let rec aux i acc =
      if i = n then acc else aux (i + 1) (f acc (String.get s i))
    in
    aux 0 a

  let exists p s =
    let n = String.length s in
    let rec aux i =
      if i = n then false else if p (String.get s i) then true else aux (i + 1)
    in
    aux 0
end

let freq = 30
let width_full = 150
let width = float (width_full - 3)
let height_full = 40
let height = float (height_full - 2)

type key =
  | LeftArrow
  | RightArrow
  | UpArrow
  | DownArrow
  | Char of char
  | Special of string

type key_mod = (key * bool * bool) option

let string_of_mod s c k =
  (if s then "Shift " else "") ^ (if c then "Ctrl " else "") ^ k

let (string_of_key : key_mod -> string) = function
  | Some (LeftArrow, s, c) -> string_of_mod s c "LA"
  | Some (RightArrow, s, c) -> string_of_mod s c "RA"
  | Some (UpArrow, s, c) -> string_of_mod s c "UA"
  | Some (DownArrow, s, c) -> string_of_mod s c "DA"
  | Some (Char x, s, c) -> string_of_mod s c (Char.escaped x)
  | Some (Special st, s, c) -> string_of_mod s c (sprintf "Special(%s)" st)
  | None -> ""

let key_of_string = function
  | "Shift" -> Some (Special "Shift", true, false)
  | "Control" -> Some (Special "Control", false, true)
  | "Escape" -> exit 1
  | s ->
      let n = String.length s in
      let aux i =
        if i = n then (None : key_mod)
        else
          Some
            (match
               ( s.[i],
                 (if i < n - 1 then Some s.[i + 1] else None),
                 (if i < n - 2 then Some s.[i + 2] else None),
                 if
                   i < n - 5
                   && s.[i + 3] = ';'
                   && (s.[i + 4] = '2' || s.[i + 4] = '5')
                 then
                   Some
                     ( (if s.[i + 4] = '2' then (true, false) else (false, true)),
                       s.[i + 5] )
                 else None )
             with
            | x, _, _, _ when x >= 'A' && x <= 'Z' ->
                (Char (Char.lowercase_ascii x), true, false)
            | '\027', Some '[', Some 'A', _ -> (UpArrow, false, false)
            | '\027', Some '[', Some 'B', _ -> (DownArrow, false, false)
            | '\027', Some '[', Some 'C', _ -> (RightArrow, false, false)
            | '\027', Some '[', Some 'D', _ -> (LeftArrow, false, false)
            | '\027', Some '[', Some '1', Some ((s, c), 'A') -> (UpArrow, s, c)
            | '\027', Some '[', Some '1', Some ((s, c), 'B') -> (DownArrow, s, c)
            | '\027', Some '[', Some '1', Some ((s, c), 'C') ->
                (RightArrow, s, c)
            | '\027', Some '[', Some '1', Some ((s, c), 'D') -> (LeftArrow, s, c)
            | x, _, _, _ when int_of_char x >= 0 && int_of_char x < 32 ->
                (Char (char_of_int (int_of_char x + 96)), false, true)
            | x, _, _, _ when x >= ' ' && int_of_char x <= 127 ->
                (Char x, false, false)
            | _ -> (Special (String.sub s i (n - i)), false, false))
      in
      aux 0

let set_cursor x y = printf "\027[%i;%iH%!" y x
let convert_y y = min height_full (max 0 (height_full - y))
let place_string x y s = printf "\027[%d;%dH%s\028" (convert_y y) x s

let get_uchar_length s i =
  let v =
    match int_of_char s.[i] with
    | x when x land 0b10000000 = 0 -> 1
    | x when x land 0b11100000 = 0b11000000 -> 2
    | x when x land 0b11110000 = 0b11100000 -> 3
    | x when x land 0b11111000 = 0b11110000 -> 4
    | _ -> failwith "Invalid Utf8 encoding"
  in
  (*let oracle = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
    assert (oracle = v);*)
  v

let get_uchar s i =
  let n = get_uchar_length s i in
  String.sub s i n

let string_unicode_fold_left f a s =
  let n = String.length s in
  let rec aux i acc =
    if i = n then acc
    else aux (i + get_uchar_length s i) (f acc (get_uchar s i))
  in
  aux 0 a

let rec clip n x s xlow index_low index_up =
  if index_up >= n || x >= width_full - 3 then (xlow, index_low, index_up)
  else if x < 0 then
    let index_low2 = index_low + get_uchar_length s index_low in
    clip n (x + 1) s (xlow + 1) index_low2 index_low2
  else
    let index_up2 = index_up + get_uchar_length s index_up in
    clip n (x + 1) s xlow index_low index_up2

let place_clip_string x y s =
  if y >= 0 && y < height_full - 2 then
    if x >= 0 && x + String.length s < width_full - 3 then
      printf "\027[%d;%dH%s\028" (height_full - y - 1) (x + 2) s
    else
      let x2, low, up = clip (String.length s) x s x 0 0 in
      let s2 = String.sub s low (up - low) in
      printf "\027[%d;%dH%s\028" (height_full - y - 1) (x2 + 2) s2

let place_sprite x y s =
  let rec aux i l =
    match String.index_from_opt s i '\n' with
    | None -> place_clip_string x (y - l) (String.sub s i (String.length s - i))
    | Some j ->
        place_clip_string x (y - l) (String.sub s i (j - i));
        aux (j + 1) (l + 1)
  in
  aux 0 0

type vec = float * float

let gravity : vec = (0.0, -100.0)
let ( +.. ) ((xa, ya) : vec) ((xb, yb) : vec) : vec = (xa +. xb, ya +. yb)
let ( -.. ) ((xa, ya) : vec) ((xb, yb) : vec) : vec = (xa -. xb, ya -. yb)
let ( *.. ) k ((x, y) : vec) : vec = (k *. x, k *. y)

let update_xminmax mm s =
  let n = String.length s in
  let rec aux index count (xmin, xmax) =
    if index >= n then (xmin, xmax)
    else
      let mm2 =
        if s.[index] <> ' ' then (min count xmin, max count xmax)
        else (xmin, xmax)
      in
      aux (index + get_uchar_length s index) (count + 1) mm2
  in
  aux 0 0 mm

let update_minmax xmm y (ymin, ymax) s =
  let xmm2 = update_xminmax xmm s in
  let ymm2 =
    if String.exists (fun c -> c <> ' ') s then (min y ymin, max y ymax)
    else (ymin, ymax)
  in
  (xmm2, ymm2)

let bound ((x, y), s) =
  let rec aux i l xmm ymm =
    match String.index_from_opt s i '\n' with
    | None ->
        let s2 = String.sub s i (String.length s - i) in
        let (xmin, xmax), (ymin, ymax) = update_minmax xmm l ymm s2 in
        ((x +. float xmin, 1 + xmax), (y +. float (l - ymax), 1 + l - ymin))
    | Some j ->
        let s2 = String.sub s i (j - i) in
        let xmm2, ymm2 = update_minmax xmm l ymm s2 in
        aux (j + 1) (l + 1) xmm2 ymm2
  in
  let (*((xmin, xmax), (ymin, ymax)) as*) b =
    aux 0 0 (max_int, min_int) (max_int, min_int)
  in
  (*place_string 200 20
    (sprintf "bound '%s' (%g;%i);(%g,%i)\n" (String.escaped s) xmin xmax ymin
       ymax);*)
  b

let get_bounds s =
  let (_, a), (_, b) = bound ((0.0, 0.0), s) in
  (float a, float b)

let inter_1bound (x1min, x1_length) (x2min, x2_length) =
  not (x1min +. float x1_length <= x2min || x2min +. float x2_length <= x1min)

let inter_bound (b1x, b1y) (b2x, b2y) =
  inter_1bound b1x b2x && inter_1bound b1y b2y

let intersect_obstacles b l = List.filter (fun (_, _, b2) -> inter_bound b b2) l

let get_obst b l =
  match intersect_obstacles b l with
  | [] -> None
  | (a, b, c) :: _ -> Some (a, b, c)

let proj op_min np_length (obst_min, obst_length) =
  if op_min < obst_min then obst_min -. float (np_length + 0)
  else obst_min +. float (obst_length + 0)

type solid = vec * string

let is_above o1 o2 =
  let _, (y1, _) = bound o1 in
  let _, (y2, h2) = bound o2 in
  y1 >= y2 +. float h2

let is_below o1 o2 =
  let _, (y1, h1) = bound o1 in
  let _, (y2, _) = bound o2 in
  y1 +. float h1 <= y2

let is_left o1 o2 =
  let (x1, w1), _ = bound o1 in
  let (x2, _), _ = bound o2 in
  x1 +. float w1 <= x2

let is_right o1 o2 =
  let (x1, _), _ = bound o1 in
  let (x2, w2), _ = bound o2 in
  x1 >= x2 +. float w2

let collision o1 o2 = inter_bound (bound o1) (bound o2)
let mk_obst = function None -> None | Some (a, b, _) -> Some (a, b)

let discx (opx, opy) ((npx, width), (npy, height)) (vx, vy) l =
  (* On test les obstacle verticaux on projete si intersection *)
  let v_obst = get_obst ((opx, width), (npy, height)) l in
  let npy2, vy2 =
    match v_obst with
    | Some (_, _, (_, bobst)) ->
        let npy2 = proj opy height bobst in
        (npy2, 0.0)
    | None -> (npy, vy)
  in
  (* On test les obstacle horizontaux on projete si intersection *)
  let h_obst = get_obst ((npx, width), (npy2, height)) l in
  let npx2, vx2 =
    match h_obst with
    | Some (_, _, (bobst, _)) ->
        let npx2 = proj opx width bobst in
        (npx2, 0.0)
    | None -> (npx, vx)
  in

  (((npx2, npy2) : vec), ((vx2, vy2) : vec), (mk_obst h_obst, mk_obst v_obst))

let update_physics (pos, s) v fl o_list =
  let dt = 1.0 /. float freq in
  let sumf = List.fold_left ( +.. ) (0.0, 0.0) fl in
  let v2 = v +.. (dt *.. sumf) in
  let pos2 = pos +.. (dt *.. v2) in
  let obj_bound = bound (pos2, s) in
  let cmp_bound = List.map (fun (p, s2) -> (p, s2, bound (p, s2))) o_list in
  let int_obst = intersect_obstacles obj_bound cmp_bound in
  (*List.iter
    (fun ((x, y), s, _) ->
      eprintf "obst: (%g,%g) '%s'\n" x y (String.escaped s);
      place_sprite 220 10 s)
    int_obst;*)
  discx pos obj_bound v2 int_obst

let draw_sprite ((x, y), s) =
  let h =
    String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 s
  in
  place_sprite (int_of_float x) (h + int_of_float y) s

let print_border () =
  let soi i = String.make 1 (char_of_int i) in
  printf "\027(0";
  let head = String.make (width_full - 3) (char_of_int 0x71) in
  place_string 0 0 (soi 0x6d);
  place_string (width_full - 1) 0 (soi 0x6a);
  place_string (width_full - 1) (height_full - 1) (soi 0x6b);
  place_string 0 (height_full - 1) (soi 0x6c);
  place_string 2 0 head;
  place_string 2 (height_full - 1) head;
  let rec aux i =
    if i > 0 then (
      place_string 0 i (soi 0x78);
      place_string (width_full - 1) i (soi 0x78);
      aux (i - 1))
  in
  aux (height_full - 2);
  printf "\027(B"

let wait x = ignore @@ Unix.select [] [] [] x

let read_imdt () =
  let l, _, _ = Unix.select [ Unix.descr_of_in_channel stdin ] [] [] 0.0 in
  match l with
  | [] -> (None, "")
  | _ ->
      let buff = Bytes.create 512 in
      let c = input stdin buff 0 512 in
      if c = 0 then exit 1
      else
        let s = Bytes.sub_string buff 0 c in
        if s = "\027" || s = "\027\027" then exit 1 else (key_of_string s, s)

let init () =
  printf "\027[?25l";
  let param = Unix.tcgetattr (Unix.descr_of_in_channel stdin) in
  let param2 =
    { param with c_echo = false; c_icanon = false; c_isig = false }
  in
  Unix.tcsetattr (Unix.descr_of_in_channel stdin) Unix.TCSANOW param2;
  ignore @@ Sys.command ("xset r rate 1 " ^ string_of_int freq);
  at_exit (fun () ->
      ignore @@ Sys.command "xset r rate 200 30";
      printf "\027[?25h";
      Unix.tcsetattr (Unix.descr_of_in_channel stdin) Unix.TCSANOW param)

let clear_screen () =
  set_cursor 0 0;
  printf "\027[1;1H\027[2J";
  print_border ()

let loop' init_state f =
  init ();
  let step = 1. /. float freq in
  let rec aux s f2 =
    let t1 = Unix.gettimeofday () in
    let c, sk = read_imdt () in
    clear_screen ();
    place_string 5 height_full
      (sprintf "key: '%s' -> [%s]" (String.escaped sk) (string_of_key c));
    place_string (width_full - 15) height_full (sprintf "fps: %g" (1.0 /. f2));
    let draws, state = f s c in
    List.iter draw_sprite draws;
    flush stdout;
    let t2 = Unix.gettimeofday () in
    wait (step -. t2 +. t1);
    aux state (Unix.gettimeofday () -. t1)
  in
  aux init_state 1.0

let loop init_state update affiche =
  loop' init_state (fun state keys ->
      let state2 = update state keys in
      (affiche state2, state2))

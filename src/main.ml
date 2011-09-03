open Yojson.Safe

let parse_json j =
    match j with
    | `String s -> s
    | `Int i -> string_of_int i
    | `Float f -> string_of_float f
    | _ -> ""

let parse_url s =
    try
        let i = String.index_from s 7 '/' in
        (String.sub s 0 i), (String.sub s i ((String.length s) - i))
    with Not_found -> s, ""

let play_note st n =
    match n with
    | `Assoc d ->
            let url = parse_json (List.assoc "u" d) in
            let site, page = parse_url url in
            let ua = parse_json (List.assoc "a" d) in
            let inst = (Hashtbl.hash site) mod 100 in
            let key = (Hashtbl.hash page) mod 128 in
            let time = 50 + ((Hashtbl.hash ua) mod 100) in
            Portmidi.write_short st Int32.zero (Portmidi.message 0xC0 inst 0);
            Portmidi.write_short st Int32.zero (Portmidi.message 0x90 key 100);
            Portmidi.Time.sleep time;
            Portmidi.write_short st Int32.zero (Portmidi.message 0x80 key 0);
            (site, page, time)
    | _ -> ("", "", -1)

let write_note track ttime n =
    match n with
    | `Assoc d ->
            let url = List.assoc "u" d in
            let ua = List.assoc "a" d in
            let key = (Hashtbl.hash url) mod 128 in
            let time = (Hashtbl.hash ua) mod 100 in
            let event = Smf.event_new_from_bytes 0x90 100 (-1) in
            Smf.track_add_event_seconds track event (ttime + time);
            let event = Smf.event_new_from_bytes 0x80 0 (-1) in
            Smf.track_add_event_seconds track event (ttime + (2 * time));
            ttime + (2 * time)
    | _ -> ttime

type mode = Input | Output

let get_device m =
    print_endline "Select device:";
    let print_device i = function
        | Input, {Portmidi.input=true; Portmidi.name=n}
        | Output, {Portmidi.output=true; Portmidi.name=n} ->
                print_int i;
                print_endline (" " ^ n);
        | _ -> ()
    in
    let count = Portmidi.count_devices () in
    for i = 0 to count - 1 do
        let info = Portmidi.get_device_info i in
        print_device i (m, info);
        ()
    done;
    Pervasives.read_int ()

let play _ = 
    Portmidi.init ();
    Portmidi.Time.start 1;
    let d = get_device Output in
    let st = Portmidi.open_output d 8 0 in
    let raw_json = open_in "bitly.json" in
    while true do
        try
            let l = input_line raw_json in
            try
                let json = from_string l in
                ignore (play_note st json)
            with _ -> ()
        with End_of_file -> exit 0
    done

let write _ = 
    let smf = Smf.smf_new () in
    let track = Smf.track_new () in
    Smf.add_track smf track;
    let raw_json = open_in "bitly.json" in
    let time = ref 0 in
    while true do
        try
            let l = input_line raw_json in
            try
                let json = from_string l in
                time := write_note track (!time) json
            with _ -> ()
        with End_of_file -> ignore (Smf.save smf "bitly.mid"); exit 0
    done

let curlplay st str = 
    begin
    try
        let json = from_string str in
        match play_note st json with
        | ("", "", -1) -> ()
        | (i, k, t) ->
                let str = "Instrument: " ^ i ^ " Key: " ^
                k ^ " Time: " ^ (string_of_int t) ^ "\n" in
                ignore (Unix.write Unix.stdout str 0 (String.length str))
    with _ -> ()
    end;
    String.length str

let curlstuff _ =
    Portmidi.init ();
    Portmidi.Time.start 1;
    let d = get_device Output in
    let st = Portmidi.open_output d 8 0 in
    Portmidi.write_short st Int32.zero (Portmidi.message 0xC0 40 0);

    Curl.global_init Curl.CURLINIT_GLOBALNOTHING;
    let conn = Curl.init () in
    Curl.set_url conn "http://developer.usa.gov/1usagov";
    Curl.set_writefunction conn (curlplay st);
    Curl.perform conn;
    Curl.cleanup conn;
    Curl.global_cleanup ();
    ()

let _ = curlstuff ()

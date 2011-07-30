open Yojson.Safe

let get_list file =
    let json = from_file file in
    match json with
    | `Assoc d ->
            begin
                match List.assoc "data" d with
                | `List l -> l
                | _ -> raise Not_found
            end
    | _ -> raise Not_found

let play_note st n =
    match n with
    | `Assoc d ->
            let url = List.assoc "u" d in
            let ua = List.assoc "a" d in
            let key = (Hashtbl.hash url) mod 128 in
            let time = (Hashtbl.hash ua) mod 100 in
            Portmidi.write_short st Int32.zero (Portmidi.message 0x90 key 100);
            Portmidi.Time.sleep time;
            Portmidi.write_short st Int32.zero (Portmidi.message 0x80 key 0)
    | _ -> ()

let write_note track ttime n =
    match n with
    | `Assoc d ->
            let url = List.assoc "u" d in
            let ua = List.assoc "a" d in
            let key = (Hashtbl.hash url) mod 128 in
            let time = (Hashtbl.hash ua) mod 100 in
            let ba = Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout [|0x90; key; 100|] in
            let event = Smf.event_new_from_pointer ba 3 in
            Smf.track_add_event_seconds track event (ttime + time);
            Bigarray.Array1.set ba 0 0x80;
            Bigarray.Array1.set ba 2 0;
            let event = Smf.event_new_from_pointer ba 3 in
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
                play_note st json
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

let _ = write ()

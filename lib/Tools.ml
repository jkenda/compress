let time f_name f arg =
    let t0 = Sys.time () in
    let result = f arg in
    let t1 = Sys.time () in
    let open Format in
    let _ = printf "%.3f ms <- %s\n" ((t1 -. t0) *. 1000.) f_name in
    result


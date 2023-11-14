open System

//тип событий
type State = 
    | CLOSED
    | LISTEN
    | SYN_SENT
    | SYN_RCVD
    | ESTABLISHED
    | CLOSE_WAIT
    | LAST_ACK
    | FIN_WAIT_1
    | FIN_WAIT_2
    | CLOSING
    | TIME_WAIT

//тип событий
type Event = //union тип событий
    | APP_PASSIVE_OPEN
    | APP_ACTIVE_OPEN
    | APP_SEND
    | APP_CLOSE
    | APP_TIMEOUT
    | RCV_SYN
    | RCV_ACK
    | RCV_SYN_ACK
    | RCV_FIN
    | RCV_FIN_ACK

//таблица переходов
let transitionTable =
    [
        (CLOSED, APP_PASSIVE_OPEN, LISTEN);
        (CLOSED, APP_ACTIVE_OPEN, SYN_SENT);
        (LISTEN, RCV_SYN, SYN_RCVD);
        (LISTEN, APP_SEND, SYN_SENT);
        (LISTEN, APP_CLOSE, CLOSED);
        (SYN_RCVD, APP_CLOSE, FIN_WAIT_1);
        (SYN_RCVD, RCV_ACK, ESTABLISHED);
        (SYN_SENT, RCV_SYN, SYN_RCVD);
        (SYN_SENT, RCV_SYN_ACK, ESTABLISHED);
        (SYN_SENT, APP_CLOSE, CLOSED);
        (ESTABLISHED, APP_CLOSE, FIN_WAIT_1);
        (ESTABLISHED, RCV_FIN, CLOSE_WAIT);
        (FIN_WAIT_1, RCV_FIN, CLOSING);
        (FIN_WAIT_1, RCV_FIN_ACK, TIME_WAIT);
        (FIN_WAIT_1, RCV_ACK, FIN_WAIT_2);
        (CLOSING, RCV_ACK, TIME_WAIT);
        (FIN_WAIT_2, RCV_FIN, TIME_WAIT);
        (TIME_WAIT, APP_TIMEOUT, CLOSED);
        (CLOSE_WAIT, APP_CLOSE, LAST_ACK);
        (LAST_ACK, RCV_ACK, CLOSED);
    ]

let processEvent currentState event =
    match List.tryFind (fun (s, e, nextState) -> s = currentState && e = event) transitionTable with
    | Some (_, _, nextState) -> nextState
    | None -> failwith "Unexpected event"

let processEvents initialState events =
    List.fold (fun state event -> processEvent state event) initialState events

let printResult result =
    printfn "Final State: %A" result

let eventFromString s =
    match s with
    | "APP_PASSIVE_OPEN" -> APP_PASSIVE_OPEN
    | "APP_ACTIVE_OPEN" -> APP_ACTIVE_OPEN
    | "APP_SEND" -> APP_SEND
    | "APP_CLOSE" -> APP_CLOSE
    | "APP_TIMEOUT" -> APP_TIMEOUT
    | "RCV_SYN" -> RCV_SYN
    | "RCV_ACK" -> RCV_ACK
    | "RCV_SYN_ACK" -> RCV_SYN_ACK
    | "RCV_FIN" -> RCV_FIN
    | "RCV_FIN_ACK" -> RCV_FIN_ACK
    | _ -> failwith "Invalid event"

let readEvents () =
    printf "Enter events (comma-separated): "
    System.Console.ReadLine().Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim() |> eventFromString) |> List.ofArray

let main () =
    try
        let initialState = CLOSED
        let events = readEvents ()
        let finalState = processEvents initialState events
        printResult finalState
    with
    | ex -> printResult $"ERROR ({ex.Message})" 

main ()
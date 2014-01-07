module EventCreators

type Move =
    | Rock
    | Paper
    | Scissors

type ValidActions =
    | Create = 1
    | PlayerOneMove = 2
    | PlayerTwoMove = 4
    | EitherPlayerMove = 6
    | None = 8

type GameState = {
    creator:string
    opponent:string
    creatorMoves:List<Move>
    opponentMoves:List<Move>
    nextValidAction:ValidActions
}
    
type CreateGame = {
    creator:string
}
type GameCreated={
    creator:string
    id:System.Guid
}
type MakeMove ={
    move:Move
    player:string
    id:System.Guid
}
type MoveMade ={
    move:Move
    player:string
}
type PlayerJoined={player:string}
type GameWon={
    winner:string
    loser:string
}
type GameTied={
    creator:string
    opponent:string
}
let FlagIsSet(source,flag)= 
    source &&& flag = flag

let DefaultIfUnable(state,action,command,applier)=
    match FlagIsSet(state.nextValidAction,action) with
    | true -> []
    | _ -> applier()

let CreateGame (command:CreateGame,state)=
    DefaultIfUnable(state,ValidActions.Create,command,
        fun ()->[({id=System.Guid.NewGuid();creator=command.creator}:GameCreated)])

let WhoWillMakeMove (command:MakeMove,state:GameState)=
    match command.player with
    | name when name = state.creator -> ValidActions.PlayerOneMove
    | _ -> ValidActions.PlayerTwoMove
    
let NextAction state player= 
    if state.creatorMoves.Length>=1 || state.opponentMoves.Length>=1
    then ValidActions.None
    else ValidActions.EitherPlayerMove - player

let WhoWon(state:GameState) =
    match state.creatorMoves.Head,state.opponentMoves.Head with
    | Move.Rock,Move.Scissors -> (state.creator,state.opponent)
    | Move.Scissors,Move.Paper -> (state.creator,state.opponent)
    | Move.Paper,Move.Rock -> (state.creator,state.opponent)
    | (x,y) when x=y-> (null,null)
    | _ -> (state.opponent,state.creator)
    
    
let Applier state (ev:obj)=
    match ev with
        | :? GameCreated as e -> {state with nextValidAction=ValidActions.EitherPlayerMove;creator=e.creator}
        | :? MoveMade as e when NextAction state ValidActions.EitherPlayerMove = ValidActions.None ->
            {state with nextValidAction=ValidActions.None}
        | :? MoveMade as e when e.player=state.creator -> {state with nextValidAction=NextAction state ValidActions.PlayerOneMove;creatorMoves=e.move::state.creatorMoves}
        | :? MoveMade as e -> {state with nextValidAction=NextAction state ValidActions.PlayerTwoMove;opponentMoves=e.move::state.opponentMoves;opponent=e.player}
        | _ -> state

let MakeMove (command,state)=
    let action=WhoWillMakeMove(command,state)
    DefaultIfUnable(state,action,command,
        fun () ->
            let result=[box {move=command.move;player=command.player}]
            let wonEvent=(if (NextAction state ValidActions.EitherPlayerMove) = ValidActions.None
                            then 
                                let winner=WhoWon (Applier state result.Head)
                                if fst winner=null then [box {creator=state.creator;opponent=state.opponent}]
                                else [box {winner=fst winner;loser=snd winner}]                                
                            else [])

            if state.opponentMoves.Length=0 && action=ValidActions.PlayerTwoMove 
                then box {player=command.player} :: result @ wonEvent
                else result @ wonEvent)

let RestoreState(events)=
    let initial={creator="";opponent="";creatorMoves=[];opponentMoves=[];nextValidAction=ValidActions.Create}
    List.fold Applier initial events

let fakeId=System.Guid.NewGuid();

let state=RestoreState [({creator="Me!";id=fakeId}),({move=Move.Rock;player="Me!"}:MoveMade)]
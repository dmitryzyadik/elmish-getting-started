module App

open Elmish
open Elmish.React
open Feliz
open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css">

type TodoId = TodoId of System.Guid

type Todo =
    { Id: TodoId
      Description: string
      Complite: bool }

type State = { Todos: Todo list; NewTodo: string }

let keyCodeEnter = 13.0
type Msg =
    | NewTodoChanged of string
    | AddNewTodo
    | DeleteTodo of TodoId
    | Complite of TodoId

let init () =
    { Todos =
          [ { Id = System.Guid.NewGuid() |> TodoId
              Description = "Задание 1"
              Complite = false }
            { Id = System.Guid.NewGuid() |> TodoId
              Description = "Задание 2"
              Complite = false } ]
      NewTodo = "" }

let NewTodoInput (currentNewTodo: string) (dispatch: Msg -> unit) =
    Html.div [ prop.classes [ "field"; "has-addons" ]
               prop.children [ Html.div [ prop.classes [ "control"
                                                         "is-expanded" ]
                                          prop.children [ Html.input [ prop.classes [ "input"; "is-medium" ]
                                                                       prop.onKeyUp (fun ev -> if ev.keyCode = keyCodeEnter then AddNewTodo |> dispatch)
                                                                       prop.valueOrDefault currentNewTodo
                                                                       prop.onTextChange (NewTodoChanged >> dispatch) ] ] ]
                               Html.div [ prop.classes [ "control" ]
                                          prop.children [ Html.button [ prop.classes [ "button"
                                                                                       "is-primary"
                                                                                       "is-medium" ]
                                                                        prop.onClick (fun _ -> AddNewTodo |> dispatch)
                                                                        prop.children [ Html.i [ prop.classes [ "fa"
                                                                                                                "fa-plus" ] ] ] ] ] ] ] ]

let title =
    Html.p [ prop.className "title"
             prop.text "Список заданий" ]


let div (classes: string list) (children: ReactElement list) =
    Html.div [ prop.classes classes
               prop.children children ]

let renderTodo (todo: Todo) dispatch =
    div [ "box" ] [
        div [ "columns"; "is-mobile" ] [
            div [ "column" ] [
                Html.p [ prop.className "subtitle"
                         prop.text todo.Description ]
            ]
            div [ "column"; "is-narrow" ] [
                div ["buttons"][
                Html.button [
                  prop.className ["button"; "is-danger"]
                  prop.onClick (fun _ ->  dispatch (DeleteTodo todo.Id))
                  prop.children [
                    Html.i [prop.classes ["fa"; "fa-times"]
                    ]
                  ]
                ]
                Html.button [
                  prop.className ["button"; "is-success"]
                  prop.onClick (fun _ ->  dispatch (Complite todo.Id))
                  prop.children [
                    Html.i [prop.classes ["fa"; "fa-fire"]
                    ]
                  ]
                ]
                ]
            ]
        ]
    ]

let todoList (todos: Todo list) (dispatch: Msg -> unit) =
    Html.ul [ prop.children [ for todo in todos -> renderTodo todo dispatch] ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [ prop.style [ style.padding 20 ]
               prop.children [ title
                               NewTodoInput state.NewTodo dispatch
                               todoList state.Todos dispatch ] ]

let withNewTodo description state =
    let todo =
        { Id = System.Guid.NewGuid() |> TodoId
          Description = state.NewTodo
          Complite = false }

    { state with
          Todos = List.append state.Todos [ todo ]
          NewTodo = "" }

let withoutTodo todoId state =
  {state with Todos = state.Todos |> List.filter (fun todo -> todo.Id <> todoId )}

let compliteTodo (todoId: TodoId) state =
  {state with Todos = state.Todos |> List.map (fun (todo:Todo) -> if todo.Id = todoId then {todo with Complite = true}  else todo  )}

let update (msg: Msg) (state: State): State =
    match msg with
    | NewTodoChanged description -> { state with NewTodo = description }
    | AddNewTodo -> state |> withNewTodo state.NewTodo
    | DeleteTodo todoId -> state |> withoutTodo todoId
    | Complite todoId -> state |> compliteTodo todoId

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run

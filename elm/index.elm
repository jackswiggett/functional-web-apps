import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array exposing (Array)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Move =
  { id: Int
  , name: String
  , tags: Array String
  , urls: Array String
  }

type alias Model =
  { moves : List Move
  , draft: Move
  , searchInput: String
  }

defaultMove1 : Move
defaultMove1 =
  { id = 0
  , name = "Swingout"
  , tags = Array.fromList [ "8-count", "fundamentals", "open to open" ]
  , urls = Array.fromList
    [ "https://www.youtube.com/watch?v=tNJzu8nOI4g"
    , "https://www.youtube.com/watch?v=l6N_2lxg_Mk"
    , "https://www.youtube.com/watch?v=88xB7bmia4Y"
    ]
  }

defaultMove2 : Move
defaultMove2 =
  { id = 1
  , name = "Tuck Turn"
  , tags = Array.fromList [ "6-count", "fundamentals", "closed to open" ]
  , urls = Array.fromList
    [ "https://www.youtube.com/watch?v=ZlWFfNgsfnw"
    , "https://www.youtube.com/watch?v=U4wp8EORODI"
    , "https://www.youtube.com/watch?v=Q3ew4E_-8Ps"
    ]
  }


init : (Model, Cmd Msg)
init =
  (
    { moves = [ defaultMove1, defaultMove2 ]
    , draft =
      { id = 2
      , name = ""
      , tags = Array.fromList [ "" ]
      , urls = Array.fromList [ "" ]
      }
    , searchInput = ""
    }
    , Cmd.none
  )



-- UPDATE


type Msg
  -- edit draft
  = EditName String
  | EditTagAtIndex Int String
  | DeleteTagAtIndex Int
  | AddTag
  | EditUrlAtIndex Int String
  | DeleteUrlAtIndex Int
  | AddUrl
  -- modify catalog
  | AddDraftToCatalog
  | RemoveFromCatalogById Int
  -- search catalog
  | EditSearchInput String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditName name ->
      let draft = model.draft in
      let newDraft = { draft | name = name } in
      ({ model | draft = newDraft }, Cmd.none)
    EditTagAtIndex index tag ->
      let draft = model.draft in
      let newDraft = { draft | tags = Array.set index tag draft.tags } in
      ({ model | draft = newDraft }, Cmd.none)
    DeleteTagAtIndex index ->
      let draft = model.draft in
      let newDraft =
        ({ draft
        | tags = Array.append
            (Array.slice 0 index draft.tags)
            (Array.slice (index + 1) (Array.length draft.tags) draft.tags)
        }) in
      ({ model | draft = newDraft }, Cmd.none)
    AddTag ->
      let draft = model.draft in
      let newDraft = { draft | tags = Array.push "" draft.tags } in
      ({ model | draft = newDraft}, Cmd.none)
    EditUrlAtIndex index url ->
      let draft = model.draft in
      let newDraft = { draft | urls = Array.set index url draft.urls } in
      ({ model | draft = newDraft }, Cmd.none)
    DeleteUrlAtIndex index ->
      let draft = model.draft in
      let newDraft =
        ({ draft
        | urls = Array.append
            (Array.slice 0 index draft.urls)
            (Array.slice (index + 1) (Array.length draft.urls) draft.urls)
        }) in
      ({ model | draft = newDraft }, Cmd.none)
    AddUrl ->
      let draft = model.draft in
      let newDraft = { draft | urls = Array.push "" draft.urls } in
      ({ model | draft = newDraft}, Cmd.none)
    AddDraftToCatalog ->
      (
        { model
        | moves = model.draft :: model.moves
        , draft =
            { id = model.draft.id + 1
            , name = ""
            , tags = Array.fromList [ "" ]
            , urls = Array.fromList [ "" ]
            }
        },
        Cmd.none
      )
    RemoveFromCatalogById id ->
      let moves = List.filter (\move -> move.id /= id) model.moves
      in ({ model | moves = moves}, Cmd.none)
    EditSearchInput searchInput ->
      ({ model | searchInput = searchInput }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


-- Render the entire page
view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ rel "stylesheet", href "style.css" ] []
    , div [ class "box" ]
      (
        [ div [ class "flex-line" ]
          [ span [ class "input-label" ] [ text "Name:" ]
          , div [ class "input-wrapper" ]
            [ input [ class "input", value model.draft.name, type_ "text", onInput EditName ] []
            ]
          ]
        , div [ class "flex-line" ]
            (
              [ span [ class "input-label" ] [ text "Tags:" ] ]
              ++
              Array.toList (Array.indexedMap viewDraftTag model.draft.tags)
              ++
              [ button [ class "plus-button", onClick AddTag ] [ text "+" ] ]
            )
        , div [ class "flex-line" ]
          [ span [ class "input-label" ] [ text "Video URLs:" ]
          ]
        ]
        ++
        Array.toList (Array.indexedMap viewDraftUrl model.draft.urls)
        ++
        [ div [ class "flex-line" ]
          [ button [ class "plus-button", onClick AddUrl ] [ text "+" ]
          ]
        ]
      )
    , div [ class "add-button-wrapper" ]
      [ button [ class "button", onClick AddDraftToCatalog ] [ text "Add to catalog" ]
      ]
    , div [ class "title-wrapper" ]
      [ span [ class "title" ] [ text "Catalog" ]
      , div [ class "input-wrapper search" ]
        [ input
            [ class "input"
            , placeholder "Search..."
            , value model.searchInput
            , type_ "text"
            , onInput EditSearchInput
            ] []
        ]
      ]
    , div []
      (
        List.map
          viewCatalogMove
          (filterMoves model.searchInput model.moves)
      )
    ]


-- Render a tag input box
viewDraftTag : Int -> String -> Html Msg
viewDraftTag index tag =
  div [ class "input-wrapper tag" ]
    [ input [ class "input", value tag, type_ "text", onInput (EditTagAtIndex index) ] []
    , button [ class "delete-button", onClick (DeleteTagAtIndex index) ] [ text "X" ]
    ]


-- Render a url input box
viewDraftUrl : Int -> String -> Html Msg
viewDraftUrl index url =
  div [ class "flex-line" ]
    [ div [ class "input-wrapper url" ]
      [ input [ class "input", value url, type_ "text", onInput (EditUrlAtIndex index) ] []
      , button [ class "delete-button", onClick (DeleteUrlAtIndex index) ] [ text "X" ]
      ]
    ]



-- Filter moves based on the search input
filterMoves : String -> List Move -> List Move
filterMoves searchInput moves =
  List.filter
    (
      \move ->
        let
          input = String.toLower searchInput
        in
          String.contains input (String.toLower move.name) ||
          List.any
            (\tag -> String.contains input (String.toLower tag))
            (Array.toList move.tags)
    )
    moves


-- Render a single move in the catalog
viewCatalogMove : Move -> Html Msg
viewCatalogMove move =
  div [ class "box catalog-box" ]
    [ div [ class "move-header-wrapper" ]
      [ div [ class "move-title-wrapper" ]
        [ span [ class "move-title" ] [ text move.name ]
        , span [ class "move-bull-sep" ] [ text "•" ]
        , span [ class "move-tags" ] [ text (String.join ", " (Array.toList move.tags)) ]
        ]
      , button [ class "button", onClick (RemoveFromCatalogById move.id) ] [ text "Remove from catalog" ]
      ]
    , ul [ class "url-list" ]
      (Array.toList (Array.map viewUrl move.urls))
    ]


-- Render a single URL list element
viewUrl : String -> Html Msg
viewUrl url =
  li []
    [ a [ href url ] [ text url ]
    ]

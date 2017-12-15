/* Alias this function since we have to use it any time
 * we want to render text in the DOM */

let str = ReasonReact.stringToElement;

/* Set up the types and initial state */

type move = {
  id: int,
  name: string,
  tags: array(string),
  urls: array(string)
};

type state = {
  moves: list(move),
  draft: move,
  searchInput: string
};

let defaultMove1: move = {
  id: 0,
  name: "Swingout",
  tags: [| "8-count", "fundamentals", "open to open" |],
  urls: [|
    "https://www.youtube.com/watch?v=tNJzu8nOI4g",
    "https://www.youtube.com/watch?v=l6N_2lxg_Mk",
    "https://www.youtube.com/watch?v=88xB7bmia4Y"
  |]
};

let defaultMove2: move = {
  id: 1,
  name: "Tuck Turn",
  tags: [| "6-count", "fundamentals", "closed to open" |],
  urls: [|
    "https://www.youtube.com/watch?v=ZlWFfNgsfnw",
    "https://www.youtube.com/watch?v=U4wp8EORODI",
    "https://www.youtube.com/watch?v=Q3ew4E_-8Ps"
  |]
};

/* Set up actions to modify the state */

type action =
  /* edit draft */
  | EditName(string)
  | EditTagAtIndex(int, string)
  | DeleteTagAtIndex(int)
  | AddTag
  | EditUrlAtIndex(int, string)
  | DeleteUrlAtIndex(int)
  | AddUrl
  /* modify catalog */
  | AddDraftToCatalog
  | RemoveFromCatalogById(int)
  /* search catalog */
  | EditSearchInput(string);

/* Equivalent of getting event.target.value in JavaScript.
 * This is not typesafe; we've annotated it to return a string,
 * but if value happens to be of a different type we could have
 * an error. Unfortunately there is currently no better way to
 * to this in Reason. */

let valueFromEvent = (evt) : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

/* Filter moves based on the search input */

let filterMoves = (searchInput: string, moves: list(move)) : list(move) => {
  List.filter(
    (move) => {
      let input = String.lowercase(searchInput);
      Js.String.includes(input, String.lowercase(move.name)) ||
      Js.Array.some(
        (tag) => Js.String.includes(input, String.lowercase(tag)),
        move.tags
      )
    },
    moves
  )
};

/* Create sub-components */

module DraftTag {
  let component = ReasonReact.statelessComponent("DraftTag");
  let make = (~tag, ~edit, ~delete, _children) => {
    ...component,
    render: (_self) => {
      <div className="input-wrapper tag">
        <input
          value=tag
          _type="text"
          className="input"
          onChange=edit
        />
        <button className="delete-button" onClick=delete>(str("X"))</button>
      </div>
    }
  }
};

module DraftUrl {
  let component = ReasonReact.statelessComponent("DraftUrl");
  let make = (~url, ~edit, ~delete, _children) => {
    ...component,
    render: (_self) => {
      <div className="flex-line">
        <div className="input-wrapper url">
          <input
            value=url
            _type="text"
            className="input"
            onChange=edit
          />
          <button className="delete-button" onClick=delete>(str("X"))</button>
        </div>
      </div>
    }
  }
};

module Url {
  let component = ReasonReact.statelessComponent("Url");
  let make = (~url, _children) => {
    ...component,
    render: (_self) => {
      <li>
        <a href=url>
          (str(url))
        </a>
      </li>
    }
  }
};

module CatalogMove {
  let component = ReasonReact.statelessComponent("CatalogMove");
  let make = (~move, ~removeFromCatalog, _children) => {
    ...component,
    render: (_self) => {
      <div className="box catalog-box">
        <div className="move-header-wrapper">
          <div className="move-title-wrapper">
            <span className="move-title">(str(move.name))</span>
            <span className="move-bull-sep">(str(Js.String.fromCharCode(0x2022)))</span>
            <span className="move-tags">(str(Js.Array.joinWith(", ", move.tags)))</span>
          </div>
          <button className="button" onClick=removeFromCatalog>
            (str("Remove from catalog"))
          </button>
        </div>
        <ul className="url-list">
          (
            ReasonReact.arrayToElement(
              Array.mapi((index, url) => <Url url key=string_of_int(index) />, move.urls)
            )
          )
        </ul>
      </div>
    }
  }
};

/* Create the main component */

let component = ReasonReact.reducerComponent("Page");

let make = (_children) => {
  ...component,
  initialState: () => {
    moves: [ defaultMove1, defaultMove2 ],
    draft: {
      id: 2,
      name: "",
      tags: [| "" |],
      urls: [| "" |]
    },
    searchInput: ""
  },
  reducer: (action, state) => switch action {
    | EditName(name) => {
        let draft = { ...(state.draft), name };
        ReasonReact.Update({ ...state, draft })
      }
    | EditTagAtIndex(index, tag) => {
        state.draft.tags[index] = tag;
        ReasonReact.Update(state)
      }
    | DeleteTagAtIndex(index) => {
        Js.Array.removeCountInPlace(~pos=index, ~count=1, state.draft.tags) |> ignore;
        ReasonReact.Update(state)
      }
    | AddTag => {
        let draft = { ...(state.draft), tags: Array.append(state.draft.tags, [| "" |]) };
        ReasonReact.Update({ ...state, draft })
      }
    | EditUrlAtIndex(index, url) => {
        state.draft.urls[index] = url;
        ReasonReact.Update(state)
      }
    | DeleteUrlAtIndex(index) => {
        Js.Array.removeCountInPlace(~pos=index, ~count=1, state.draft.urls) |> ignore;
        ReasonReact.Update(state)
      }
    | AddUrl => {
        let draft = { ...(state.draft), urls: Array.append(state.draft.urls, [| "" |]) };
        ReasonReact.Update({ ...state, draft })
      }
    | AddDraftToCatalog => {
        ReasonReact.Update({
          ...state,
          moves: [state.draft, ...(state.moves)],
          draft: {
            id: state.draft.id + 1,
            name: "",
            tags: [| "" |],
            urls: [| "" |]
          }
        })
      }
    | RemoveFromCatalogById(id) => {
        let moves = List.filter((move) => move.id !== id, state.moves);
        ReasonReact.Update({ ...state, moves })
      }
    | EditSearchInput(searchInput) => {
        ReasonReact.Update({ ...state, searchInput })
      }
  },
  render: ({ state: { moves, draft, searchInput }, reduce }) =>
    <div>
      <div className="box">
        <div className="flex-line">
          <span className="input-label">(str("Name:"))</span>
          <div className="input-wrapper">
            <input
              value=draft.name
              _type="text"
              className="input"
              onChange=(reduce((evt) => EditName(valueFromEvent(evt))))
            />
          </div>
        </div>
        <div className="flex-line">
          <span className="input-label">(str("Tags:"))</span>
          (
            ReasonReact.arrayToElement(
              Array.mapi(
                (index, tag) => <DraftTag
                  key=string_of_int(index)
                  tag
                  edit=(reduce((evt) => EditTagAtIndex(index, valueFromEvent(evt))))
                  delete=(reduce((_evt) => DeleteTagAtIndex(index)))
                />, draft.tags))
          )
          <button className="plus-button" onClick=(reduce((_evt) => AddTag))>(str("+"))</button>
        </div>
        <div className="flex-line">
          <span className="input-label">(str("Video URLs:"))</span>
        </div>
        (
          ReasonReact.arrayToElement(
            Array.mapi(
              (index, url) => <DraftUrl
                key=string_of_int(index)
                url
                edit=(reduce((evt) => EditUrlAtIndex(index, valueFromEvent(evt))))
                delete=(reduce((_evt) => DeleteUrlAtIndex(index)))
              />, draft.urls))
        )
        <div className="flex-line">
          <button className="plus-button" onClick=(reduce((_evt) => AddUrl))>(str("+"))</button>
        </div>
      </div>
      <div className="add-button-wrapper">
        <button className="button" onClick=(reduce((_evt) => AddDraftToCatalog))>
          (str("Add to catalog"))
        </button>
      </div>
      <div className="title-wrapper">
        <span className="title">(str("Catalog"))</span>
        <div className="input-wrapper search">
          <input
            value=searchInput
            _type="text"
            className="input"
            placeholder="Search..."
            onChange=(reduce((evt) => EditSearchInput(valueFromEvent(evt))))
          />
        </div>
      </div>
      <div>
        (
          ReasonReact.arrayToElement(Array.of_list(
            List.map(
              (move) => <CatalogMove
                key=string_of_int(move.id)
                move
                removeFromCatalog=(reduce((_evt) => RemoveFromCatalogById(move.id)))
              />,
              filterMoves(searchInput, moves)
            )
          ))
        )
      </div>
    </div>
};

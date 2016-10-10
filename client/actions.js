
import fetch from 'isomorphic-fetch'

export const REQUEST_TODOS = 'REQUEST_TODOS'
export const RECEIVE_TODOS = 'RECEIVE_TODOS'

function receiveTodos (json) {
  return {
    type: RECEIVE_TODOS,
    data: json,
    receivedAt: Date.now()
  }
}

export function fetchTodos() {
  return function (dispatch) {
    return fetch('/api/todo')
      .then(response => response.json())
      .then(json =>
        dispatch(receiveTodos(json))
      )
  }
}

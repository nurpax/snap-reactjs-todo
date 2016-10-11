
import fetch from 'isomorphic-fetch'

export const USER_LOGGED_IN = 'USER_LOGGED_IN'
export const USER_LOGGED_OUT = 'USER_LOGGED_OUT'

export const REQUEST_TODOS = 'REQUEST_TODOS'
export const RECEIVE_TODOS = 'RECEIVE_TODOS'

// Login handling
// TODO issue async here
export function login(data) {
  return {
    type: USER_LOGGED_IN,
    payload: data
  }
}

export function logout() {
  return {
    type: USER_LOGGED_OUT
  }
}

// Todo handling
function receiveTodos (json) {
  return {
    type: RECEIVE_TODOS,
    data: json,
    receivedAt: Date.now()
  }
}

export function fetchTodos () {
  return function (dispatch) {
    return fetch('/api/todo')
      .then(response => response.json())
      .then(json =>
        dispatch(receiveTodos(json))
      )
  }
}

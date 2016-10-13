
import fetch from 'isomorphic-fetch'
import jwt_decode from 'jwt-decode'

export const USER_LOGGED_IN = 'USER_LOGGED_IN'
export const USER_LOGGED_OUT = 'USER_LOGGED_OUT'

export const REQUEST_TODOS = 'REQUEST_TODOS'
export const RECEIVE_TODOS = 'RECEIVE_TODOS'

export function login (login, pass) {
  return function (dispatch) {
    return fetch('/api/login', {
        method: 'POST',
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({login, pass})
      })
      .then(response => response.json())
      .then(function (json) {
        let tok = jwt_decode(json.token)
        localStorage.setItem('token', JSON.stringify(tok))
        dispatch({
          type: USER_LOGGED_IN,
          payload: tok
        })
      })
    }
}

export function logout() {
  localStorage.removeItem('token')
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

/* eslint camelcase: 0 */

import fetch from 'isomorphic-fetch'
import jwt_decode from 'jwt-decode'

export const USER_LOGGED_IN = 'USER_LOGGED_IN'
export const USER_LOGGED_OUT = 'USER_LOGGED_OUT'

export const REQUEST_TODO_LIST = 'REQUEST_TODO_LIST'
export const RECEIVE_TODO_LIST = 'RECEIVE_TODO_LIST'

export const RECEIVE_TODO = 'RECEIVE_TODO'

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
      // Keep the original jwt token for sending it back to the server in
      // fetch headers
      tok.token = json.token
      localStorage.setItem('token', JSON.stringify(tok))
      dispatch({
        type: USER_LOGGED_IN,
        payload: tok
      })
    })
  }
}

export function logout () {
  localStorage.removeItem('token')
  return {
    type: USER_LOGGED_OUT
  }
}

// Todo handling
function receiveTodos (json) {
  return {
    type: RECEIVE_TODO_LIST,
    data: json,
    receivedAt: Date.now()
  }
}

function receiveTodo (json) {
  return {
    type: RECEIVE_TODO,
    data: json,
    receivedAt: Date.now()
  }
}

export function fetchTodos () {
  return function (dispatch, getState) {
    return fetch('/api/todo', { headers: {'Authorization': 'Bearer ' + getState().user.token} })
      .then(response => response.json())
      .then(json =>
        dispatch(receiveTodos(json))
      )
  }
}

export function saveTodo (todo) {
  return function (dispatch, getState) {
    return fetch('/api/todo', {
      method: 'POST',
      headers: {'Authorization': 'Bearer ' + getState().user.token},
      body: JSON.stringify(todo)
    })
    .then(response => response.json())
    .then(json =>
      dispatch(receiveTodo(json))
    )
  }
}

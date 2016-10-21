/* eslint camelcase: 0 */

import fetch from 'isomorphic-fetch'
import jwt_decode from 'jwt-decode'

import * as c from './constants'

// Wrapper to translate expected login errors
function checkLogin (dispatch, response, doLogin) {
  if (response.status === 200) {
    return response.json().then(doLogin)
  } else if (response.status === 401) {
    return response.json().then(function (err) {
      dispatch(setNotification(err.error))
    })
  }
  // TODO throw unknown error here
}

export function doLoginRequest (newUser, login, pass) {
  let url = newUser ? '/api/login/new' : '/api/login'
  return function (dispatch) {
    function doLogin (json) {
      let tok = jwt_decode(json.token)
      // Keep the original jwt token for sending it back to the server in
      // fetch headers
      tok.token = json.token
      localStorage.setItem('token', JSON.stringify(tok))
      dispatch({
        type: c.USER_LOGGED_IN,
        payload: tok
      })
    }
    return fetch(url, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({login, pass})
    })
    .then(response => checkLogin(dispatch, response, doLogin))
    .catch(function (error) {
      console.log('request failed', error)
    })
  }
}

export function login (login, pass) {
  return doLoginRequest(false, login, pass)
}

export function signUp (login, pass) {
  return doLoginRequest(true, login, pass)
}

export function logout () {
  localStorage.removeItem('token')
  return {
    type: c.USER_LOGGED_OUT
  }
}

// Todo handling
function receiveTodos (json) {
  return {
    type: c.RECEIVE_TODO_LIST,
    data: json,
    receivedAt: Date.now()
  }
}

function receiveTodo (json) {
  return {
    type: c.RECEIVE_TODO,
    data: json,
    receivedAt: Date.now()
  }
}

// Authorized fetch that should be used for any API end point that requires
// the user to be logged in.
//
// The body parameter is automatically converted into JSON if it's passed in
// as an object.  The server response is also automatically JSON parsed.
function fetchWithAuth (getState, url, params) {
  let p = {...params}
  p.headers = {'Authorization': 'Bearer ' + getState().user.token}
  if (params.body instanceof Object) {
    p.body = JSON.stringify(params.body)
  }
  return fetch(url, p).then(r => r.json())
}

export function fetchTodos () {
  return function (dispatch, getState) {
    return fetchWithAuth(getState, '/api/todo', {}).then(json => dispatch(receiveTodos(json)))
  }
}

export function saveTodo (todo) {
  return function (dispatch, getState) {
    return fetchWithAuth(getState, '/api/todo', {method: 'POST', body: todo})
      .then(json => dispatch(receiveTodo(json))
      )
  }
}

export function setFilter (filter) {
  return {
    type: c.SET_FILTER,
    data: filter
  }
}

export function setNotification (msg) {
  return dispatch => {
    dispatch({ type: c.NOTIFY_SET, data: msg })
    setTimeout(() => {
      dispatch({
        type: c.NOTIFY_DISMISS,
        data: null
      })
    }, 5000)
  }
}

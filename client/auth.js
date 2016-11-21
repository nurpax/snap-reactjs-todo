
/* eslint camelcase: 0 */
/*eslint no-console: ["error", { allow: ["warn", "error"] }] */

import fetch from 'isomorphic-fetch'
import jwt_decode from 'jwt-decode'
import { push } from 'react-router-redux'

// Redux actions and reducers for authetication with JWT

export const USER_LOGGED_IN = 'USER_LOGGED_IN'
export const USER_LOGGED_OUT = 'USER_LOGGED_OUT'

// Wrapper to translate expected login errors
function checkLogin (dispatch, response, doLogin, notify) {
  if (response.status === 200) {
    return response.json().then(doLogin)
  } else if (response.status === 401) {
    return response.json().then(function (err) {
      if (notify && notify instanceof Function) {
        dispatch(notify(err.error))
      } else {
        throw new Error('notify action must be a function')
      }
    })
  }
  // TODO throw unknown error here
}

function doLoginRequest (newUser, params) {
  let url = newUser ? '/api/login/new' : '/api/login'
  return function (dispatch) {
    function doLogin (json) {
      let tok = jwt_decode(json.token)
      // Keep the original jwt token for sending it back to the server in
      // fetch headers
      tok.token = json.token
      localStorage.setItem('token', JSON.stringify(tok))
      dispatch({
        type: USER_LOGGED_IN,
        payload: tok
      })
    }
    let postParams = { login: params.login, pass: params.pass }
    return fetch(url, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(postParams)
    })
    .then(response => checkLogin(dispatch, response, doLogin, params.notify))
    .catch(function (error) {
      console.warn('unknown error in handling login request', error)
    })
  }
}

function checkHttpStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response
  } else {
    var error = new Error(response.statusText)
    error.response = response
    throw error
  }
}

// Authorized fetch that should be used for any API end point that requires
// the user to be logged in.
//
// The body parameter is automatically converted into JSON if it's passed in
// as an object.  The server response is also automatically JSON parsed.
export function fetchWithAuth (dispatch, getState, url, params, cb) {
  let p = {...params}
  p.headers = {'Authorization': `Bearer ${getState().user.token}`}
  if (params.body instanceof Object) {
    p.body = JSON.stringify(params.body)
  }
  return fetch(url, p)
    .then(checkHttpStatus)
    .then(r => r.json())
    .then(json => cb(json))
    .catch(error => {
      // TODO should set a notifier for this
      // maybe move notification code into the auth module?
      if (error.response.status == 401) {
        dispatch(logout())
        dispatch(push('/login'))
      }
    })
}

// params.login  - login name
// params.pass   - password to login with
// params.notify - notify action to call on login errors
export function login (params) {
  return doLoginRequest(false, params)
}

// params.login  - login name
// params.pass   - password to login with
// params.notify - notify action to call on login errors
export function signUp (params) {
  return doLoginRequest(true, params)
}

export function logout () {
  localStorage.removeItem('token')
  return {
    type: USER_LOGGED_OUT
  }
}

export const userReducer = (state = JSON.parse(localStorage.getItem('token')) || null, { type, payload }) => {
  if (type === USER_LOGGED_IN) {
    return payload
  }
  if (type === USER_LOGGED_OUT) {
    return null
  }
  return state
}


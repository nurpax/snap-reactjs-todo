
/* eslint camelcase: 0 */
/*eslint no-console: ["error", { allow: ["warn", "error"] }] */

import fetch from 'isomorphic-fetch'
import jwt_decode from 'jwt-decode'
import { combineReducers } from 'redux'
import { push } from 'react-router-redux'
import locationHelperBuilder from 'redux-auth-wrapper/history4/locationHelper'
import { connectedRouterRedirect } from 'redux-auth-wrapper/history4/redirect'
import connectedAuthWrapper from 'redux-auth-wrapper/connectedAuthWrapper'

// Redux actions and reducers for authetication with JWT

export const USER_LOGGED_IN = 'USER_LOGGED_IN'
export const USER_LOGGED_OUT = 'USER_LOGGED_OUT'
export const LOGIN_STATUS_SET = 'LOGIN_STATUS_SET'
export const LOGIN_STATUS_DISMISS = 'LOGIN_STATUS_DISMISS'

export function setNotification (msg) {
  return dispatch => {
    dispatch({ type: LOGIN_STATUS_SET, data: msg })
    setTimeout(() => {
      dispatch({
        type: LOGIN_STATUS_DISMISS
      })
    }, 5000)
  }
}

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
  p.headers = {...p.headers, 'Authorization': `Bearer ${getState().auth.user.token}`}
  if (params.body instanceof Object) {
    p.body = JSON.stringify(params.body)
  }
  return fetch(url, p)
    .then(checkHttpStatus)
    .then(r => r.json())
    .then(json => cb(json))
    .catch(error => {
      if (error.response.status == 401) {
        dispatch(logout())
        dispatch(setNotification('Login expired'))
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

const userReducer = (state = JSON.parse(localStorage.getItem('token')) || null, { type, payload }) => {
  if (type === USER_LOGGED_IN) {
    return payload
  }
  if (type === USER_LOGGED_OUT) {
    return null
  }
  return state
}

const notifyReducer = (state = null, { type, data }) => {
  if (type === LOGIN_STATUS_SET) {
    return data
  } else if (type === LOGIN_STATUS_DISMISS) {
    return null
  }
  return state
}

export const authReducer = combineReducers({
  user: userReducer,
  status: notifyReducer
})

// User selector from state
export const getUser = (state) => state.auth ? state.auth.user : null

const locationHelper = locationHelperBuilder({})

const userIsAuthenticatedDefaults = {
  authenticatedSelector: state => getUser(state) !== null,
  wrapperDisplayName: 'UserIsAuthenticated'
}

export const userIsAuthenticated = connectedAuthWrapper(userIsAuthenticatedDefaults)
export const userIsAuthenticatedRedir = connectedRouterRedirect({
  ...userIsAuthenticatedDefaults,
  redirectPath: '/login'
})

const userIsNotAuthenticatedDefaults = {
  authenticatedSelector: state => getUser(state) == null,
  wrapperDisplayName: 'UserIsNotAuthenticated'
}

export const userIsNotAuthenticated = connectedAuthWrapper(userIsNotAuthenticatedDefaults)

export const userIsNotAuthenticatedRedir = connectedRouterRedirect({
  ...userIsNotAuthenticatedDefaults,
  redirectPath: (state, ownProps) => locationHelper.getRedirectQueryParam(ownProps) || '/',
  allowRedirectBack: false
})


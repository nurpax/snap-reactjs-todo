
import { combineReducers } from 'redux'
import { routerReducer } from 'react-router-redux'

import * as c from './constants'
import * as auth from './auth'

function receiveTodos (state = [], action) {
  switch (action.type) {
    case c.RECEIVE_TODO_LIST:
      return action.data
    case c.RECEIVE_TODO:
      let existingItem = false
      let newState =
        state.map(function (todo) {
          if (todo.id === action.data.id) {
            existingItem = true
            return action.data
          } else {
            return todo
          }
        })
      if (!existingItem) {
        return state.concat(action.data)
      }
      return newState
    default:
      return state
  }
}

const filterReducer = (state = 'active', { type, data }) => {
  if (type === c.SET_FILTER) {
    return data
  }
  return state
}

const notifyReducer = (state = null, { type, data }) => {
  if (type === c.NOTIFY_SET) {
    return data
  } else if (type === c.NOTIFY_DISMISS) {
    return null
  }
  return state
}

const appReducer = combineReducers({
  user: auth.userReducer,
  filter: filterReducer,
  routing: routerReducer,
  todos: receiveTodos,
  notification: notifyReducer
})

const rootReducer = (state, action) => {
  // Reset redux state if the user logged out.
  //
  // This state reset is required.  Otherwise logging in as user X, logging
  // out and logging in as user Y will show user Y data from the previously
  // logged in user X.
  if (action.type === auth.USER_LOGGED_OUT) {
    state = undefined
  }
  return appReducer(state, action)
}

export default rootReducer

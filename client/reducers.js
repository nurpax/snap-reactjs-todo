
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

const rootReducer = combineReducers({
  user: auth.userReducer,
  filter: filterReducer,
  routing: routerReducer,
  todos: receiveTodos,
  notification: notifyReducer
})

export default rootReducer

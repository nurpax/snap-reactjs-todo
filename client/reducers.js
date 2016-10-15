
import { combineReducers } from 'redux'
import { routerReducer } from 'react-router-redux'

import {
  RECEIVE_TODO_LIST, RECEIVE_TODO, USER_LOGGED_IN, USER_LOGGED_OUT, SET_FILTER
} from './actions'

function receiveTodos (state = [], action) {
  switch (action.type) {
    case RECEIVE_TODO_LIST:
      return action.data
    case RECEIVE_TODO:
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

const userReducer = (state = JSON.parse(localStorage.getItem('token')) || null, { type, payload }) => {
  if (type === USER_LOGGED_IN) {
    return payload
  }
  if (type === USER_LOGGED_OUT) {
    return null
  }
  return state
}

const filterReducer = (state = 'active', { type, data }) => {
  if (type === SET_FILTER) {
    return data
  }
  return state
}

const rootReducer = combineReducers({
  user: userReducer,
  filter: filterReducer,
  routing: routerReducer,
  todos: receiveTodos
})

export default rootReducer

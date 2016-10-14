
import { combineReducers } from 'redux'
import { routerReducer } from 'react-router-redux'

import {
  RECEIVE_TODO_LIST, RECEIVE_TODO, USER_LOGGED_IN, USER_LOGGED_OUT
} from './actions'

function receiveTodos (state = [], action) {
  switch (action.type) {
    case RECEIVE_TODO_LIST:
      return action.data
    case RECEIVE_TODO:
      return state.concat(action.data)
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

const rootReducer = combineReducers({
  user: userReducer,
  routing: routerReducer,
  todos: receiveTodos
})

export default rootReducer

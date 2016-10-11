
import { combineReducers } from 'redux'
import {
  REQUEST_TODOS
} from './actions'

function requestTodos(state = { }, action) {
  switch (action.type) {
    case REQUEST_TODOS:
      return state
    default:
      return state
  }
}

const rootReducer = combineReducers({
  requestTodos
})

export default rootReducer

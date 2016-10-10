
import { combineReducers } from 'redux'
import {
  RECEIVE_TODOS
} from './actions'

function todos(state = [], action) {
  switch (action.type) {
    case RECEIVE_TODOS:
      return action.data
    default:
      return state
  }
}

const rootReducer = combineReducers({
  todos
})

export default rootReducer

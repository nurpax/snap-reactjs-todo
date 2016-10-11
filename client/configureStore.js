import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import rootReducer from './reducers'
import createLogger from 'redux-logger'
import { routerMiddleware } from 'react-router-redux'
import { browserHistory } from 'react-router'

const logger = createLogger()

const routingMiddleware = routerMiddleware(browserHistory)

export default function configureStore (preloadedState) {
  return createStore(
    rootReducer,
    preloadedState,
    applyMiddleware(
      thunkMiddleware,
      routingMiddleware,
      logger
    )
  )
}

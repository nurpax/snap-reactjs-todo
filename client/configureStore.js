import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import rootReducer from './reducers'
import { routerMiddleware } from 'react-router-redux'
import { browserHistory } from 'react-router'

const routingMiddleware = routerMiddleware(browserHistory)

const middlewares = [thunkMiddleware, routingMiddleware]

if (process.env.NODE_ENV === 'development') {
  const createLogger = require('redux-logger')
  const logger = createLogger()
  middlewares.push(logger)
}

export default function configureStore (preloadedState) {
  return createStore(
    rootReducer,
    preloadedState,
    applyMiddleware(...middlewares)
  )
}

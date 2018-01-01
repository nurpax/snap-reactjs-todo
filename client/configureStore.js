import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import rootReducer from './reducers'
import { routerMiddleware } from 'react-router-redux'
import createHistory from 'history/createBrowserHistory'

// Create a history of your choosing (we're using a browser history in this case)
const history = createHistory()

const routermware = routerMiddleware(history)

// Build the middleware for intercepting and dispatching navigation actions
const middlewares = [thunkMiddleware, routermware]

if (process.env.NODE_ENV === 'development') {
  const logger = require('redux-logger').default
  middlewares.push(logger)
}

export default function configureStore (preloadedState) {
  const store = createStore(
    rootReducer,
    preloadedState,
    applyMiddleware(...middlewares)
  )
  return { store, history }
}

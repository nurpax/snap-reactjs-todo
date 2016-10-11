import React from 'react'
import { Provider } from 'react-redux'
import { Router, Route, browserHistory } from 'react-router'
import { syncHistoryWithStore, routerActions } from 'react-router-redux'
import { UserAuthWrapper } from 'redux-auth-wrapper'

import App from './App'
import Login from './Login'
import Test from './Test'

import configureStore from '../configureStore'
import { fetchTodos } from '../actions'

const store = configureStore()
const history = syncHistoryWithStore(browserHistory, store)

const UserIsAuthenticated = UserAuthWrapper({
  authSelector: state => state.user,
  redirectAction: routerActions.replace,
  wrapperDisplayName: 'UserIsAuthenticated'
})

const Root = () => (
  <Provider store={store}>
    <Router history={history}>
      <Route path='test' component={UserIsAuthenticated(Test)} />
      <Route path='login' component={Login} />
      <Route path='/(:filter)' component={App} />
    </Router>
  </Provider>
)

store.dispatch(fetchTodos())

export default Root

import React from 'react'
import { Provider } from 'react-redux'
import { Router, Route, browserHistory } from 'react-router'
import { syncHistoryWithStore, routerActions } from 'react-router-redux'
import { UserAuthWrapper } from 'redux-auth-wrapper'

import Main from './Main'
import App from './App'
import Login from './Login'

import configureStore from '../configureStore'

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
      <Route path='/' component={Main} />
      <Route path='/todos' component={UserIsAuthenticated(App)} />
      <Route path='/login' component={Login} />
    </Router>
  </Provider>
)

export default Root

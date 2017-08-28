import React from 'react'
import { Provider } from 'react-redux'
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom'

import { userIsNotAuthenticatedRedir, userIsAuthenticatedRedir } from '../auth'
import Main from './Main'
import TodoList from './TodoList'
import Login from './Login'
import SignUp from './SignUp'
import Profile from './Profile'

import configureStore from '../configureStore'

const store = configureStore()

const AuthLogin = userIsNotAuthenticatedRedir(Login)
const AuthSignUp = userIsNotAuthenticatedRedir(SignUp)
const AuthTodoList = userIsAuthenticatedRedir(TodoList)
const AuthProfile = userIsAuthenticatedRedir(Profile)

const Root = () => (
  <Provider store={store}>
    <Router>
      <Switch>
        <Route exact path='/' component={Main} />
        <Route exact path='/todos' component={AuthTodoList} />
        <Route exact path='/login' component={AuthLogin} />
        <Route exact path='/profile' component={AuthProfile} />
        <Route exact path='/signup' component={AuthSignUp} />
      </Switch>
    </Router>
  </Provider>
)

export default Root

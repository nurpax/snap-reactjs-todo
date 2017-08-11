import React from 'react'
import { Provider } from 'react-redux'
import { BrowserRouter as Router, Route } from 'react-router-dom'

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
      <div>
        <Route exact path='/' component={Main} />
        <Route path='/todos' component={AuthTodoList} />
        <Route path='/login' component={AuthLogin} />
        <Route path='/profile' component={AuthProfile} />
        <Route path='/signup' component={AuthSignUp} />
      </div>
    </Router>
  </Provider>
)

export default Root

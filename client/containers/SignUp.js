
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from './Layout'
import LoginForm from '../components/LoginForm'
import LoginError from './LoginError.js'
import { signUp } from '../auth'

class SignUpContainer extends Component {
  static propTypes = {
    signUp: PropTypes.func.isRequired,
  }

  handleSubmit = (p) => {
    this.props.signUp({
      login: p.login,
      pass: p.pass
    })
  };

  render () {
    return (
      <div>
        <LoginForm type='signup' onSubmit={this.handleSubmit} />
        <LoginError />
      </div>
    )
  }
}

export default Layout(connect(null, { signUp, replace: routerActions.replace })(SignUpContainer))


import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'

import Layout from './Layout'
import LoginForm from '../components/LoginForm'
import LoginError from './LoginError.js'
import { login } from '../auth'

class LoginContainer extends Component {
  static propTypes = {
    login: PropTypes.func.isRequired
  }

  handleSubmit = (p) => {
    this.props.login({
      login: p.login,
      pass: p.pass
    })
  };

  render () {
    return (
      <div>
        <LoginForm type='login' onSubmit={this.handleSubmit} />
        <LoginError />
      </div>
    )
  }
}

export default Layout(connect(null, { login })(LoginContainer))

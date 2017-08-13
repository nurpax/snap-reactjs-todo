
import React, { Component } from 'react'
import PropTypes from 'prop-types'

import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginForm from '../components/LoginForm'

import LoginError from './LoginError.js'
import { login, getUser } from '../auth'

function select (state, ownProps) {
  const isAuthenticated = getUser(state)
  return {
    isAuthenticated,
  }
}

class LoginContainer extends Component {
  static propTypes = {
    login: PropTypes.func.isRequired,
    isAuthenticated: PropTypes.object
  }

  handleSubmit = (p) => {
    this.props.login({
      login: p.login,
      pass: p.pass
    })
  };

  render () {
    return (
      <Layout user={this.props.isAuthenticated}>
        <LoginForm type='login' onSubmit={this.handleSubmit} />
        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { login })(LoginContainer)

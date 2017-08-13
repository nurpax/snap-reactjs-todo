
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginForm from '../components/LoginForm'
import LoginError from './LoginError.js'
import { signUp, getUser } from '../auth'

function select (state) {
  const isAuthenticated = getUser(state)
  return {
    isAuthenticated
  }
}

class SignUpContainer extends Component {
  static propTypes = {
    signUp: PropTypes.func.isRequired,
    isAuthenticated: PropTypes.object
  }

  handleSubmit = (p) => {
    this.props.signUp({
      login: p.login,
      pass: p.pass
    })
  };

  render () {
    return (
      <Layout user={this.props.isAuthenticated}>
        <LoginForm type='signup' onSubmit={this.handleSubmit} />
        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { signUp, replace: routerActions.replace })(SignUpContainer)

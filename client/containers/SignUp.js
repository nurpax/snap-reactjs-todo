
import React, { PropTypes, Component } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginForm from '../components/LoginForm'
import LoginError from './LoginError.js'
import { signUp } from '../auth'
import { setNotification } from '../actions'

function select (state) {
  const isAuthenticated = state.user || null
  return {
    isAuthenticated
  }
}

class SignUpContainer extends Component {
  static defaultProps = { redirect: '/' }
  static propTypes = {
    signUp: PropTypes.func.isRequired,
    replace: PropTypes.func.isRequired,
    redirect: React.PropTypes.string.isRequired,
    isAuthenticated: React.PropTypes.object
  }

  componentWillReceiveProps (nextProps) {
    const { isAuthenticated, replace, redirect } = nextProps

    if (isAuthenticated) {
      replace(redirect)
    }
  }

  handleSubmit = (p) => {
    this.props.signUp({
      login: p.login,
      pass: p.pass,
      notify: setNotification
    })
  };

  render () {
    return (
      <Layout isLoginScreen user={this.props.isAuthenticated}>
        <LoginForm type='signup' onSubmit={this.handleSubmit} />
        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { signUp, replace: routerActions.replace })(SignUpContainer)

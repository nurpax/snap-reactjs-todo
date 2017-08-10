
import React, { PropTypes, Component } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginForm from '../components/LoginForm'

import LoginError from './LoginError.js'
import { login, getUser } from '../auth'

function select (state, ownProps) {
  const isAuthenticated = getUser(state)
  const redirect = ownProps.location.query.redirect || '/'
  return {
    isAuthenticated,
    redirect
  }
}

class LoginContainer extends Component {
  static propTypes = {
    login: PropTypes.func.isRequired,
    replace: PropTypes.func.isRequired,
    isAuthenticated: React.PropTypes.object,
    redirect: React.PropTypes.string
  }

  componentWillMount () {
    const { isAuthenticated, replace, redirect } = this.props
    if (isAuthenticated) {
      replace(redirect)
    }
  }

  componentWillReceiveProps (nextProps) {
    const { isAuthenticated, replace, redirect } = nextProps
    const { isAuthenticated: wasAuthenticated } = this.props

    if (!wasAuthenticated && isAuthenticated) {
      replace(redirect)
    }
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

export default connect(select, { login, replace: routerActions.replace })(LoginContainer)


import React, { PropTypes, Component } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginError from './LoginError.js'
import { login } from '../actions'

function select (state, ownProps) {
  const isAuthenticated = state.user || null
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

  onClick = (e) => {
    e.preventDefault()
    this.props.login(this.refs.login.value, this.refs.pass.value)
    this.refs.login.value = ''
    this.refs.pass.value = ''
  };

  render () {
    return (
      <Layout isLoginScreen user={this.props.isAuthenticated}>
        <h2>Login</h2>
        <input type='text' ref='login' placeholder='Account name..' />
        <br />
        <input type='password' ref='pass' placeholder='Password..' />
        <br />
        <button className='button-primary' onClick={this.onClick}>Login</button>
        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { login, replace: routerActions.replace })(LoginContainer)


import React, { PropTypes, Component } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
import LoginError from './LoginError.js'
import { login } from '../auth'
import { setNotification } from '../actions'

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
    this.props.login({
      login: this.refs.login.value,
      pass: this.refs.pass.value,
      notify: setNotification
    })
    this.refs.login.value = ''
    this.refs.pass.value = ''
  };

  render () {
    return (
      <Layout isLoginScreen user={this.props.isAuthenticated}>
        <h2>Login</h2>
        <div className='row'>
          <div className='six columns'>
            <label htmlFor='loginNameInput'>Username or e-mail address</label>
            <input className='u-full-width' type='text' placeholder='test@example.com'
              id='loginNameInput' ref='login' />
          </div>
        </div>

        <div className='row'>
          <div className='six columns'>
            <label htmlFor='loginPassInput'>Password</label>
            <input className='u-full-width' type='password' placeholder='Password..'
              id='loginPassInput' ref='pass' />
          </div>
        </div>

        <div className='row'>
          <div className='six columns'>
            <button className='button-primary u-full-width' onClick={this.onClick}>Sign in</button>
          </div>
        </div>

        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { login, replace: routerActions.replace })(LoginContainer)

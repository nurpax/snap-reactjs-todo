
import React, { PropTypes, Component } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import Layout from '../components/Layout'
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

  onClick = (e) => {
    e.preventDefault()
    this.props.signUp({
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
        <h2>Sign up</h2>
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

export default connect(select, { signUp, replace: routerActions.replace })(SignUpContainer)

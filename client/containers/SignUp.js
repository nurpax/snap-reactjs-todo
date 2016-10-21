
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
        <h2>Create a New User</h2>
        <input type='text' ref='login' placeholder='Account name..' />
        <br />
        <input type='password' ref='pass' placeholder='Password..' />
        <br />
        <button className='button-primary' onClick={this.onClick}>Sign Up</button>
        <LoginError />
      </Layout>
    )
  }
}

export default connect(select, { signUp, replace: routerActions.replace })(SignUpContainer)

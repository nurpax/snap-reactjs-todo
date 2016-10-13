
import React, { Component, PropTypes } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import { login } from '../actions'

function select (state, ownProps) {
  const isAuthenticated = state.user || false
  const redirect = ownProps.location.query.redirect || '/'
  return {
    isAuthenticated,
    redirect
  }
}

class LoginContainer extends Component {
  static propTypes = {
    login: PropTypes.func.isRequired,
    replace: PropTypes.func.isRequired
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
  };

  render () {
    return (
      <div>
        <h2>Login</h2>
        <input type="text" ref="login" placeholder="Account name.." />
        <br/>
        <input type="password" ref="pass" placeholder="Password.." />
        <br/>
        <button onClick={this.onClick}>Login</button>
      </div>
    )
  }
}

export default connect(select, { login, replace: routerActions.replace })(LoginContainer)

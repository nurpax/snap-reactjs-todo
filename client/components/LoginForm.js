// Login/Signup form

import React, { Component } from 'react'
import PropTypes from 'prop-types'

export default class LoginForm extends Component {
  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    type: PropTypes.oneOf(['login', 'signup']).isRequired
  }

  onClick = (e) => {
    e.preventDefault()
    this.props.onSubmit({
      login: this.refs.login.value,
      pass: this.refs.pass.value
    })
    this.refs.login.value = ''
    this.refs.pass.value = ''
  };

  render () {
    return (
      <div>
        <h2>{this.props.type == 'login' ? "Login" : "Sign up"}</h2>
        <div>
          <label htmlFor='loginNameInput'>Username or e-mail address</label>
          <input className='u-full-width' type='text' placeholder='test@example.com'
            id='loginNameInput' ref='login' />
        </div>

        <div>
          <label htmlFor='loginPassInput'>Password</label>
          <input className='u-full-width' type='password' placeholder='Password..'
            id='loginPassInput' ref='pass' />
        </div>

        <div>
          <button className='button-primary u-full-width'
            onClick={this.onClick}>
            {this.props.type == 'login' ? 'Sign in' : 'Sign up'}
          </button>
        </div>
      </div>
    )
  }
}

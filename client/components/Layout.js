
import React, { Component, PropTypes } from 'react'

import { Link } from 'react-router'
import LogoutLink from '../containers/LogoutLink'

export default class Layout extends Component {
  static propTypes = {
    children: PropTypes.any,
    user: PropTypes.object,
    isLoginScreen: PropTypes.bool
  }
  render () {
    let login = this.props.user ?
      null : <Link to='/login'>Sign In</Link>
    let logout = this.props.user ?
      <LogoutLink to='/'>Log out</LogoutLink> : null
    let loginControls =
      this.props.isLoginScreen ? null : <span>{login}{logout}</span>
    return (
      <div className='container'>
        <h1>Todo list app</h1>
        {this.props.children}
        <br />
        <div>
          <Link to='/'>Home</Link>{loginControls ? <span> | {loginControls}</span> : null}
        </div>
      </div>
    )
  }
}

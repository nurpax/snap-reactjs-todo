
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { Link } from 'react-router-dom'
import classNames from 'classnames'

import LogoutLink from '../containers/LogoutLink'
import s from './Navbar.scss'

/* eslint-disable react/prop-types */
const Nli = (props) =>
  <li className={classNames(s.nav, props.className)}>{props.children}</li>
/* eslint-enable react/prop-types */

export default class Navbar extends Component {
  static propTypes = {
    user: PropTypes.object
  }

  loggedInUserMenu () {
    return (
      <Nli className={s.dropdown}>
        <span className={s.dropbtn}>Profile</span>
        <div className={s.dropdownContent}>
          <div className={s.text}>Logged in as {this.props.user.login}</div>
          <div className={s.dropdownDivider} />
          <Link to='/profile'>Settings</Link>
          <LogoutLink to='/'>Logout</LogoutLink>
        </div>
      </Nli>
    )
  }

  loggedOutUserMenu () {
    return (
      <Nli className={s.dropdown}>
        <span className={s.dropbtn}>Sign-in</span>
        <div className={s.dropdownContent}>
          <Link to='/login'>Login</Link>
          <Link to='/signup'>Sign Up</Link>
        </div>
      </Nli>
    )
  }

  userMenu () {
    return this.props.user ? this.loggedInUserMenu() : this.loggedOutUserMenu()
  }

  render () {
    let topClass = classNames(s.nav)
    return (
      <ul className={topClass}>
        <Nli><Link className={s.active} to='/'>Home</Link></Nli>
        {this.userMenu()}
      </ul>
      )
  }
}

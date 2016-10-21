
import React, { Component, PropTypes } from 'react'
import { routerActions } from 'react-router-redux'
import { connect } from 'react-redux'

import { logout } from '../auth'
import { Link } from 'react-router'

class LogoutLinkContainer extends Component {
  static propTypes = {
    logout: PropTypes.func.isRequired,
    replace: PropTypes.func.isRequired,
    to: PropTypes.string.isRequired,
    children: React.PropTypes.any.isRequired
  }

  onClick = (e) => {
    e.preventDefault()
    this.props.logout()
    this.props.replace(this.props.to)
  };

  render () {
    return <Link onClick={this.onClick} to={this.props.to}>{this.props.children}</Link>
  }
}

export default connect(null, { logout, replace: routerActions.replace })(LogoutLinkContainer)

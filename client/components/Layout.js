
import React, { Component, PropTypes } from 'react'

var Link = require('react-router').Link

export default class Layout extends Component {
  static propTypes = {
    user: PropTypes.object.isRequired
  }
  render () {
    let login = this.props.user ?
      null : <Link to='/login'>Sign In</Link>
    return (
      <div className="container">
        <h1>Todo list app</h1>
        {login}
        {this.props.children}
      </div>
    )
  }
}

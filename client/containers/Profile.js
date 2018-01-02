
import React, { Component } from 'react'
import Layout from './Layout'

class ProfileView extends Component {
  render () {
    return (
      <div>
        <h4>Account settings for {this.props.user.login}</h4>
        <p>TODO change password</p>
      </div>
    )
  }
}

export default Layout(ProfileView)


import React, { Component, PropTypes } from 'react'

// eslint-disable-next-line no-unused-vars
import globalStyles from './Layout.scss'

import Navbar from './Navbar'

export default class Layout extends Component {
  static propTypes = {
    children: PropTypes.any,
    user: PropTypes.object,
    isLoginScreen: PropTypes.bool
  }
  render () {
    return (
      <div>
        <div className='container'>
          <Navbar user={this.props.user} />
          {this.props.children}
          <br />
        </div>
      </div>
    )
  }
}

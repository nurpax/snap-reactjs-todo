
import React, { Component } from 'react'
import PropTypes from 'prop-types'

// Pull skeleton into the CSS build
// eslint-disable-next-line no-unused-vars
import globalStyles from './globalStyles.css'
import s from './Layout.scss'

import Navbar from './Navbar'

export default class Layout extends Component {
  static propTypes = {
    children: PropTypes.any,
    user: PropTypes.object
  }
  render () {
    return (
      <div className={s.app}>
        <div className={s.flexContainer}>
          <div className={s.navbar}>
            <Navbar user={this.props.user} />
          </div>
        </div>

        <div className={s.flexContainer}>
          <div className={s.contentPadding}>
            {this.props.children}
          </div>
        </div>
      </div>
    )
  }
}

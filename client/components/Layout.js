
import React, { Component, PropTypes } from 'react'
import classNames from 'classNames'

// eslint-disable-next-line no-unused-vars
import s from './Layout.scss'

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

          <div className={classNames("row", s.navbar)}>
            <div className='one column' />
            <div className='ten columns'>
              <Navbar user={this.props.user} />
            </div>
            <div className='one column' />
          </div>

          <div className='row'>
            <div className='one column' />
            <div className='ten columns'>
              <div className={s.contentMargin}>
                {this.props.children}
              </div>
            </div>
            <div className='one column' />
          </div>
        </div>
      </div>
    )
  }
}

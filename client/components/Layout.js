
import React, { Component, PropTypes } from 'react'

import { Row, Columns } from './helpers'
// Pull skeleton into the CSS build
// eslint-disable-next-line no-unused-vars
import globalStyles from './globalStyles.css'
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

          <Row className={s.navbar}>
            <Columns n={1} />
            <Columns n={10}>
              <Navbar user={this.props.user} />
            </Columns>
            <Columns n={1} />
          </Row>

          <Row>
            <Columns n={1} />
            <Columns n={10}>
              <div className={s.contentMargin}>
                {this.props.children}
              </div>
            </Columns>
            <Columns n={1} />
          </Row>
        </div>
      </div>
    )
  }
}

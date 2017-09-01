
import React, { Component } from 'react'
import PropTypes from 'prop-types'

import { Row, Columns } from './helpers'
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
      <div>
        <div className='container'>

          <Row className={s.navbar}>
            <Columns n={1} />
            <Columns n={10}>
              <Navbar user={this.props.user} />
            </Columns>
            <Columns n={1} />
          </Row>

          <Row className={s.padTopBottom}>
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

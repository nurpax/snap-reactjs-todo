import React, { Component } from 'react'
import PropTypes from 'prop-types'

import s from './helpers.scss'

export class Alert extends Component {
  static propTypes = {
    type: PropTypes.oneOf(['info', 'warning', 'error']).isRequired,
    children: PropTypes.any
  }
  static css = {
    'info': s.info,
    'warning': s.warning,
    'error': s.error
  }
  render () {
    return <div className={Alert.css[this.props.type]}>{this.props.children}</div>
  }
}

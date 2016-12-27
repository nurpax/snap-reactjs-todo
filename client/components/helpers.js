import React, { PropTypes, Component } from 'react'
import classNames from 'classnames'

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

// Skeleton CSS helpers
export class Row extends Component {
  static propTypes = {
    children: PropTypes.any,
    className: PropTypes.string
  }
  render () {
    return <div className={classNames(this.props.className, 'row')}>{this.props.children}</div>
  }
}

export class Columns extends Component {
  static propTypes = {
    n: PropTypes.oneOf([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]).isRequired,
    children: PropTypes.any,
    className: PropTypes.string
  }
  static columnClass = {
    1:  'one column',
    2:  'two columns',
    3:  'three columns',
    4:  'four columns',
    5:  'five columns',
    6:  'six columns',
    7:  'seven columns',
    8:  'eight columns',
    9:  'nine columns',
    10: 'ten columns',
    11: 'eleven columns',
    12: 'twelve columns'
  }
  render () {
    return (
      <div className={classNames(this.props.className, Columns.columnClass[this.props.n])}>
        {this.props.children}
      </div>
    )
  }
}

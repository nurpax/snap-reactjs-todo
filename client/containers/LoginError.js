
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { Alert } from '../components/helpers'

export class LoginErrorView extends Component {
  static propTypes = {
    msg: PropTypes.string
  }

  static mapStateToProps (state) {
    return {
      msg: state.auth ? state.auth.status : null
    }
  }

  render () {
    let msg = this.props.msg
      ? <Alert type='error'>{this.props.msg}</Alert>
      : null
    return (
      <div>{msg}</div>
    )
  }
}

export default connect(LoginErrorView.mapStateToProps)(LoginErrorView)

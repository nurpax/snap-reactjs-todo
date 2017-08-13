
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { Alert, Row, Columns } from '../components/helpers'

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
      <Row>
        <Columns n={6}>
          {msg}
        </Columns>
      </Row>
    )
  }
}

export default connect(LoginErrorView.mapStateToProps)(LoginErrorView)

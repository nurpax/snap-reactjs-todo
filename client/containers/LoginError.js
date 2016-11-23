
import React, { Component } from 'react'
import { connect } from 'react-redux'
import { Alert } from '../components/helpers'

export class LoginErrorView extends Component {
  static propTypes = {
    msg: React.PropTypes.string
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
      <div className='row'>
        <div className='six columns'>
          {msg}
        </div>
      </div>
    )
  }
}

export default connect(LoginErrorView.mapStateToProps)(LoginErrorView)

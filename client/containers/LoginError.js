
import React, { Component } from 'react'
import { connect } from 'react-redux'

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
    let style = {
      color: 'red'
    }
    return (
      <div style={style}>{this.props.msg}</div>
    )
  }
}

export default connect(LoginErrorView.mapStateToProps)(LoginErrorView)

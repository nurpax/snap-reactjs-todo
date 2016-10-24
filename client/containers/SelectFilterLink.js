
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { setFilter } from '../actions'

class FilterLink extends Component {
  baseButtonStyle = {
    color: '#777',
    width: '44px',
    fontSize: '11px',
    padding: '4px 4px 4px 4px',
    borderRadius: '4px',
    display: 'inline-block',
    textAlign: 'center',
    textDecoration: 'none',
    textTransform: 'uppercase'
  }
  inactiveButtonStyle = {
    border: '1px solid #fff',
    ...this.baseButtonStyle
  }

  activeButtonStyle = {
    border: '1px solid #bbb',
    ...this.baseButtonStyle
  }

  static propTypes = {
    filter: React.PropTypes.string.isRequired,
    setFilter: React.PropTypes.func.isRequired,
    selectedFilter: React.PropTypes.string.isRequired
  }

  constructor (props) {
    super(props)
    this.handleClick = this.handleClick.bind(this)
  }

  handleClick (e) {
    e.preventDefault()
    this.props.setFilter(this.props.filter)
  }

  render () {
    let style = this.props.filter === this.props.selectedFilter ? this.activeButtonStyle : this.inactiveButtonStyle
    return (
      <a style={style} onClick={this.handleClick} href='#'>{this.props.filter}</a>
    )
  }
}

function mapStateToProps (state) {
  return {
    selectedFilter: state.filter
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    setFilter: filter => dispatch(setFilter(filter))
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(FilterLink)

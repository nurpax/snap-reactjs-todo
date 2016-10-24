
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { setFilter } from '../actions'

import styles from './SelectFilterLink.scss'

class FilterLink extends Component {

  static propTypes = {
    filter: PropTypes.string.isRequired,
    setFilter: PropTypes.func.isRequired,
    selectedFilter: PropTypes.string.isRequired
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
    let buttonClass = this.props.filter === this.props.selectedFilter ? styles.active : styles.inactive
    return (
      <a className={buttonClass} onClick={this.handleClick} href='#'>{this.props.filter}</a>
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

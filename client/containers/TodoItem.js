
import React, { Component } from 'react'
import PropTypes from 'prop-types'

import styles from './TodoItem.scss'

export default class TodoItem extends Component {
  static propTypes = {
    todo: PropTypes.object.isRequired,
    saveTodo: PropTypes.func.isRequired
  }

  handleChange = (e) => {
    e.preventDefault()
    let todo = this.props.todo
    this.props.saveTodo({...todo, completed: !todo.completed})
  };

  render () {
    let { completed, text, savedOn } = this.props.todo
    return (
      <li className={completed ? styles.completed : ''}>
        <label>
          <input onChange={this.handleChange} type='checkbox' checked={completed} />
          <span className='label-body'> {text} <small className={styles.date}>{savedOn}</small></span>
        </label>
      </li>
    )
  }
}

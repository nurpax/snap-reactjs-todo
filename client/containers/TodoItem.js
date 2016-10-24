
import React, { Component, PropTypes } from 'react'

export default class TodoItem extends Component {
  static propTypes = {
    todo: PropTypes.object.isRequired,
    saveTodo: PropTypes.func.isRequired
  }

  onClick = (e) => {
    e.preventDefault()
    let todo = this.props.todo
    this.props.saveTodo({...todo, completed: !todo.completed})
  };

  render () {
    let { completed, text, savedOn } = this.props.todo
    return (
      <li className={completed ? 'completed' : ''}>
        <label>
          <input onClick={this.onClick} type='checkbox' checked={completed} />
          <span className='label-body'> {text} <small className='date'>{savedOn}</small></span>
        </label>
      </li>
    )
  }
}


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
    let todo = this.props.todo
    let completedClass = todo.completed ? 'completed' : ''
    return (
      <li onClick={this.onClick} className={completedClass}>
        {todo.text} <small>{todo.savedOn}</small>
      </li>
    )
  }
}

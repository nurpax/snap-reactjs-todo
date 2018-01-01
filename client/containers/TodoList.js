
import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { getUser } from '../auth'
import { fetchTodos, saveTodo } from '../actions'
import { getSortedTodos } from '../selectors'

import s from './TodoList.scss'

import Layout from '../components/Layout'
import TodoItem from './TodoItem'
import SelectFilterLink from './SelectFilterLink'

class NewTodoForm extends Component {
  static propTypes = {
    saveTodo: PropTypes.func.isRequired
  }

  state = {
    todo: ''
  }

  constructor (props) {
    super(props)
    this.handleChange = this.handleChange.bind(this)
  }

  onClick = (e) => {
    e.preventDefault()
    this.props.saveTodo({text: this.state.todo, completed: false})
    this.setState({todo: ''})
  };

  handleChange (event) {
    this.setState({todo: event.target.value})
  }

  render () {
    return (
      <form>
        <div className={s.flex}>
          <div className={s.width3}>
            <input className='u-full-width'
              value={this.state.todo}
              onChange={this.handleChange}
              type='text' placeholder='Todo..'
              id='newTodo' />
          </div>
          <div className={s.width1}>
            <input onClick={this.onClick} className='button-primary' type='submit' value='Add' />
          </div>
        </div>
      </form>
    )
  }
}

class TodoList extends Component {
  static propTypes = {
    loadTodoList: PropTypes.func.isRequired,
    saveTodo: PropTypes.func.isRequired,
    todos: PropTypes.array.isRequired,
    user: PropTypes.object
  }

  componentDidMount () {
    this.props.loadTodoList()
  }

  render () {
    let filterChoices = ['active', 'all']
    let saveTodo = this.props.saveTodo
    let todos = this.props.todos.map(todo => <TodoItem key={todo.id} todo={todo} saveTodo={saveTodo} />)
    return (
      <Layout user={this.props.user}>
        <h2>Todos</h2>
        <p><strong>Show: </strong>
          {filterChoices.map(f => <SelectFilterLink key={f} filter={f} />)}
        </p>
        <ul className={s.todos}>
          {todos}
        </ul>
        <NewTodoForm saveTodo={saveTodo} />
      </Layout>
    )
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    loadTodoList: () => {
      dispatch(fetchTodos())
    },
    saveTodo: (todo) => {
      dispatch(saveTodo(todo))
    }
  }
}

function mapStateToProps (state) {
  return {
    user: getUser(state),
    todos: getSortedTodos(state)
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(TodoList)

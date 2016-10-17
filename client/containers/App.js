
import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'
import { fetchTodos, saveTodo } from '../actions'

import Layout from '../components/Layout'

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
        <div className='row'>
          <div className='six columns'>
            <input className='u-full-width'
              value={this.state.todo}
              onChange={this.handleChange}
              type='text' placeholder='Todo..'
              id='newTodo' />
          </div>
          <div className='six columns'>
            <input onClick={this.onClick} className='button-primary' type='submit' value='Add' />
          </div>
        </div>
      </form>
    )
  }
}

class TodoItem extends Component {
  static propTypes = {
    todo: PropTypes.object.isRequired,
    saveTodo: PropTypes.func.isRequired
  }

  constructor (props) {
    super(props)
  }

  onClick = (e) => {
    e.preventDefault()
    this.props.saveTodo({...this.props.todo, completed: true})
  };

  render () {
    let todo = this.props.todo
    let completedClass = todo.completed ? 'completed' : '';
    return (
      <li onClick={this.onClick} className={completedClass}>{todo.text} <small>{todo.savedOn}</small></li>
    )
  }
}

class App extends Component {
  static propTypes = {
    loadTodoList: PropTypes.func.isRequired,
    saveTodo: PropTypes.func.isRequired,
    todos: PropTypes.array.isRequired
  }

  componentDidMount () {
    this.props.loadTodoList()
  }

  render () {
    let saveTodo = this.props.saveTodo
    let todos = this.props.todos.map(todo => <TodoItem key={todo.id} todo={todo} saveTodo={saveTodo}/>)
    return (
      <Layout user={this.props.user}>
        <h2>Todos</h2>
        <ul>
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
    user: state.user,
    todos: state.todos
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)

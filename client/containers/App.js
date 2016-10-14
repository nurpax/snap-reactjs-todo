
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
    this.props.saveTodo({text: this.state.todo})
    e.preventDefault()
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
    let todos = this.props.todos.map(todo => <li key={todo.id}>{todo.text} <small>{todo.savedOn}</small></li>)
    return (
      <Layout user={this.props.user}>
        <h2>Todos</h2>
        <ul>
          {todos}
        </ul>
        <NewTodoForm saveTodo={this.props.saveTodo} />
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

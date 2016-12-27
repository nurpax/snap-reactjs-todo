
import * as c from './constants'
import { fetchWithAuth } from './auth'

// Todo handling
function receiveTodos (json) {
  return {
    type: c.RECEIVE_TODO_LIST,
    data: json,
    receivedAt: Date.now()
  }
}

function receiveTodo (json) {
  return {
    type: c.RECEIVE_TODO,
    data: json,
    receivedAt: Date.now()
  }
}

export function fetchTodos () {
  return function (dispatch, getState) {
    return fetchWithAuth(dispatch, getState, '/api/todo', {}, function (json) {
      dispatch(receiveTodos(json))
    })
  }
}

export function saveTodo (todo) {
  return function (dispatch, getState) {
    const url = todo.id ? `/api/todo/${todo.id}` : '/api/todo'
    return fetchWithAuth(dispatch, getState, url, { method: 'POST', body: todo },
      function (json) {
        dispatch(receiveTodo(json))
      })
  }
}

export function setFilter (filter) {
  return {
    type: c.SET_FILTER,
    data: filter
  }
}

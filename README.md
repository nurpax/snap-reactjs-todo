# React/Redux + Snap example

A todo app example built using React+Redux running on a Haskell Snap server.

Implements JWT-based authentication for REST API entry points.  The frontend uses `react-router`, `react-redux-router` and `redux-auth-wrapper` for managing authenticated routes and logged-in state.

This was not intended as a "boilerplate" project, so some features like hot-loading are missing.  But it does implement a reasonably nice webpack config that supports ES6+ usage with React, CSS modules, etc.  

## Running the app

1. Install nodejs
2. Install Haskell `stack`
3. `npm i`
4. `stack setup`, `stack build`
5. In shell A: `npm run dev` (start watching the JS client source, rebuilding on changes)
6. In shell B: `stack exec snap-reactjs-todo` (start Haskell server)
7. Navigate to http://localhost:8000

You need to create a new user in order to actually get into the todo view.  The home page route doesn't require logged-in status.

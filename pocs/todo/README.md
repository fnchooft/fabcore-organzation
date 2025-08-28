# GPT-5 : generated Todo App

```
Hi I am preparing a coding demo for the Erlang programming language. Can you get me started with a rebar3 based project? The project should have a step by step README.md file.  Assume we are on an ubuntu linux laptop with erlang installed.  The application should start up and be a todo-list using sqlite3. Since we are using Erlang there is a built-in web server and we would like to be able to use curl to add, del, and show todos.  Can you generate this for me?
```

```
GPT-5
Erlang rebar3 Todo List Demo (inets + SQLite)

Overview
This is a minimal Erlang OTP application using rebar3 that implements a simple todo list backed by SQLite. It uses Erlang’s built-in HTTP server (inets httpd with mod_esi) to provide a tiny HTTP API and a small HTML UI.

- HTTP server: inets httpd (built-in).
- Dynamic endpoints: mod_esi.
- Storage: SQLite via esqlite (Hex dependency).
- JSON: Built-in `json` module from Erlang/OTP (27+).
- Formatter: `erlfmt` via `rebar3_format` plugin.
- Endpoints:
  - `GET /todo/list`
  - `GET /todo/add?text=Buy%20milk`
  - `GET /todo/del?id=1`
- UI:
  - `GET /index.html` (served statically from `priv/`).

 
Prerequisites
 
Ubuntu Linux laptop.
Erlang/OTP installed.
rebar3 installed.
SQLite development libraries (for building esqlite NIFs).
```

Verify that you have the requirements:

```
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
rebar3 version
```
Notes and Troubleshooting
 
Inets and mod_esi:
This demo maps the prefix /todo to the ESI module todo_http, so GET /todo/add invokes todo_http:add/2, etc.
We keep responses simple and always return 200 OK with a JSON body indicating success or failure.
SQLite and esqlite:
Building esqlite requires libsqlite3-dev and a C toolchain. Ensure apt packages are installed as shown.
The database file is created under priv/todo.db. The priv directory is ensured at runtime.
Port and bind address:
Defaults are 127.0.0.1:8080. Change via application environment in src/todo.app.src.
Shell auto-start:
rebar.config includes a shell config to auto-start the todo application when running rebar3 shell.
Clean build:
If you change dependencies or encounter build issues, try:
rebar3 clean --all.
rebar3 do deps, compile.
Example curl session:
Add: curl "http://127.0.0.1:8080/todo/add?text=Read%20Erlang%20docs".
List: curl "http://127.0.0.1:8080/todo/list".
Delete: curl "http://127.0.0.1:8080/todo/del?id=1".
 
Happy hacking!

## Dependencies

```bash
sudo apt-get update
sudo apt-get install -y erlang-base erlang-dev build-essential libsqlite3-dev
```
### If you don’t have rebar3:

```bash
curl -fsSL https://github.com/erlang/rebar3/releases/latest/download/rebar3 -o rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```
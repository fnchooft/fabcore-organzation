# Generic Tree (ltree) to XML Exporter in PostgreSQL

This project demonstrates a powerful and generic method for storing 
hierarchical data in a single PostgreSQL table 
using the `ltree` extension and exporting it to a nested XML format.

The core of the project is a `plpgsql` function, `generate_xml_from_tree`, which can export any subtree from a specified starting node to a given depth. The entire environment is containerized with Docker and managed with a Makefile for easy setup and testing.

## Ai Studio supported

This example was written using [ai-studio](https://aistudio.google.com/).

Some iterations were needed, ai-studio cannot execute code yet as claude-code does
so the human-in-the-loop is still needed.

However, after several tries the result does work.

### Other notes

Asking ai-studio to generated tests is a good exercise and even in those tests
one can find subtle mistakes.

### Final words

ai-studio should be part of your programming ai-toolkit.

others which one should try are chatgpt, deepseek and claude.


## Features

* **Generic Table Structure:** A single table (`generic_tree`) can store multiple, unrelated hierarchies.
* **Powerful Export Function:** A robust procedural function converts `ltree` paths into nested XML.
* **Parameterized:** Export any part of the tree by specifying a starting node ID and maximum depth.
* **Self-Contained:** The entire environment, including PostgreSQL and all schemas, is managed by Docker.
* **Testable:** A simple `make test` command verifies that the function is working correctly.

## Prerequisites

*   Docker
*   `make`

## How to Use

### Build the Docker Image

```sh
make build
```

### Start the PostgreSQL Container

```sh
make start
```

### Run the Tests

```sh
make test
```

### Connect to the Database (Optional)

```sh
make psql
```

### Stop and Clean Up

```sh
make stop
```



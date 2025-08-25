IMAGE_NAME := pg-ltree-xml
CONTAINER_NAME := pg-ltree-xml-container
DB_PASSWORD := mysecretpassword
HOST_PORT := 8123

# --- Docker Commands ---

.PHONY: build
build:
	@echo "Building PostgreSQL Docker image..."
	@docker build -t $(IMAGE_NAME) .

.PHONY: start
start:
	@echo "Starting PostgreSQL container on localhost:$(HOST_PORT)..."
	docker run --name $(CONTAINER_NAME) \
		-e POSTGRES_PASSWORD=$(DB_PASSWORD) \
		-d -p $(HOST_PORT):5432 \
		$(IMAGE_NAME)

.PHONY: stop
stop:
	@echo "Stopping and removing PostgreSQL container..."
	docker stop $(CONTAINER_NAME) || true
	docker rm $(CONTAINER_NAME) || true

.PHONY: clean
clean: stop
	@echo "Removing Docker image..."
	docker rmi $(IMAGE_NAME) || true

.PHONY: psql
psql:
	@echo "Connecting to the database (via docker exec)..."
	docker exec -it $(CONTAINER_NAME) psql -U postgres

.PHONY: test
test:
	@echo "Running tests..."
	docker exec -i $(CONTAINER_NAME) psql -U postgres < test.sql

export_xmls:
	@echo "Running tests..."
	docker exec -i $(CONTAINER_NAME) psql -U postgres -c "SELECT generate_xml_from_tree(1, 1000);" -t -A | xmllint --format - > tree-1.xml
	docker exec -i $(CONTAINER_NAME) psql -U postgres -c "SELECT generate_xml_from_tree(10, 1000);" -t -A | xmllint --format - > tree-2.xml
	docker exec -i $(CONTAINER_NAME) psql -U postgres -c "SELECT generate_xml_from_tree(20, 1000);" -t -A | xmllint --format - > tree-3.xml



# --- Utility ---

.PHONY: help
help:
	@echo "Available commands:"
	@echo "  build    - Build the Docker image."
	@echo "  start    - Start the PostgreSQL container."
	@echo "  stop     - Stop and remove the container."
	@echo "  clean    - Stop the container and remove the image."
	@echo "  test     - Run the SQL tests against the running container."
	@echo "  psql     - Connect to the database with psql."
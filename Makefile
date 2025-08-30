.PHONY: build run clean exec

IMAGE_NAME = lean-dev
CONTAINER_NAME = lean-container

build:
	docker build -t $(IMAGE_NAME) .

run:
	docker run -it --rm --name $(CONTAINER_NAME) -v $(PWD):/workspace $(IMAGE_NAME)

exec:
	docker run --rm -v $(PWD):/workspace $(IMAGE_NAME) sh -c "cd /workspace && lean src/$(DIR)/$(FILE)"


clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true

help:
	@echo "Available commands:"
	@echo "  make build - Build the Lean Docker image"
	@echo "  make run   - Run the Lean container with current directory mounted"
	@echo "  make exec DIR=directory FILE=filename.lean - Execute a specific Lean file"
	@echo "  make clean - Remove the Docker image"
	@echo "  make help  - Show this help message"
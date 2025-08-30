.PHONY: build run clean

IMAGE_NAME = lean-dev
CONTAINER_NAME = lean-container

build:
	docker build -t $(IMAGE_NAME) .

run:
	docker run -it --rm --name $(CONTAINER_NAME) -v $(PWD):/workspace $(IMAGE_NAME)

clean:
	docker rmi $(IMAGE_NAME) 2>/dev/null || true

help:
	@echo "Available commands:"
	@echo "  make build - Build the Lean Docker image"
	@echo "  make run   - Run the Lean container with current directory mounted"
	@echo "  make clean - Remove the Docker image"
	@echo "  make help  - Show this help message"
# Variables
DOCKER_COMPOSE_FILE=docker-compose.yml
SERVICE_NAME=router-agent

# Local commands
install:
	pnpm install

build:
	pnpm run build

start:
	pnpm start

# Docker commands
docker-build:
	docker compose -f $(DOCKER_COMPOSE_FILE) build

docker-up:
	docker compose -f $(DOCKER_COMPOSE_FILE) up -d

docker-up-service:
	docker compose -f $(DOCKER_COMPOSE_FILE) up -d $(SERVICE)

docker-down:
	docker compose -f $(DOCKER_COMPOSE_FILE) down

docker-logs:
	docker compose -f $(DOCKER_COMPOSE_FILE) logs -f $(SERVICE_NAME)

.PHONY: install build start docker-build docker-up docker-down docker-logs

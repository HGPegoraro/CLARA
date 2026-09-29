#!/usr/bin/env bash
# Mac / Linux launcher. Make executable once:  chmod +x start_CLARA.sh
cd "$(dirname "$0")" || exit 1

if ! docker info >/dev/null 2>&1; then
  echo "Docker is not running. Start Docker Desktop (or the docker service) and try again."
  exit 1
fi

echo "Getting CLARA ready (the first time can take a few minutes)..."
docker compose pull >/dev/null 2>&1 || docker compose build

( sleep 6; (open http://localhost:3838 || xdg-open http://localhost:3838) >/dev/null 2>&1 ) &
docker compose up

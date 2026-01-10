#!/usr/bin/env bash
set -euo pipefail

# 一键重置本项目本地 MySQL 8.0（Docker compose + bind mount）
# 说明：
# - 会删除 ./\.data/mysql（仅影响本项目的 MySQL 数据）
# - 适合“Flyway 迁移失败/半迁移状态/重复字段”时恢复到干净库

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "[reset] docker compose down"
docker compose down

echo "[reset] removing $ROOT_DIR/.data/mysql"
rm -rf "$ROOT_DIR/.data/mysql"

echo "[reset] docker compose up -d"
docker compose up -d

echo "[reset] done"


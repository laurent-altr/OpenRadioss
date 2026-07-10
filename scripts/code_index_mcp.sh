#!/bin/sh
# Launcher for the OpenRadioss code-index MCP server (scripts/code_index_mcp.py).
#
# This wrapper exists because some MCP clients (e.g. GitHub Copilot CLI,
# github/copilot-cli#1385) rewrite commands that look like Python invocations
# into something else (pipx), breaking in-repo servers. A shell script is
# launched exactly as configured.
#
# Works from any current directory: the script locates itself.
DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec python3 "$DIR/code_index_mcp.py" "$@"

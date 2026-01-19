#!/bin/bash
# Improved Cloud Deployment Script for ClioPatria Context Server
# Usage: ./run-cloud.sh [start|stop|restart|status]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

PID_FILE="$SCRIPT_DIR/server.pid"
LOG_FILE="$SCRIPT_DIR/server.log"
PORT=3020

start_server() {
    if [ -f "$PID_FILE" ]; then
        PID=$(cat "$PID_FILE")
        if ps -p "$PID" > /dev/null 2>&1; then
            echo "Server is already running (PID: $PID)"
            return 1
        else
            echo "Removing stale PID file"
            rm -f "$PID_FILE"
        fi
    fi

    echo "Starting ClioPatria server..."
    echo "Log file: $LOG_FILE"
    echo "Port: $PORT"
    
    # Start the server in background
    # cp_server starts the HTTP server but returns, so we need thread_get_message to keep alive
    nohup swipl -g '(cp_server, thread_get_message(_))' -s run.pl > "$LOG_FILE" 2>&1 &
    SERVER_PID=$!
    
    # Save PID
    echo "$SERVER_PID" > "$PID_FILE"
    
    # Wait a bit and check if it's running
    sleep 3
    if ps -p "$SERVER_PID" > /dev/null 2>&1; then
        echo "✓ Server started successfully (PID: $SERVER_PID)"
        echo "Access at: http://localhost:$PORT/"
        return 0
    else
        echo "✗ Server failed to start. Check $LOG_FILE for errors"
        rm -f "$PID_FILE"
        return 1
    fi
}

stop_server() {
    if [ ! -f "$PID_FILE" ]; then
        echo "Server is not running (no PID file found)"
        return 1
    fi
    
    PID=$(cat "$PID_FILE")
    if ! ps -p "$PID" > /dev/null 2>&1; then
        echo "Server is not running (PID $PID not found)"
        rm -f "$PID_FILE"
        return 1
    fi
    
    echo "Stopping ClioPatria server (PID: $PID)..."
    kill "$PID"
    
    # Wait for graceful shutdown
    for i in {1..10}; do
        if ! ps -p "$PID" > /dev/null 2>&1; then
            echo "✓ Server stopped successfully"
            rm -f "$PID_FILE"
            return 0
        fi
        sleep 1
    done
    
    # Force kill if still running
    if ps -p "$PID" > /dev/null 2>&1; then
        echo "Server did not stop gracefully, forcing..."
        kill -9 "$PID"
        sleep 1
    fi
    
    rm -f "$PID_FILE"
    echo "✓ Server stopped"
    return 0
}

status_server() {
    if [ ! -f "$PID_FILE" ]; then
        echo "Server is not running (no PID file)"
        return 1
    fi
    
    PID=$(cat "$PID_FILE")
    if ps -p "$PID" > /dev/null 2>&1; then
        echo "✓ Server is running (PID: $PID)"
        echo "  Port: $PORT"
        echo "  Log: $LOG_FILE"
        echo "  URL: http://localhost:$PORT/"
        return 0
    else
        echo "✗ Server is not running (stale PID file)"
        return 1
    fi
}

case "${1:-start}" in
    start)
        start_server
        ;;
    stop)
        stop_server
        ;;
    restart)
        stop_server
        sleep 2
        start_server
        ;;
    status)
        status_server
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|status}"
        exit 1
        ;;
esac

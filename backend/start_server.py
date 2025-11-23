"""
Start the backend server without auto-reload
"""
import uvicorn

if __name__ == "__main__":
    print("[INFO] Starting BRE backend server...")
    print("[INFO] Server will run at http://0.0.0.0:8000")
    print("[INFO] Press CTRL+C to stop")

    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=False,  # Disable auto-reload to avoid Windows issues
        log_level="info"
    )

#!/usr/bin/env python3
"""
Simple HTTP server for testing the Fancy Regex Playground locally.
"""

import http.server
import socketserver
import os
import sys
import webbrowser
from pathlib import Path

class MyHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        # Add CORS headers for WASM
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        super().end_headers()

    def guess_type(self, path):
        mimetype = super().guess_type(path)
        # Ensure WASM files are served with correct MIME type
        if path.endswith('.wasm'):
            return 'application/wasm'
        return mimetype

def main():
    port = 8000
    web_dir = Path(__file__).parent / "web"
    
    if not web_dir.exists():
        print(f"Error: Web directory not found at {web_dir}")
        sys.exit(1)
    
    os.chdir(web_dir)
    
    with socketserver.TCPServer(("", port), MyHTTPRequestHandler) as httpd:
        print(f"🚀 Fancy Regex Playground server starting...")
        print(f"📂 Serving from: {web_dir}")
        print(f"🌐 Open: http://localhost:{port}")
        print(f"⏹️  Press Ctrl+C to stop")
        
        try:
            # Try to open browser automatically
            webbrowser.open(f'http://localhost:{port}')
        except:
            pass
        
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n👋 Server stopped")

if __name__ == "__main__":
    main()
#!/bin/bash
set -e

echo "🔧 Building Fancy Regex Playground..."

# Check if wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "❌ wasm-pack is not installed. Installing..."
    cargo install wasm-pack
fi

# Check if wasm32 target is installed
if ! rustup target list --installed | grep -q "wasm32-unknown-unknown"; then
    echo "🎯 Adding wasm32-unknown-unknown target..."
    rustup target add wasm32-unknown-unknown
fi

# Build the WASM module
echo "🚀 Building WASM module..."
wasm-pack build --target web --out-dir pkg --release

# Copy WASM files to web directory
echo "📦 Copying WASM files to web directory..."
cp -r pkg web/

echo "✅ Build complete!"
echo ""
echo "To run locally:"
echo "  python3 serve.py"
echo ""
echo "Then open: http://localhost:8000"
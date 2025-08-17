# Fancy Regex Playground

A browser-based interactive playground for testing and exploring the [fancy-regex](https://github.com/fancy-regex/fancy-regex) crate. This playground is compiled to WebAssembly (WASM) and provides a user-friendly interface for experimenting with advanced regex features like backreferences and lookaround.

## ✨ Features

- **Interactive Regex Testing**: Enter regex patterns and test text to see real-time results
- **Advanced Regex Support**: Full support for fancy-regex features including:
  - Backreferences (`\1`, `\2`, etc.)
  - Named capture groups (`(?<name>...)` and `\k<name>`)
  - Lookahead and lookbehind assertions
  - All other fancy-regex features
- **Real-time Highlighting**: Visual highlighting of matches and capture groups in the test text
- **Regex Flags**: Support for case-insensitive, multi-line, dot-matches-newline, and ignore-whitespace flags
- **Parse Tree Visualization**: View the internal parse tree structure of your regex
- **Analysis Output**: See detailed analysis information about your regex pattern
- **Error Handling**: Clear error messages for invalid patterns or runtime errors
- **Debounced Updates**: Smooth real-time updates with debouncing to prevent excessive computation
- **Responsive Design**: Works well on desktop and mobile devices

## 🚀 Quick Start

### Running Locally

1. **Prerequisites**: 
   - Rust (latest stable)
   - `wasm-pack` - Install with: `cargo install wasm-pack`
   - Python 3 (for local server)

2. **Clone and build**:
   ```bash
   git clone https://github.com/fancy-regex/fancy-regex.git
   cd fancy-regex/playground
   
   # Build the WASM module
   wasm-pack build --target web --out-dir pkg
   
   # Copy WASM files to web directory
   cp -r pkg web/
   
   # Start local server
   python3 serve.py
   ```

3. **Open in browser**: Visit `http://localhost:8000`

### Building for Production

To build optimized WASM for production deployment:

```bash
cd playground
wasm-pack build --target web --out-dir pkg --release
cp -r pkg web/
```

## 📦 Publishing to GitHub Pages

### Automatic Deployment (Recommended)

1. **Create a GitHub Actions workflow** (`.github/workflows/deploy-playground.yml`):

```yaml
name: Deploy Playground to GitHub Pages

on:
  push:
    branches: [ main ]
    paths: [ 'playground/**' ]
  workflow_dispatch:

permissions:
  contents: read
  pages: write
  id-token: write

concurrency:
  group: "pages"
  cancel-in-progress: false

jobs:
  build-and-deploy:
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v4
        
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          target: wasm32-unknown-unknown
          
      - name: Install wasm-pack
        run: cargo install wasm-pack
        
      - name: Build WASM
        run: |
          cd playground
          wasm-pack build --target web --out-dir pkg --release
          cp -r pkg web/
          
      - name: Setup Pages
        uses: actions/configure-pages@v4
        
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: './playground/web'
          
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

2. **Enable GitHub Pages**:
   - Go to your repository settings
   - Navigate to "Pages" section
   - Set source to "GitHub Actions"
   - The playground will be available at `https://yourusername.github.io/fancy-regex/`

### Manual Deployment

If you prefer manual deployment:

1. **Build the playground**:
   ```bash
   cd playground
   wasm-pack build --target web --out-dir pkg --release
   cp -r pkg web/
   ```

2. **Create `gh-pages` branch**:
   ```bash
   git checkout --orphan gh-pages
   git rm -rf .
   cp -r playground/web/* .
   git add .
   git commit -m "Deploy playground to GitHub Pages"
   git push origin gh-pages
   ```

3. **Configure GitHub Pages** to use the `gh-pages` branch

## 🎯 Usage Examples

### Basic Regex Testing
- Pattern: `hello`
- Text: `hello world hello there`
- Result: Highlights both "hello" occurrences

### Backreferences
- Pattern: `(\w+)\s+\1`
- Text: `hello hello world test test`
- Result: Matches repeated words like "hello hello" and "test test"

### Named Capture Groups
- Pattern: `(?<word>\w+)\s+\k<word>`
- Text: `apple apple orange banana banana`
- Result: Matches repeated words using named backreferences

### Lookahead Assertions
- Pattern: `\w+(?=\s+world)`
- Text: `hello world goodbye moon`
- Result: Matches "hello" (word followed by " world")

## 🏗️ Architecture

The playground consists of:

- **WASM Module** (`playground/src/lib.rs`): Rust code compiled to WASM that exposes fancy-regex functionality
- **Frontend** (`playground/web/`): HTML/CSS/JavaScript interface for user interaction
- **Server Script** (`playground/serve.py`): Simple Python HTTP server for local development

### Key WASM Functions

- `find_matches(pattern, text, flags)`: Find all matches in text
- `find_captures(pattern, text, flags)`: Find matches with capture groups  
- `parse_regex(pattern, flags)`: Parse regex and return AST with flag consideration
- `analyze_regex(pattern, flags)`: Analyze regex and return detailed information with flag consideration
- `is_match(pattern, text, flags)`: Test if pattern matches text

## 🛠️ Development

### Project Structure
```
playground/
├── Cargo.toml          # WASM crate configuration
├── src/
│   └── lib.rs          # WASM wrapper implementation
├── web/
│   ├── index.html      # Main playground interface
│   ├── app.js          # JavaScript application logic
│   └── pkg/            # Generated WASM files (copied from ../pkg/)
├── pkg/                # Generated WASM output
└── serve.py            # Local development server
```

### Adding New Features

1. **Add WASM function** in `src/lib.rs`:
   ```rust
   #[wasm_bindgen]
   pub fn your_function(pattern: &str) -> Result<String, JsValue> {
       // Implementation
   }
   ```

2. **Update frontend** in `web/app.js`:
   ```javascript
   import { your_function } from '../pkg/fancy_regex_playground.js';
   
   // Use the function in your UI logic
   ```

3. **Rebuild**:
   ```bash
   wasm-pack build --target web --out-dir pkg
   cp -r pkg web/
   ```

### Testing

The playground includes error handling for:
- Invalid regex patterns
- Compilation errors
- Runtime errors during matching
- WASM module loading issues

Test with various patterns to ensure robust error handling.

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project follows the same license as the fancy-regex crate (MIT License).

## 🙏 Acknowledgments

- Built on top of the excellent [fancy-regex](https://github.com/fancy-regex/fancy-regex) crate
- Uses [wasm-bindgen](https://github.com/rustwasm/wasm-bindgen) for Rust/JavaScript interop
- Inspired by various online regex testing tools

---

**Note**: This playground demonstrates the capabilities of the fancy-regex crate. For production use, always validate and sanitize user input appropriately.
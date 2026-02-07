import init, {
    find_captures,
    parse_regex,
    analyze_regex,
    analyze_regex_tree,
    is_match
} from './pkg/fancy_regex_playground.js';

class FancyRegexPlayground {
    constructor() {
        this.isInitialized = false;
        this.debounceTimer = null;
        this.elements = {};
        this.lastResults = null;
        this.expandedNodes = new Set(); // Track which node paths are expanded
    }

    async init() {
        // Initialize the WASM module
        await init();
        this.isInitialized = true;
        
        // Get DOM elements
        this.elements = {
            regexInput: document.getElementById('regex-input'),
            textInput: document.getElementById('text-input'),
            matchResults: document.getElementById('match-results'),
            highlightedText: document.getElementById('highlighted-text'),
            parseTreeSection: document.getElementById('parse-tree-section'),
            parseTreeDisplay: document.getElementById('parse-tree-display'),
            analysisSection: document.getElementById('analysis-section'),
            analysisDisplay: document.getElementById('analysis-display'),
            showParseTreeBtn: document.getElementById('show-parse-tree'),
            showAnalysisBtn: document.getElementById('show-analysis'),
            flags: {
                caseInsensitive: document.getElementById('flag-case-insensitive'),
                multiLine: document.getElementById('flag-multi-line'),
                dotMatchesNewline: document.getElementById('flag-dot-matches-newline'),
                ignoreWhitespace: document.getElementById('flag-ignore-whitespace'),
                onigurumaMode: document.getElementById('flag-oniguruma-mode'),
            }
        };

        this.setupEventListeners();
        this.loadExampleData();
    }

    setupEventListeners() {
        // Debounced input handlers
        this.elements.regexInput.addEventListener('input', () => this.debounceUpdate());
        this.elements.textInput.addEventListener('input', () => this.debounceUpdate());
        
        // Flag change handlers
        Object.values(this.elements.flags).forEach(flag => {
            flag.addEventListener('change', () => this.debounceUpdate());
        });

        // Toggle button handlers
        this.elements.showParseTreeBtn.addEventListener('click', () => this.toggleParseTree());
        this.elements.showAnalysisBtn.addEventListener('click', () => this.toggleAnalysis());
    }

    debounceUpdate() {
        clearTimeout(this.debounceTimer);
        const delayMs = 300;
        this.debounceTimer = setTimeout(() => this.updateResults(), delayMs);
    }

    getFlags() {
        return {
            case_insensitive: this.elements.flags.caseInsensitive.checked,
            multi_line: this.elements.flags.multiLine.checked,
            dot_matches_new_line: this.elements.flags.dotMatchesNewline.checked,
            ignore_whitespace: this.elements.flags.ignoreWhitespace.checked,
            unicode: true,
            oniguruma_mode: this.elements.flags.onigurumaMode.checked,
        };
    }

    async updateResults() {
        if (!this.isInitialized) return;

        const pattern = this.elements.regexInput.value.trim();
        const text = this.elements.textInput.value;

        if (!pattern) {
            this.clearResults();
            return;
        }

        try {
            this.setLoading(true);
            const flags = this.getFlags();

            this.updateParseTreeIfVisible(pattern, flags);
            this.updateAnalysisIfVisible(pattern, flags);

            // Test if pattern is valid
            const isValid = await this.testRegexValidity(pattern, flags);
            if (!isValid) return;

            // Find captures (which includes matches as full_match)
            const captures = find_captures(pattern, text, flags);
            
            // Extract matches from capture results
            const matches = captures.map(capture => capture.full_match).filter(match => match !== null);
            
            this.displayResults(matches, captures, text);

        } catch (error) {
            this.displayError(error.toString());
        } finally {
            this.setLoading(false);
        }
    }

    async testRegexValidity(pattern, flags) {
        try {
            // Try to parse the regex first
            parse_regex(pattern, flags);
            return true;
        } catch (error) {
            this.displayError(error.toString());
            return false;
        }
    }

    displayResults(matches, captures, originalText) {
        // Display match results
        if (matches.length === 0) {
            this.elements.matchResults.innerHTML = '<div class="info">No matches found</div>';
            this.elements.highlightedText.innerHTML = '<div class="info">No matches to highlight</div>';
            return;
        }

        // Format match results
        let resultHtml = '<div class="success">Found ' + matches.length + ' match(es):</div><br>';
        
        if (captures.length > 0) {
            resultHtml += '<strong>Captures:</strong><br>';
            captures.forEach((captureSet, setIndex) => {
                resultHtml += `<strong>Match ${setIndex + 1}:</strong><br>`;
                captureSet.captures.forEach((capture, index) => {
                    let groupDisplay = `Group ${index}`;
                    if (capture.name)
                        groupDisplay += ` / ${capture.name}`;
                    let captureResult;
                    if (capture.text !== null)
                        captureResult = `"${this.escapeHtml(capture.text)}" (${capture.start}-${capture.end})`;
                    else
                        captureResult = '<em>not captured</em>';
                    resultHtml += `  ${groupDisplay}: ${captureResult}<br>`;
                });
                resultHtml += '<br>';
            });
        } else {
            resultHtml += '<strong>Simple matches:</strong><br>';
            matches.forEach((match, index) => {
                resultHtml += `${index + 1}: "${this.escapeHtml(match.text)}" (${match.start}-${match.end})<br>`;
            });
        }

        this.elements.matchResults.innerHTML = resultHtml;

        // Highlight matches in text
        this.highlightMatches(originalText, matches);
    }

    highlightMatches(text, matches) {
        if (matches.length === 0) {
            this.elements.highlightedText.textContent = text;
            return;
        }

        // Sort matches by start position (descending) to avoid position shifts during insertion
        const sortedMatches = [...matches].sort((a, b) => b.start - a.start);
        
        const originalText = new Utf8String(text);
        let highlightedTextChunks = [];
        let latestOffset = originalText.buffer.length;
        
        sortedMatches.forEach((match, index) => {
            const matchText = originalText.substr(match.start, match.end);
            highlightedTextChunks.push(originalText.substr(match.end, latestOffset));
            highlightedTextChunks.push(
                /*html*/`<span class="match-highlight" title="Match ${sortedMatches.length - index}: ${match.start}-${match.end}">${this.escapeHtml(matchText)}</span>`
            );
            latestOffset = match.start;
        });
        highlightedTextChunks.push(originalText.substr(0, latestOffset));
        const highlightedText = highlightedTextChunks.reverse().join('');

        this.elements.highlightedText.innerHTML = highlightedText;
    }

    toggleParseTree() {
        const isVisible = !this.elements.parseTreeSection.classList.contains('hidden');
        
        if (isVisible) {
            this.elements.parseTreeSection.classList.add('hidden');
            this.elements.showParseTreeBtn.classList.remove('active');
            this.elements.showParseTreeBtn.textContent = 'Show Parse Tree';
        } else {
            this.elements.parseTreeSection.classList.remove('hidden');
            this.elements.showParseTreeBtn.classList.add('active');
            this.elements.showParseTreeBtn.textContent = 'Hide Parse Tree';
            this.updateParseTree();
        }
    }

    toggleAnalysis() {
        const isVisible = !this.elements.analysisSection.classList.contains('hidden');
        
        if (isVisible) {
            this.elements.analysisSection.classList.add('hidden');
            this.elements.showAnalysisBtn.classList.remove('active');
            this.elements.showAnalysisBtn.textContent = 'Show Analysis';
        } else {
            this.elements.analysisSection.classList.remove('hidden');
            this.elements.showAnalysisBtn.classList.add('active');
            this.elements.showAnalysisBtn.textContent = 'Hide Analysis';
            this.updateAnalysis();
        }
    }

    updateParseTreeIfVisible(pattern, flags) {
        if (!this.elements.parseTreeSection.classList.contains('hidden')) {
            this.updateParseTree(pattern, flags);
        }
    }

    updateAnalysisIfVisible(pattern, flags) {
        if (!this.elements.analysisSection.classList.contains('hidden')) {
            this.updateAnalysis(pattern, flags);
        }
    }

    updateParseTree(pattern = null, flags = null) {
        if (pattern === null) {
            pattern = this.elements.regexInput.value.trim();
        }
        if (flags === null) {
            flags = this.getFlags();
        }

        try {
            const parseTree = parse_regex(pattern, flags);
            this.elements.parseTreeDisplay.textContent = parseTree;
        } catch (error) {
            this.elements.parseTreeDisplay.textContent = error.toString();
        }
    }

    updateAnalysis(pattern = null, flags = null) {
        if (pattern === null) {
            pattern = this.elements.regexInput.value.trim();
        }
        if (flags === null) {
            flags = this.getFlags();
        }

        try {
            const analysisTree = analyze_regex_tree(pattern, flags);
            this.renderAnalysisTree(analysisTree);
        } catch (error) {
            this.elements.analysisDisplay.innerHTML = `<div class="error">${this.escapeHtml(error.toString())}</div>`;
        }
    }

    renderAnalysisTree(tree) {
        // Clear and build the tree view
        this.elements.analysisDisplay.innerHTML = '';
        
        // Create header
        const header = document.createElement('div');
        header.className = 'analysis-grid__header';
        header.innerHTML = `
            <div class="analysis-row__node">Node</div>
            <div class="analysis-row__min-size">min_size</div>
            <div class="analysis-row__const-size">const_size</div>
        `;
        this.elements.analysisDisplay.appendChild(header);
        
        // Create tree container
        const treeContainer = document.createElement('div');
        treeContainer.className = 'analysis-tree';
        this.elements.analysisDisplay.appendChild(treeContainer);
        
        // Initialize all nodes as expanded
        this.expandedNodes = new Set();
        this.collectAllPaths(tree, '0', this.expandedNodes);
        
        // Render the tree
        this.renderTreeNode(tree, treeContainer, 0, '0');
    }

    collectAllPaths(node, path, pathSet) {
        if (node.children && node.children.length > 0) {
            pathSet.add(path);
            node.children.forEach((child, index) => {
                const childPath = `${path}-${index}`;
                this.collectAllPaths(child, childPath, pathSet);
            });
        }
    }

    renderTreeNode(node, container, depth, path) {
        // Create row
        const row = document.createElement('div');
        row.className = `analysis-row ${node.hard ? 'analysis-node--hard' : 'analysis-node--easy'}`;
        if (node.children.length > 0 && !this.expandedNodes.has(path)) {
            row.classList.add('analysis-node--collapsed');
        }
        
        // Create node cell with indentation
        const nodeCell = document.createElement('div');
        nodeCell.className = 'analysis-row__node';
        
        const indent = document.createElement('span');
        indent.className = 'analysis-node__indent';
        indent.style.paddingLeft = `${depth * 1.5}rem`;
        
        // Add toggle if has children
        if (node.children.length > 0) {
            const toggle = document.createElement('span');
            toggle.className = 'analysis-node__toggle';
            toggle.textContent = this.expandedNodes.has(path) ? '−' : '+';
            toggle.onclick = (e) => {
                e.stopPropagation();
                this.toggleNode(path);
            };
            indent.appendChild(toggle);
        } else {
            // Placeholder for alignment
            const placeholder = document.createElement('span');
            placeholder.className = 'analysis-node__toggle analysis-node__toggle--placeholder';
            placeholder.textContent = ' ';
            indent.appendChild(placeholder);
        }
        
        // Add node label
        const label = document.createElement('span');
        label.className = 'analysis-node__label';
        label.textContent = `${node.kind}${node.summary ? ' ' + node.summary : ''}`;
        indent.appendChild(label);
        
        nodeCell.appendChild(indent);
        row.appendChild(nodeCell);
        
        // Add min_size cell
        const minSizeCell = document.createElement('div');
        minSizeCell.className = 'analysis-row__min-size';
        minSizeCell.textContent = node.min_size.toString();
        row.appendChild(minSizeCell);
        
        // Add const_size cell
        const constSizeCell = document.createElement('div');
        constSizeCell.className = 'analysis-row__const-size';
        if (node.const_size) {
            constSizeCell.className += ' const-size-tick';
            constSizeCell.textContent = '✓';
        } else {
            constSizeCell.className += ' const-size-cross';
            constSizeCell.textContent = '✗';
        }
        row.appendChild(constSizeCell);
        
        container.appendChild(row);
        
        // Render children if expanded
        if (this.expandedNodes.has(path) && node.children.length > 0) {
            node.children.forEach((child, index) => {
                const childPath = `${path}-${index}`;
                this.renderTreeNode(child, container, depth + 1, childPath);
            });
        }
    }

    toggleNode(path) {
        if (this.expandedNodes.has(path)) {
            this.expandedNodes.delete(path);
        } else {
            this.expandedNodes.add(path);
        }
        // Re-render the analysis
        this.updateAnalysis();
    }

    displayError(message) {
        const errorHtml = `<div class="error">${this.escapeHtml(message)}</div>`;
        this.elements.matchResults.innerHTML = errorHtml;
        this.elements.highlightedText.innerHTML = '<div class="info">Fix the pattern error to see highlights</div>';
    }

    clearResults() {
        this.elements.matchResults.innerHTML = '<div class="info">Enter a regex pattern and test text to see results</div>';
        this.elements.highlightedText.innerHTML = '<div class="info">Matches will be highlighted here</div>';
    }

    setLoading(loading) {
        if (loading) {
            document.body.classList.add('loading');
        } else {
            document.body.classList.remove('loading');
        }
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    loadExampleData() {
        // Load a sample regex and text for demonstration
        this.elements.regexInput.value = '(?<word>\\w+)\\s+\\k<word>';
        this.elements.textInput.value = `This is a test test with some some repeated words.
Another line line with more more examples.
Single words here.
Here are some Greek letters and an emoji: δ Δ 🎯
And and final test test case.`;
        
        // Trigger initial update
        setTimeout(() => this.updateResults(), 100);
    }
}

// Initialize the playground when the page loads
const playground = new FancyRegexPlayground();
playground.init().then(() => {
    console.log('fancy-regex playground initialized successfully!');
}).catch(error => {
    console.error('Failed to initialize playground:', error);
    document.body.innerHTML = `
        <div style="padding: 2rem; text-align: center; color: #e74c3c;">
            <h1>Failed to load the fancy-regex playground</h1>
            <p>Error: ${error.message}</p>
            <p>Please check the browser console for more details.</p>
        </div>
    `;
});

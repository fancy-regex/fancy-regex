class Utf8String {
  constructor(str) {
    this.str = str;
    this.encoder = new TextEncoder();
    this.decoder = new TextDecoder("utf-8");
    this.utf8 = this.encoder.encode(str);
  }

  /**
   * Get a single substring by byte offsets
   * @param {number} start - UTF-8 byte start
   * @param {number} end - UTF-8 byte end
   * @returns {string}
   */
  substr(start, end) {
    return this.decoder.decode(this.utf8.subarray(start, end));
  }

  /**
   * Get multiple substrings at once
   * @param {[number, number][]} ranges - Array of [start, end] byte ranges
   * @returns {string[]}
   */
  substrRanges(ranges) {
    return ranges.map(([start, end]) =>
      this.decoder.decode(this.utf8.subarray(start, end))
    );
  }

  /**
   * Expose underlying UTF-8 buffer (if needed for WASM interop)
   */
  get buffer() {
    return this.utf8;
  }
}

/*
// Example usage:
const s = new Utf8String("Hello 🌍, こんにちは");
console.log(s.substr(0, 5)); // "Hello"

// Multiple ranges
const ranges = [
  [0, 5],          // "Hello"
  [6, 10],         // " 🌍"
  [12, 28],        // "こんにちは"
];

console.log(s.substrRanges(ranges)); 
// → ["Hello", " 🌍", "こんにちは"]
*/

/** @type {import('tailwindcss').Config} */
const { spawnSync } = require("child_process")

module.exports = {
  content: {
    files: [ "../_build/default/example/*.pp.ml" ],
    transform: str => {
      const proc = spawnSync("strings", [], {
        stdio: "pipe",
        encoding: "utf-8",
        input: str
      })
      return proc.output.toString()
    },
    extract: str => {
      const matches = str.match(/.*tw:(.*)$/)
      if (matches === null) {
        return []
      } else {
        return [ matches[1] ]
      }
    }
  },
  theme: {
    extend: {},
  },
  plugins: [],
}


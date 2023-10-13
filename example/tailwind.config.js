/** @type {import('tailwindcss').Config} */
module.exports = {
  content: {
    files: [ "./tw_classes" ],
    extract: {
      tw_classes: (str) => str.split(/\r?\n/)
    }
  },
  theme: {
    extend: {},
  },
  plugins: [],
}


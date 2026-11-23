import { defineConfig } from 'vite'

const scalajsMode = process.env.SCALAJS_MODE || "fast";

export default defineConfig({
  root: 'frontend',
  build: {
    outDir: process.env.VITE_OUT_DIR || 'dist',
    emptyOutDir: true,
    // the page awaits its engraver at the top level of a module
    target: 'esnext',
  },
  server: {
    port: 5174,
    proxy: { '/api': { target: 'http://localhost:4322' } },
  },
  resolve: {
    alias: [
      {
        find: /^scalajs:(.*)$/,
        replacement: `/../out/frontend/${scalajsMode}LinkJS.dest/$1`
      }
    ]
  }
})

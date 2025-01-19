import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  server: {
    open: true
  },
  plugins: [elmPlugin()]
})

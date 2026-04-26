import { defineConfig } from 'vite';
import solid from 'vite-plugin-solid';

export default defineConfig({
  plugins: [solid()],
  build: {
    outDir: '../priv/static',
    emptyOutDir: true
  },
  server: {
    proxy: {
      '/api': 'http://10.0.0.2:8002'
    }
  }
});
